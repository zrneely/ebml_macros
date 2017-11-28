
use std::str::{self, FromStr};

use chrono::{Duration, NaiveDate, NaiveDateTime, NaiveTime};
use ebml::Id;
use nom::{AsChar, ErrorKind, IResult, Needed, is_digit, is_hex_digit};

use {BinaryRange, BinaryRangeItem, Cardinality, DateRange, DateRangeItem, FloatRange,
     FloatRangeItem, Header, HeaderStatement, IntRange, IntRangeItem, Level, NewType, Property,
     SizeList, StringRange, StringRangeItem, Type, UintRange, UintRangeItem};

const NANOS_PER_SEC: f64 = 1_000_000_000f64;

fn from_hex(s: &str) -> Option<Vec<u8>> {
    let mut b = Vec::with_capacity(s.len() / 2);
    let mut modulus = 0;
    let mut buf = 0;

    for (idx, byte) in s.bytes().enumerate() {
        buf <<= 4;

        match byte {
            b'A'...b'F' => buf |= byte - b'A' + 10,
            b'a'...b'f' => buf |= byte - b'a' + 10,
            b'0'...b'9' => buf |= byte - b'0',
            b' '|b'\r'|b'\n'|b'\t' => {
                buf >>= 4;
                continue
            }
            _ => return None
        }

        modulus += 1;
        if modulus == 2 {
            modulus = 0;
            b.push(buf);
        }
    }

    if modulus == 0 {
        Some(b)
    } else {
        None
    }
}

named!(lcomment<&str>, map_res!(
    preceded!(
        tag!("//"),
        take_until_and_consume!("\n")
    ),
    str::from_utf8
));

named!(bcomment<&str>, map_res!(
    preceded!(
        tag!("/*"),
        take_until_and_consume!("*/")
    ),
    str::from_utf8
));

named!(comment<&str>, alt!(lcomment | bcomment));

named!(separator<()>, ws!(
    fold_many0!(
        ws!(comment),
        (),
        // We don't care about any of the values
        |_: (), _| ()
    )
));

// Sadly handwritten name parser.
fn name(input: &[u8]) -> IResult<&[u8], &str> {
    let len = input.len();
    if len == 0 {
        IResult::Incomplete(Needed::Size(1))
    } else {
        // The first character must be alpha or underscore
        let zeroth = input[0] as char;
        if !zeroth.is_alpha() && zeroth != '_' {
            IResult::Error(error_position!(ErrorKind::AlphaNumeric, input))
        } else {
            for (idx, item) in input[1..].iter().enumerate() {
                if !item.is_alphanum() && item.as_char() != '_' {
                    return IResult::Done(
                        &input[idx + 1..],
                        str::from_utf8(&input[0..idx + 1]).unwrap()
                    )
                }
            }
            IResult::Done(&input[len..], str::from_utf8(&input[..]).unwrap())
        }
    }
}

named!(id<Id>, map_opt!(
    map_res!(
        map_res!(take_while!(is_hex_digit), str::from_utf8),
        |str_val| u32::from_str_radix(str_val, 16)
    ),
    Id::from_encoded
));

named!(type_<Type>, alt_complete!(
    map!(tag!("int"), |_| Type::Int) |
    map!(tag!("uint"), |_| Type::Uint) |
    map!(tag!("float"), |_| Type::Float) |
    map!(tag!("string"), |_| Type::String) |
    map!(tag!("date"), |_| Type::Date) |
    map!(tag!("binary"), |_| Type::Binary) |
    map!(tag!("container"), |_| Type::Container) |
    map!(name, |n| Type::Name(n))
));

named!(parent<Vec<&str>>, delimited!(
    tuple!(tag!("parent"), separator, tag!(":"), separator),
    parents,
    pair!(separator, tag!(";"))
));

named!(parents<Vec<&str>>, separated_nonempty_list_complete!(
    delimited!(separator, tag!(","), separator),
    name
));

named!(level<Level>, do_parse!(
    tag!("level") >> separator >> tag!(":") >> separator >>
    start: map_res!(
        map_res!(take_while!(is_digit), str::from_utf8),
        FromStr::from_str
    ) >>
    tag!("..") >>
    end: opt!(
        map_res!(
            map_res!(take_while!(is_digit), str::from_utf8),
            FromStr::from_str
        )
    ) >>
    separator >> tag!(";") >>

    (if let Some(end) = end {
        Level::Bounded { start, end }
    } else {
        Level::Open { start }
    })
));

named!(cardinality<Cardinality>, delimited!(
    tuple!(tag!("card"), separator, tag!(":"), separator),
    alt_complete!(
        map!(tag!("*"), |_| Cardinality::ZeroOrMany) |
        map!(tag!("?"), |_| Cardinality::ZeroOrOne) |
        map!(tag!("1"), |_| Cardinality::ExactlyOne) |
        map!(tag!("+"), |_| Cardinality::OneOrMany)
    ),
    pair!(separator, tag!(";"))
));

named!(int_v<i64>, map_res!(
    map_res!(
        take_while!(|x| is_digit(x) || x == b'-'),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(float_v<f64>, map_res!(
    map_res!(
        take_while!(|x| is_digit(x) || x == b'-' || x == b'+' || x == b'.' || x == b'e'),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(date_v<NaiveDateTime>, alt_complete!(
    do_parse!(
        year: map_res!(
            map_res!(take!(4), str::from_utf8),
            FromStr::from_str
        ) >>
        month: map_res!(
            map_res!(take!(2), str::from_utf8),
            FromStr::from_str
        ) >>
        day: map_res!(
            map_res!(take!(2), str::from_utf8),
            FromStr::from_str
        ) >>
        tag!("T") >>
        hour: map_res!(
            map_res!(take!(2), str::from_utf8),
            FromStr::from_str
        ) >>
        tag!(":") >>
        minute: map_res!(
            map_res!(take!(2), str::from_utf8),
            FromStr::from_str
        ) >>
        tag!(":") >>
        second: map_res!(
            map_res!(take!(2), str::from_utf8),
            FromStr::from_str
        ) >>
        fractional: opt!(
            map_res!(
                map_res!(
                    // Use recognize here to discard the pair itself, giving the input slice
                    // containing the dot back.
                    recognize!(
                        pair!(
                            tag!("."),
                            take_while!(is_digit)
                        )
                    ),
                    str::from_utf8
                ),
                <f64 as FromStr>::from_str
            )
        ) >>
        time: map_opt!(value!(()),
            |_| if let Some(part) = fractional {
                NaiveTime::from_hms_nano_opt(hour, minute, second, (part * NANOS_PER_SEC) as u32)
            } else {
                NaiveTime::from_hms_opt(hour, minute, second)
            }
        ) >>
        date: map_opt!(value!(()), |_| NaiveDate::from_ymd_opt(year, month, day)) >>
        (NaiveDateTime::new(date, time))
    ) |
    map!(int_v, |val| {
        // Numerical values are nanoseconds since the millennium
        let epoch = NaiveDateTime::new(
            NaiveDate::from_ymd(2001, 1, 1),
            NaiveTime::from_hms(0, 0, 0)
        );
        epoch + Duration::nanoseconds(val)
    })
));

// Not part of the spec, but helpful for implementing the string_def and binary_def things.
// This creates owned data (copies the input) since it must transform any input hex data.
named!(binary_v<Vec<u8>>, alt_complete!(
    preceded!(
        tag!("0x"),
        map_opt!(
            map_res!(take_while!(is_hex_digit), str::from_utf8),
            from_hex
        )
    ) |
    map!(
        delimited!(
            tag!("\""),
            recognize!(take_until!("\"")),
            tag!("\"")
        ),
        |slice| slice.to_vec()
    )
));


named!(int_def<Property>, delimited!(
    tuple!(tag!("def"), separator, tag!(":"), separator),
    map!(int_v, Property::IntDefault),
    pair!(separator, tag!(";"))
));

named!(uint_def<Property>, delimited!(
    tuple!(tag!("def"), separator, tag!(":"), separator),
    map!(
        map_res!(
            map_res!(take_while!(is_digit), str::from_utf8),
            FromStr::from_str
        ),
        Property::UintDefault
    ),
    pair!(separator, tag!(";"))
));

named!(float_def<Property>, delimited!(
    tuple!(tag!("def"), separator, tag!(":"), separator),
    map!(float_v, Property::FloatDefault),
    pair!(separator, tag!(";"))
));

named!(date_def<Property>, delimited!(
    tuple!(tag!("def"), separator, tag!(":"), separator),
    map!(date_v, Property::DateDefault),
    pair!(separator, tag!(";"))
));

named!(string_def<Property>, delimited!(
    tuple!(tag!("def"), separator, tag!(":"), separator),
    map!(map_res!(binary_v, String::from_utf8), Property::StringDefault),
    pair!(separator, tag!(";"))
));

named!(binary_def<Property>, delimited!(
    tuple!(tag!("def"), separator, tag!(":"), separator),
    map!(binary_v, Property::BinaryDefault),
    pair!(separator, tag!(";"))
));

named!(int_range<Property>, delimited!(
    tuple!(tag!("range"), separator, tag!(":"), separator),
    map!(
        separated_nonempty_list_complete!(
            delimited!(separator, tag!(","), separator),
            alt_complete!(
                do_parse!(
                    start: int_v >>
                    tag!("..") >>
                    end: int_v >>
                    (IntRangeItem::Bounded { start, end })
                ) |
                map!(
                    terminated!(
                        int_v,
                        tag!("..")
                    ),
                    |start| IntRangeItem::From { start }
                ) |
                map!(
                    preceded!(
                        tag!(".."),
                        int_v
                    ),
                    |end| IntRangeItem::To { end }
                ) |
                map!(int_v, IntRangeItem::Single)
            )
        ),
        Property::IntRange
    ),
    pair!(separator, tag!(";"))
));

named!(uint_range<Property>, delimited!(
    tuple!(tag!("range"), separator, tag!(":"), separator),
    map!(
        separated_nonempty_list_complete!(
            delimited!(separator, tag!(","), separator),
            alt_complete!(
                do_parse!(
                    start: map_res!(
                        map_res!(take_while!(is_digit), str::from_utf8),
                        FromStr::from_str
                    ) >>
                    tag!("..") >>
                    end: map_res!(
                        map_res!(take_while!(is_digit), str::from_utf8),
                        FromStr::from_str
                    ) >>
                    (UintRangeItem::Bounded { start, end })
                ) |
                map!(
                    terminated!(
                        map_res!(
                            map_res!(take_while!(is_digit), str::from_utf8),
                            FromStr::from_str
                        ),
                        tag!("..")
                    ),
                    |start| UintRangeItem::From { start }
                ) |
                map!(
                    map_res!(
                        map_res!(take_while!(is_digit), str::from_utf8),
                        FromStr::from_str
                    ),
                    UintRangeItem::Single
                )
            )
        ),
        Property::UintRange
    ),
    pair!(separator, tag!(";"))
));

named!(float_range<Property>, delimited!(
    tuple!(tag!("range"), separator, tag!(":"), separator),
    map!(
        separated_nonempty_list_complete!(
            delimited!(separator, tag!(","), separator),
            alt_complete!(
                do_parse!(
                    start: float_v >>
                    tag!("<") >>
                    include_start: map!(opt!(tag!("=")), |x| x.is_some()) >>
                    tag!("..") >>
                    tag!("<") >>
                    include_end: map!(opt!(tag!("=")), |x| x.is_some()) >>
                    end: float_v >>
                    (FloatRangeItem::Bounded { start, include_start, end, include_end })
                ) |
                do_parse!(
                    tag!("<") >>
                    include_end: map!(opt!(tag!("=")), |x| x.is_some()) >>
                    end: float_v >>
                    (FloatRangeItem::To { end, include_end })
                ) |
                do_parse!(
                    tag!(">") >>
                    include_start: map!(opt!(tag!("=")), |x| x.is_some()) >>
                    start: float_v >>
                    (FloatRangeItem::From { start, include_start })
                )
            )
        ),
        Property::FloatRange
    ),
    pair!(separator, tag!(";"))
));

named!(date_range<Property>, delimited!(
    tuple!(tag!("range"), separator, tag!(":"), separator),
    map!(
        separated_nonempty_list_complete!(
            delimited!(separator, tag!(","), separator),
            alt_complete!(
                do_parse!(
                    start: date_v >>
                    tag!("..") >>
                    end: date_v >>
                    (DateRangeItem::Bounded { start, end })
                ) |
                map!(
                    terminated!(date_v, tag!("..")),
                    |start| DateRangeItem::From { start }
                ) |
                map!(
                    preceded!(tag!(".."), date_v),
                    |end| DateRangeItem::To { end }
                )
            )
        ),
        Property::DateRange
    ),
    pair!(separator, tag!(";"))
));

named!(string_range<StringRange>, map_opt!(
    uint_range,
    |ur: UintRange| ur.iter().map(|uri| uri.to_string_range_item()).collect()
));

named!(binary_range<BinaryRange>, map_opt!(
    uint_range,
    |ur: UintRange| ur.iter().map(|uri| uri.to_binary_range_item()).collect()
));

named!(size<Property>, delimited!(
    tuple!(tag!("size"), separator, tag!(":"), separator),
    map!(
        separated_nonempty_list_complete!(
            delimited!(separator, tag!(","), separator),
            alt_complete!(
                do_parse!(
                    start: map_res!(
                        map_res!(take_while!(is_digit), str::from_utf8),
                        FromStr::from_str
                    ) >>
                    tag!("..") >>
                    end: map_res!(
                        map_res!(take_while!(is_digit), str::from_utf8),
                        FromStr::from_str
                    ) >>
                    (UintRangeItem::Bounded { start, end })
                ) |
                map!(
                    terminated!(
                        map_res!(
                            map_res!(take_while!(is_digit), str::from_utf8),
                            FromStr::from_str
                        ),
                        tag!("..")
                    ),
                    |start| UintRangeItem::From { start }
                ) |
                map!(
                    map_res!(
                        map_res!(take_while!(is_digit), str::from_utf8),
                        FromStr::from_str
                    ),
                    UintRangeItem::Single
                )
            )
        ),
        Property::Size
    ),
    pair!(separator, tag!(";"))
));

named!(ordered<Property>, delimited!(
    tuple!(tag!("ordered"), separator, tag!(":"), separator),
    alt_complete!(
        map!(
            alt_complete!(tag!("yes") | tag!("1")),
            |_| Property::Ordered(true)
        ) |
        map!(
            alt_complete!(tag!("no") | tag!("0")),
            |_| Property::Ordered(false)
        )
    ),
    pair!(separator, tag!(";"))
));

// Types impossible to distinguish:
//      Uint vs Int, if the Int happens to be positive
//      String vs Binary, if the Binary happens to be valid Unicode
named!(header_statement<HeaderStatement>, do_parse!(
    name: name >>
    separator >>
    tag!(":=") >>
    separator >>
    value: alt_complete!(
        // By including the terminator in these parsers, we stop floats from getting interpreted as
        // integers.
        map!(
            terminated!(
                map_res!(map_res!(take_while!(is_digit), str::from_utf8), FromStr::from_str),
                pair!(separator, tag!(";"))
            ),
            |value| HeaderStatement::Uint { name, value }
        ) |
        map!(
            terminated!(int_v, pair!(separator, tag!(";"))),
            |value| HeaderStatement::Int { name, value }
        ) |
        map!(
            terminated!(float_v, pair!(separator, tag!(";"))),
            |value| HeaderStatement::Float { name, value }
        ) |
        map!(
            terminated!(date_v, pair!(separator, tag!(";"))),
            |value| HeaderStatement::Date { name, value }
        ) |
        map!(
            terminated!(
                map_res!(binary_v, String::from_utf8),
                pair!(separator, tag!(";"))
            ),
            |value| HeaderStatement::String { name, value }
        ) |
        map!(
            terminated!(binary_v, pair!(separator, tag!(";"))),
            |value| HeaderStatement::Binary { name, value }
        ) |
        map!(
            terminated!(::parsers::name, pair!(separator, tag!(";"))),
            |value| HeaderStatement::Named { name, value }
        )
    ) >>
    (value)
));

named!(hblock<Header>, preceded!(
    tuple!(tag!("declare"), separator, tag!("header"), separator, tag!("{"), separator),
    separated_nonempty_list_complete!(separator, header_statement)
));

fn update_newtype_with_property<'a, 'b>(mut nt: NewType<'a>, p: Property<'b>) -> NewType<'a> {
    nt.update(p);
    nt
}

named!(dtype<NewType>, do_parse!(
    name: name >>
    separator >>
    tag!(":=") >>
    separator >>
    value: switch!(type_,

        Type::Int => alt_complete!(
            // It _has_ properties
            delimited!(
                tuple!(separator, tag!("["), separator),
                fold_many1!(
                    preceded!(separator, alt_complete!(int_range | int_def)),
                    NewType::Int { name, default: None, range: None },
                    update_newtype_with_property
                ),
                tuple!(separator, tag!("]"), separator, opt!(tag!(";")))
            ) |
            // It _doesn't_ have properties
            value!(NewType::Int { name, default: None, range: None })
        ) |

        Type::Uint => alt_complete!(
            // It _has_ properties
            delimited!(
                tuple!(separator, tag!("["), separator),
                fold_many1!(
                    preceded!(separator, alt_complete!(uint_range | uint_def)),
                    NewType::Uint { name, default: None, range: None },
                    update_newtype_with_property
                ),
                tuple!(separator, tag!("]"), separator, opt!(tag!(";")))
            ) |
            // It _doesn't_ have properties
            value!(NewType::Uint { name, default: None, range: None })
        ) |


        _ => value!(NewType::Int { name, default: None, range: None })
    ) >>
    (value)
));

#[cfg(test)]
mod tests;
