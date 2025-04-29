use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::{
        char,
        complete::{i64, space0},
    },
    sequence::{delimited, preceded, separated_pair},
};

#[derive(Debug, PartialEq)]
pub enum Term {
    True,
    False,
    Number(i64),
    Add(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

fn term_bool(input: &str) -> IResult<&str, Term> {
    alt((tag("true"), tag("false")))
        .map(|i| if i == "true" { Term::True } else { Term::False })
        .parse(input)
}

fn term_number(input: &str) -> IResult<&str, Term> {
    i64.map(|x| Term::Number(x)).parse(input)
}

fn term_expr(input: &str) -> IResult<&str, Term> {
    let paren_term = delimited(char('('), term, (space0, char(')')));
    alt((paren_term, term_bool, term_number)).parse(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, _) = space0(input)?;
    let (input, expr) = term_expr.parse(input)?;

    let mut term_add = preceded((space0, char('+')), term);
    let mut term_cond = preceded(
        (space0, char('?')),
        separated_pair(term, (space0, char(':')), term),
    );

    if let Ok((input, expr2)) = term_add.parse(input) {
        return Ok((input, Term::Add(Box::new(expr), Box::new(expr2))));
    }

    if let Ok((input, (expr2, expr3))) = term_cond.parse(input) {
        return Ok((
            input,
            Term::If(Box::new(expr), Box::new(expr2), Box::new(expr3)),
        ));
    }

    Ok((input, expr))
}

pub fn parse(input: &str) -> Result<Term, String> {
    match term(input) {
        Ok((_, term)) => Ok(term),
        Err(e) => {
            let s = e.to_string();
            Err(s)
        }
    }
}

#[cfg(test)]
mod tests_parse {
    use super::*;

    fn create_add(a: i64, b: i64) -> Term {
        Term::Add(Box::new(Term::Number(a)), Box::new(Term::Number(b)))
    }

    #[test]
    fn parse_true() {
        assert_eq!(term("  true"), Ok(("", Term::True)));
    }

    #[test]
    fn parse_false() {
        assert_eq!(term("false"), Ok(("", Term::False)));
    }

    #[test]
    fn parse_digits() {
        assert_eq!(term("-12345"), Ok(("", Term::Number(-12345))));
    }

    #[test]
    fn parse_add() {
        assert_eq!(term("123 + 45"), Ok(("", create_add(123, 45))));
    }

    #[test]
    fn parse_add2() {
        assert_eq!(
            term("123 + 45 + 67"),
            Ok((
                "",
                Term::Add(Box::new(Term::Number(123)), Box::new(create_add(45, 67)))
            ))
        );
    }

    #[test]
    fn parse_if() {
        assert_eq!(
            term("( true ? 12 : 34 ) ? 45 : -67"),
            Ok((
                "",
                Term::If(
                    Box::new(Term::If(
                        Box::new(Term::True),
                        Box::new(Term::Number(12)),
                        Box::new(Term::Number(34))
                    )),
                    Box::new(Term::Number(45)),
                    Box::new(Term::Number(-67))
                )
            ))
        );
    }

    #[test]
    fn parse_if2() {
        assert_eq!(
            term("true ? ( 12 ? 34 : 45 ) : -67"),
            Ok((
                "",
                Term::If(
                    Box::new(Term::True),
                    Box::new(Term::If(
                        Box::new(Term::Number(12)),
                        Box::new(Term::Number(34)),
                        Box::new(Term::Number(45)),
                    )),
                    Box::new(Term::Number(-67))
                )
            ))
        );
    }
    #[test]
    fn parse_if3() {
        assert_eq!(
            term("true ? 12 : ( 34 ? 45 : -67 )"),
            Ok((
                "",
                Term::If(
                    Box::new(Term::True),
                    Box::new(Term::Number(12)),
                    Box::new(Term::If(
                        Box::new(Term::Number(34)),
                        Box::new(Term::Number(45)),
                        Box::new(Term::Number(-67))
                    ))
                )
            ))
        );
    }
}
