use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::{
        char,
        complete::{alpha1, alphanumeric0, i64, multispace0},
    },
    combinator::{all_consuming, complete},
    error,
    multi::*,
    sequence::{delimited, preceded, separated_pair, terminated},
};

#[derive(Debug, Clone)]
pub enum Type {
    Boolean,
    Number,
    Unit,
    Func {
        params: Vec<(String, Type)>,
        ret: Box<Type>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Boolean, Type::Boolean) => true,
            (Type::Number, Type::Number) => true,
            (Type::Unit, Type::Unit) => true,
            (
                Type::Func {
                    params: l_params,
                    ret: l_ret,
                },
                Type::Func {
                    params: r_params,
                    ret: r_ret,
                },
            ) => {
                if l_params.len() != r_params.len() || l_ret != r_ret {
                    return false;
                }
                l_params.iter().zip(r_params).all(|((_, l), (_, r))| l == r)
            }
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Term {
    True,
    False,
    Number(i64),
    Var(String),
    Lambda {
        var_type: Vec<(String, Type)>,
        func: Box<Term>,
    },
    FunCall {
        func: Box<Term>,
        args: Vec<Term>,
    },
    Add(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Seq {
        body: Box<Term>,
        rest: Box<Term>,
    },
    Assign {
        name: String,
        init: Box<Term>,
    },
}

fn term_number(input: &str) -> IResult<&str, Term> {
    i64.map(|x| Term::Number(x)).parse(input)
}

fn parse_name(input: &str) -> IResult<&str, String> {
    let (input, prefix) = alpha1(input)?;
    let (input, suffix) = alphanumeric0(input)?;

    let name = prefix.to_string() + suffix;
    Ok((input, name))
}

fn term_var(input: &str) -> IResult<&str, Term> {
    let (input, name) = parse_name(input)?;

    let var = if name == "true" {
        Term::True
    } else if name == "false" {
        Term::False
    } else {
        Term::Var(name)
    };
    Ok((input, var))
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    if let Ok((input, name)) = parse_name(input) {
        return match name.as_str() {
            "bool" => Ok((input, Type::Boolean)),
            "number" => Ok((input, Type::Number)),
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::AlphaNumeric,
            ))),
        };
    }

    if let Ok((input, (params, ret))) = delimited(
        (char('('), multispace0),
        separated_list0(
            (multispace0, tag(","), multispace0),
            separated_pair(
                parse_name,
                (multispace0, char(':'), multispace0),
                parse_type,
            ),
        ),
        (multispace0, char(')')),
    )
    .and(preceded((multispace0, tag("=>"), multispace0), parse_type))
    .parse(input)
    {
        return Ok((
            input,
            Type::Func {
                params: params,
                ret: Box::new(ret),
            },
        ));
    }

    (multispace0, tag("()")).map(|_| Type::Unit).parse(input)
}

fn lambda_expr(input: &str) -> IResult<&str, Term> {
    complete(
        delimited(
            (char('('), multispace0),
            separated_list0(
                (multispace0, tag(","), multispace0),
                separated_pair(
                    parse_name,
                    (multispace0, char(':'), multispace0),
                    parse_type,
                ),
            ),
            (multispace0, char(')')),
        )
        .and(preceded((multispace0, tag("=>"), multispace0), term))
        .map(|(params, func)| Term::Lambda {
            var_type: params,
            func: Box::new(func),
        }),
    )
    .parse(input)
}

fn funcall_expr(input: &str) -> IResult<&str, Term> {
    complete(
        alt((
            term_var,
            delimited((char('('), multispace0), term, (multispace0, char(')'))),
        ))
        .and(delimited(
            (char('('), multispace0),
            separated_list0((multispace0, tag(","), multispace0), term),
            (multispace0, char(')')),
        ))
        .map(|(func, args)| Term::FunCall {
            func: Box::new(func),
            args: args,
        }),
    )
    .parse(input)
}

fn term_expr(input: &str) -> IResult<&str, Term> {
    let paren_term = delimited(char('('), term, (multispace0, char(')')));
    // let r = funcall_expr.parse(input);
    // println!("{:?}", r);

    alt((funcall_expr, lambda_expr, paren_term, term_number, term_var)).parse(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, _) = multispace0(input)?;
    let (input, expr) = term_expr.parse(input)?;

    let mut term_add = preceded((multispace0, char('+')), term);
    let mut term_cond = preceded(
        (multispace0, char('?')),
        separated_pair(term, (multispace0, char(':')), term),
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

fn statement(input: &str) -> IResult<&str, Term> {
    if let Ok((input, _)) = tag::<&str, &str, error::Error<&str>>("const ").parse(input) {
        let (input, name) = preceded(multispace0, parse_name).parse(input)?;
        let (input, _) = (multispace0, tag("="), multispace0).parse(input)?;
        let (input, init) = term.parse(input)?;

        return Ok((
            input,
            Term::Assign {
                name: name,
                init: Box::new(init),
            },
        ));
    }

    term.parse(input)
}

fn statements(input: &str) -> IResult<&str, Term> {
    let (mut input, mut terms) = many0(terminated(
        complete(statement),
        (multispace0, tag(";"), multispace0),
    ))
    .parse(input)?;
    let last = statement.parse(input);

    if let Ok((input_last, last_term)) = last {
        terms.push(last_term);
        input = input_last;
    } else if terms.is_empty() {
        return last;
    }

    let mut iter = terms.into_iter().rev();
    let mut crt = iter.next().unwrap();

    for term in iter {
        crt = Term::Seq {
            body: Box::new(term),
            rest: Box::new(crt),
        };
    }
    Ok((input, crt))
}

pub fn parse(input: &str) -> Result<Term, String> {
    match all_consuming(terminated(statements, multispace0)).parse(input) {
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

    fn var(name: &str) -> Term {
        Term::Var(name.to_string())
    }

    fn create_add(a: i64, b: i64) -> Term {
        Term::Add(Box::new(Term::Number(a)), Box::new(Term::Number(b)))
    }

    #[test]
    fn parse_true() {
        assert_eq!(parse("  true"), Ok(Term::True));
    }

    #[test]
    fn parse_false() {
        assert_eq!(parse("false"), Ok(Term::False));
    }

    #[test]
    fn parse_digits() {
        assert_eq!(parse("-12345"), Ok(Term::Number(-12345)));
    }

    #[test]
    fn parse_add() {
        assert_eq!(parse("123 + 45"), Ok(create_add(123, 45)));
    }

    #[test]
    fn parse_add2() {
        assert_eq!(
            parse("123 + 45 + 67"),
            Ok(Term::Add(
                Box::new(Term::Number(123)),
                Box::new(create_add(45, 67))
            ))
        );
    }

    #[test]
    fn parse_if() {
        assert_eq!(
            parse("( true ? 12 : 34 ) ? 45 : -67"),
            Ok(Term::If(
                Box::new(Term::If(
                    Box::new(Term::True),
                    Box::new(Term::Number(12)),
                    Box::new(Term::Number(34))
                )),
                Box::new(Term::Number(45)),
                Box::new(Term::Number(-67))
            ))
        );
    }

    #[test]
    fn parse_if2() {
        assert_eq!(
            parse("true ? ( 12 ? 34 : 45 ) : -67"),
            Ok(Term::If(
                Box::new(Term::True),
                Box::new(Term::If(
                    Box::new(Term::Number(12)),
                    Box::new(Term::Number(34)),
                    Box::new(Term::Number(45)),
                )),
                Box::new(Term::Number(-67))
            ))
        );
    }
    #[test]
    fn parse_if3() {
        assert_eq!(
            parse("true ? 12 : ( 34 ? 45 : -67 )"),
            Ok(Term::If(
                Box::new(Term::True),
                Box::new(Term::Number(12)),
                Box::new(Term::If(
                    Box::new(Term::Number(34)),
                    Box::new(Term::Number(45)),
                    Box::new(Term::Number(-67))
                ))
            ))
        );
    }

    #[test]
    fn parse_var() {
        assert_eq!(parse("  true "), Ok(Term::True));
        assert_eq!(parse("  truex "), Ok(Term::Var(String::from("truex"))));
        assert_eq!(parse(" a "), Ok(Term::Var(String::from("a"))));
    }

    #[test]
    fn test_parse_type() {
        assert_eq!(parse_type.parse("()"), Ok(("", Type::Unit)));
        assert_eq!(parse_type.parse("number"), Ok(("", Type::Number)));
        assert_eq!(parse_type.parse("bool"), Ok(("", Type::Boolean)));
        assert!(parse_type.parse("booll").is_err());
        assert_eq!(
            parse_type.parse("() => ()"),
            Ok((
                "",
                Type::Func {
                    params: vec![],
                    ret: Box::new(Type::Unit)
                }
            ))
        );
        assert_eq!(
            parse_type.parse("(x: number, y: bool) => number"),
            Ok((
                "",
                Type::Func {
                    params: vec![
                        (String::from("x"), Type::Number),
                        (String::from("y"), Type::Boolean)
                    ],
                    ret: Box::new(Type::Number)
                }
            ))
        );
        assert_eq!(
            parse_type.parse("(x: number, f: (a: number) => bool ) => bool"),
            Ok((
                "",
                Type::Func {
                    params: vec![
                        (String::from("x"), Type::Number),
                        (
                            String::from("f"),
                            Type::Func {
                                params: vec![(String::from("a"), Type::Number)],
                                ret: Box::new(Type::Boolean)
                            }
                        ),
                    ],
                    ret: Box::new(Type::Boolean)
                }
            ))
        );
    }

    #[test]
    fn test_parse_lambda() {
        assert_eq!(
            parse("() => 1"),
            Ok(Term::Lambda {
                var_type: vec![],
                func: Box::new(Term::Number(1))
            })
        );

        assert_eq!(
            parse("(x: number, y: number, z: bool) => z ? x + y : 123"),
            Ok(Term::Lambda {
                var_type: vec![
                    (String::from("x"), Type::Number),
                    (String::from("y"), Type::Number),
                    (String::from("z"), Type::Boolean)
                ],
                func: Box::new(Term::If(
                    Box::new(Term::Var(String::from("z"))),
                    Box::new(Term::Add(
                        Box::new(Term::Var(String::from("x"))),
                        Box::new(Term::Var(String::from("y")))
                    )),
                    Box::new(Term::Number(123))
                ))
            })
        );

        assert_eq!(
            parse("(x: number, f: (a: number) => bool) => x"),
            Ok(Term::Lambda {
                var_type: vec![
                    (String::from("x"), Type::Number),
                    (
                        String::from("f"),
                        Type::Func {
                            params: vec![(String::from("a"), Type::Number)],
                            ret: Box::new(Type::Boolean),
                        }
                    ),
                ],
                func: Box::new(Term::Var(String::from("x")))
            })
        );
    }

    #[test]
    fn test_parse_funcall() {
        assert_eq!(
            parse("f()"),
            Ok(Term::FunCall {
                func: Box::new(Term::Var(String::from("f"))),
                args: vec![]
            })
        );

        assert_eq!(
            parse("f(1, x)"),
            Ok(Term::FunCall {
                func: Box::new(Term::Var(String::from("f"))),
                args: vec![Term::Number(1), Term::Var(String::from("x"))]
            })
        );

        assert_eq!(
            parse("g(1, f(x, y), z)"),
            Ok(Term::FunCall {
                func: Box::new(Term::Var(String::from("g"))),
                args: vec![
                    Term::Number(1),
                    Term::FunCall {
                        func: Box::new(Term::Var(String::from("f"))),
                        args: vec![Term::Var(String::from("x")), Term::Var(String::from("y"))]
                    },
                    Term::Var(String::from("z")),
                ]
            })
        );

        assert_eq!(
            parse("((x : bool, y : number) => y)(true, 123)"),
            Ok(Term::FunCall {
                func: Box::new(Term::Lambda {
                    var_type: vec![
                        (String::from("x"), Type::Boolean),
                        (String::from("y"), Type::Number),
                    ],
                    func: Box::new(Term::Var(String::from("y"))),
                }),
                args: vec![Term::True, Term::Number(123),]
            })
        );
    }

    #[test]
    fn test_statement() {
        assert_eq!(
            parse("1; true"),
            Ok(Term::Seq {
                body: Box::new(Term::Number(1)),
                rest: Box::new(Term::True),
            })
        );

        assert_eq!(
            parse("() => 1; () => false;"),
            Ok(Term::Seq {
                body: Box::new(Term::Lambda {
                    var_type: vec![],
                    func: Box::new(Term::Number(1))
                }),
                rest: Box::new(Term::Lambda {
                    var_type: vec![],
                    func: Box::new(Term::False)
                }),
            })
        );

        assert_eq!(
            parse("1; 2; 3; 4;"),
            Ok(Term::Seq {
                body: Box::new(Term::Number(1)),
                rest: Box::new(Term::Seq {
                    body: Box::new(Term::Number(2)),
                    rest: Box::new(Term::Seq {
                        body: Box::new(Term::Number(3)),
                        rest: Box::new(Term::Number(4)),
                    }),
                }),
            })
        );
    }

    #[test]
    fn test_assign() {
        assert_eq!(
            parse("const x = 1; x"),
            Ok(Term::Seq {
                body: Box::new(Term::Assign {
                    name: String::from("x"),
                    init: Box::new(Term::Number(1))
                }),
                rest: Box::new(var("x")),
            })
        );
        {
            let r = parse("const 2 = 1; x");
            assert!(r.is_err(), "actual: {:?}", r);
        }

        assert_eq!(
            parse("const f = (x : number) => x + 1; f(2);"),
            Ok(Term::Seq {
                body: Box::new(Term::Assign {
                    name: String::from("f"),
                    init: Box::new(Term::Lambda {
                        var_type: vec![(String::from("x"), Type::Number)],
                        func: Box::new(Term::Add(Box::new(var("x")), Box::new(Term::Number(1))))
                    }),
                }),
                rest: Box::new(Term::FunCall {
                    func: Box::new(var("f")),
                    args: vec![Term::Number(2)],
                }),
            })
        );

        assert_eq!(
            parse("const a = 1; const b = 2; const c = 3; a + b + c"),
            Ok(Term::Seq {
                body: Box::new(Term::Assign {
                    name: String::from("a"),
                    init: Box::new(Term::Number(1))
                }),
                rest: Box::new(Term::Seq {
                    body: Box::new(Term::Assign {
                        name: String::from("b"),
                        init: Box::new(Term::Number(2))
                    }),
                    rest: Box::new(Term::Seq {
                        body: Box::new(Term::Assign {
                            name: String::from("c"),
                            init: Box::new(Term::Number(3))
                        }),
                        rest: Box::new(Term::Add(
                            Box::new(var("a")),
                            Box::new(Term::Add(Box::new(var("b")), Box::new(var("c"))))
                        )),
                    }),
                }),
            })
        );
    }
}
