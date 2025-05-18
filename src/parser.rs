use core::fmt;
use std::collections::HashMap;

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

#[derive(Clone)]
pub enum Type {
    Boolean,
    Number,
    Unit,
    Func {
        params: Vec<(String, Type)>,
        ret: Box<Type>,
    },
    Object {
        map: HashMap<String, Type>,
    },
    TypeVar {
        name: String,
    },
    Rec {
        name: String,
        typ: Box<Type>,
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
            (Type::Object { map: mapl }, Type::Object { map: mapr }) => mapl == mapr,
            (Type::TypeVar { name: lname }, Type::TypeVar { name: rname }) => lname == rname,
            (
                Type::Rec {
                    name: lname,
                    typ: ltype,
                },
                Type::Rec {
                    name: rname,
                    typ: rtype,
                },
            ) => lname == rname && ltype == rtype,
            _ => false,
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Number => write!(f, "Number"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Unit => write!(f, "()"),
            Type::TypeVar { name } => write!(f, "{}", name),
            Type::Func { params, ret } => {
                write!(f, "(")?;
                for p in params {
                    write!(f, "{}: ", p.0)?;
                    p.1.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, ") => ")?;
                ret.fmt(f)
            }
            Type::Object { map } => {
                write!(f, "{{")?;
                for (k, v) in map {
                    write!(f, "{}: ", k)?;
                    v.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, "}}")
            }
            Type::Rec { name, typ } => {
                write!(f, "(\\mu {}. ", name)?;
                typ.fmt(f)?;
                write!(f, ")")
            }
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Term {
    True,
    False,
    Number(i64),
    Var(String),
    Lambda {
        var_type: Vec<(String, Type)>,
        func: Box<Term>,
    },
    RecFunc {
        name: String,
        var_type: Vec<(String, Type)>,
        func: Box<Term>,
        ret_type: Type,
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
    Object {
        map: HashMap<String, Term>,
    },
    ObjectRead {
        var: Box<Term>,
        prop: String,
    },
    TypeDecl {
        name: String,
        typ: Type,
    },
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            Term::Number(n) => write!(f, "{}", n),
            Term::Var(name) => write!(f, "{}", name),
            Term::Lambda { var_type, func } => {
                write!(f, "lambda(")?;

                let mut sep = "";
                for (k, v) in var_type {
                    write!(f, "{}{}: {:?}", sep, k, v)?;
                    sep = ",";
                }
                write!(f, ") => {:?}", func)
            }
            Term::RecFunc {
                name,
                var_type,
                func,
                ret_type,
            } => {
                write!(f, "function {}(", name)?;
                let mut sep = "";
                for (k, v) in var_type {
                    write!(f, "{}{}: {:?}", sep, k, v)?;
                    sep = ",";
                }
                write!(f, "): {:?} {{ {:?} }}", ret_type, func)
            }
            Term::FunCall { func, args } => {
                write!(f, "{:?}(", func)?;

                let mut sep = "";
                for t in args {
                    write!(f, "{}{:?}", sep, t)?;
                    sep = ",";
                }
                write!(f, ")")
            }
            Term::Add(lhs, rhs) => {
                write!(f, "{:?} + {:?}", lhs, rhs)
            }
            Term::If(cond, then_t, else_t) => {
                write!(f, "{:?} ? {:?} : {:?}", cond, then_t, else_t)
            }
            Term::Seq { body, rest } => {
                write!(f, "{:?};\n{:?}", body, rest)
            }
            Term::Assign { name, init } => {
                write!(f, "const {} = {:?}", name, init)
            }
            Term::Object { map } => {
                write!(f, "{{")?;
                let mut sep = "";
                for (k, v) in map {
                    write!(f, "{}{}: {:?}", sep, k, v)?;
                    sep = ",";
                }
                write!(f, "}}")
            }
            Term::ObjectRead { var, prop } => {
                write!(f, "{:?}.{}", var, prop)
            }
            Term::TypeDecl { name, typ } => {
                write!(f, "type {} = {:?}", name, typ)
            }
        }
    }
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
            "boolean" => Ok((input, Type::Boolean)),
            "number" => Ok((input, Type::Number)),
            n => Ok((
                input,
                Type::TypeVar {
                    name: String::from(n),
                },
            )),
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
    } else if let Ok((input, entries)) = delimited(
        (char('{'), multispace0),
        separated_list0(
            (multispace0, tag(","), multispace0),
            separated_pair(
                parse_name,
                (multispace0, char(':'), multispace0),
                parse_type,
            ),
        ),
        (multispace0, char('}')),
    )
    .parse(input)
    {
        return Ok((
            input,
            Type::Object {
                map: entries.into_iter().collect(),
            },
        ));
    }

    (multispace0, tag("()")).map(|_| Type::Unit).parse(input)
}

fn params_expr(input: &str) -> IResult<&str, Vec<(String, Type)>> {
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
    .parse(input)
}

fn lambda_expr(input: &str) -> IResult<&str, Term> {
    complete(
        params_expr
            .and(preceded((multispace0, tag("=>"), multispace0), term))
            .map(|(params, func)| Term::Lambda {
                var_type: params,
                func: Box::new(func),
            }),
    )
    .parse(input)
}

fn funcall_expr(input: &str) -> IResult<&str, Vec<Term>> {
    complete(delimited(
        (char('('), multispace0),
        separated_list0((multispace0, tag(","), multispace0), term),
        (multispace0, char(')')),
    ))
    .parse(input)
}

fn obj_expr(input: &str) -> IResult<&str, Term> {
    let (input, t) = delimited(
        (char('{'), multispace0),
        separated_list0(
            (multispace0, char(','), multispace0),
            separated_pair(parse_name, (multispace0, char(':'), multispace0), term),
        ),
        (multispace0, char('}'), multispace0),
    )
    .parse(input)?;

    let mut map = HashMap::new();
    for (k, v) in t {
        map.insert(k, v);
    }

    Ok((input, Term::Object { map: map }))
}

fn term_expr(input: &str) -> IResult<&str, Term> {
    let paren_term = delimited(char('('), term, (multispace0, char(')')));

    alt((obj_expr, lambda_expr, paren_term, term_number, term_var)).parse(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, _) = multispace0(input)?;
    let (input, expr) = term_expr.parse(input)?;

    let mut term_add = preceded((multispace0, char('+')), term);
    let mut term_cond = preceded(
        (multispace0, char('?')),
        separated_pair(term, (multispace0, char(':')), term),
    );

    let (input, t) = {
        if let Ok((input, expr2)) = term_add.parse(input) {
            (input, Term::Add(Box::new(expr), Box::new(expr2)))
        } else if let Ok((input, (expr2, expr3))) = term_cond.parse(input) {
            (
                input,
                Term::If(Box::new(expr), Box::new(expr2), Box::new(expr3)),
            )
        } else if let Ok((input, args)) = many1(funcall_expr).parse(input) {
            let mut crt = expr;
            for arg in args {
                crt = Term::FunCall {
                    func: Box::new(crt),
                    args: arg,
                };
            }

            (input, crt)
        } else {
            (input, expr)
        }
    };

    if let Ok((input, _)) = tag::<&str, &str, error::Error<&str>>(".").parse(input) {
        let (input, props) = separated_list1(tag("."), parse_name).parse(input)?;

        let mut crt = t;
        for prop in props {
            crt = Term::ObjectRead {
                var: Box::new(crt),
                prop: prop,
            };
        }
        Ok((input, crt))
    } else {
        Ok((input, t))
    }
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
    } else if let Ok((input, _)) = tag::<&str, &str, error::Error<&str>>("type ").parse(input) {
        let (input, name) = preceded(multispace0, parse_name).parse(input)?;
        let (input, _) = (multispace0, tag("="), multispace0).parse(input)?;
        let (input, typ) = parse_type.parse(input)?;

        return Ok((
            input,
            Term::TypeDecl {
                name: name,
                typ: typ,
            },
        ));
    } else if let Ok((input, _)) = tag::<&str, &str, error::Error<&str>>("function ").parse(input) {
        let (input, name) = preceded(multispace0, parse_name).parse(input)?;
        let (input, params) = params_expr.parse(input)?;
        let (input, _) = tag(":").parse(input)?;
        let (input, ret_type) = preceded(multispace0, parse_type).parse(input)?;
        let (input, func) =
            delimited((multispace0, tag("{")), term, (multispace0, tag("}"))).parse(input)?;

        return Ok((
            input,
            Term::RecFunc {
                name: name,
                var_type: params,
                func: Box::new(func),
                ret_type: ret_type,
            },
        ));
    }

    term.parse(input)
}

fn statements(input: &str) -> IResult<&str, Term> {
    let (input, _) = multispace0.parse(input)?;

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
    use std::vec;

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
        assert_eq!(parse_type.parse("boolean"), Ok(("", Type::Boolean)));
        assert_eq!(
            parse_type.parse("booll"),
            Ok((
                "",
                Type::TypeVar {
                    name: String::from("booll")
                }
            ))
        );
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
            parse_type.parse("(x: number, y: boolean) => number"),
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
            parse_type.parse("(x: number, f: (a: number) => boolean ) => boolean"),
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
            parse("(x: number, y: number, z: boolean) => z ? x + y : 123"),
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
            parse("(x: number, f: (a: number) => boolean) => x"),
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
            parse("((x : boolean, y : number) => y)(true, 123)"),
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

        assert_eq!(
            parse("const f = (x : number) => ((y : number) => x + y); f(2)(3);"),
            Ok(Term::Seq {
                body: Box::new(Term::Assign {
                    name: String::from("f"),
                    init: Box::new(Term::Lambda {
                        var_type: vec![(String::from("x"), Type::Number)],
                        func: Box::new(Term::Lambda {
                            var_type: vec![(String::from("y"), Type::Number)],
                            func: Box::new(Term::Add(Box::new(var("x")), Box::new(var("y")),)),
                        })
                    }),
                }),
                rest: Box::new(Term::FunCall {
                    func: Box::new(Term::FunCall {
                        func: Box::new(var("f")),
                        args: vec![Term::Number(2)],
                    }),
                    args: vec![Term::Number(3)],
                }),
            })
        );
    }

    #[test]
    fn test_object() {
        let obj = Term::Object {
            map: HashMap::from([
                (String::from("foo"), Term::Number(1)),
                (String::from("bar"), Term::True),
                (
                    String::from("inner"),
                    Term::Object {
                        map: HashMap::from([
                            (String::from("bazz"), Term::True),
                            (String::from("hoge"), Term::False),
                        ]),
                    },
                ),
            ]),
        };

        assert_eq!(
            parse("{ foo: 1, bar: true, inner: {bazz: true, hoge: false}}"),
            Ok(obj.clone())
        );

        assert_eq!(
            parse("const a = { foo: 1, bar: true, inner: {bazz: true, hoge: false}}; a.inner.hoge"),
            Ok(Term::Seq {
                body: Box::new(Term::Assign {
                    name: String::from("a"),
                    init: Box::new(obj),
                }),
                rest: Box::new(Term::ObjectRead {
                    var: Box::new(Term::ObjectRead {
                        var: Box::new(Term::Var(String::from("a"))),
                        prop: String::from("inner")
                    }),
                    prop: String::from("hoge")
                }),
            })
        );

        assert_eq!(
            parse("(x : { foo: number, bar: boolean}) => x.foo"),
            Ok(Term::Lambda {
                var_type: vec![(
                    String::from("x"),
                    Type::Object {
                        map: HashMap::from([
                            (String::from("foo"), Type::Number),
                            (String::from("bar"), Type::Boolean)
                        ])
                    }
                )],
                func: Box::new(Term::ObjectRead {
                    var: Box::new(var("x")),
                    prop: String::from("foo")
                }),
            })
        );
    }

    #[test]
    fn test_recfunc() {
        assert_eq!(
            parse("function sum(x : number): number { true ? 0 : x + sum(x + -1) }; sum(5); "),
            Ok(Term::Seq {
                body: Box::new(Term::RecFunc {
                    name: String::from("sum"),
                    var_type: vec![(String::from("x"), Type::Number)],
                    func: Box::new(Term::If(
                        Box::new(Term::True),
                        Box::new(Term::Number(0)),
                        Box::new(Term::Add(
                            Box::new(var("x")),
                            Box::new(Term::FunCall {
                                func: Box::new(var("sum")),
                                args: vec![Term::Add(
                                    Box::new(var("x")),
                                    Box::new(Term::Number(-1))
                                )]
                            })
                        ))
                    )),
                    ret_type: Type::Number,
                }),
                rest: Box::new(Term::FunCall {
                    func: Box::new(var("sum")),
                    args: vec![Term::Number(5)],
                }),
            })
        );
    }

    #[test]
    fn test_rectype() {
        assert_eq!(
            parse("type X = { foo: X }; (arg : X) => 1;"),
            Ok(Term::Seq {
                body: Box::new(Term::TypeDecl {
                    name: String::from("X"),
                    typ: Type::Object {
                        map: HashMap::from([(
                            String::from("foo"),
                            Type::TypeVar {
                                name: String::from("X")
                            }
                        )])
                    }
                }),
                rest: Box::new(Term::Lambda {
                    var_type: vec![(
                        String::from("arg"),
                        Type::TypeVar {
                            name: String::from("X")
                        }
                    )],
                    func: Box::new(Term::Number(1)),
                }),
            })
        );

        let expected_terms = vec![
            Term::TypeDecl {
                name: String::from("NumStream"),
                typ: Type::Object {
                    map: HashMap::from([
                        (String::from("num"), Type::Number),
                        (
                            String::from("rest"),
                            Type::Func {
                                params: vec![],
                                ret: Box::new(Type::TypeVar {
                                    name: String::from("NumStream"),
                                }),
                            },
                        ),
                    ]),
                },
            },
            Term::RecFunc {
                name: String::from("numbers"),
                var_type: vec![(String::from("n"), Type::Number)],
                ret_type: Type::TypeVar {
                    name: String::from("NumStream"),
                },
                func: Box::new(Term::Object {
                    map: HashMap::from([
                        (String::from("num"), var("n")),
                        (
                            String::from("rest"),
                            Term::Lambda {
                                var_type: vec![],
                                func: Box::new(Term::FunCall {
                                    args: vec![Term::Add(
                                        Box::new(var("n")),
                                        Box::new(Term::Number(1)),
                                    )],
                                    func: Box::new(var("numbers")),
                                }),
                            },
                        ),
                    ]),
                }),
            },
            Term::Assign {
                name: String::from("nx1"),
                init: Box::new(Term::FunCall {
                    func: Box::new(var("numbers")),
                    args: vec![Term::Number(1)],
                }),
            },
            Term::Assign {
                name: String::from("nx2"),
                init: Box::new(Term::FunCall {
                    func: Box::new(Term::ObjectRead {
                        var: Box::new(var("nx1")),
                        prop: String::from("rest"),
                    }),
                    args: vec![],
                }),
            },
            Term::Assign {
                name: String::from("nx3"),
                init: Box::new(Term::FunCall {
                    func: Box::new(Term::ObjectRead {
                        var: Box::new(var("nx2")),
                        prop: String::from("rest"),
                    }),
                    args: vec![],
                }),
            },
            var("nx3"),
        ];
        let terms = expected_terms
            .into_iter()
            .rev()
            .reduce(|rest, body| Term::Seq {
                body: Box::new(body),
                rest: Box::new(rest),
            })
            .unwrap();

        assert_eq!(
            parse(
                "
                type NumStream = { num: number, rest: () => NumStream };
                function numbers(n: number): NumStream {
                    { num: n, rest: () => numbers(n+1) }
                };
                
                const nx1 = numbers(1);
                const nx2 = (nx1.rest)();
                const nx3 = (nx2.rest)();
                nx3;
                "
            ),
            Ok(terms),
        );
    }
}
