use std::{collections::HashMap, hash::Hash};

use nom::Err;

use crate::parser::{Term, Type};

pub fn typecheck(t: &Term) -> Result<Type, String> {
    let mut dict = HashMap::new();
    typecheck_impl(t, &mut dict)
}

fn typecheck_impl(t: &Term, dict: &mut HashMap<String, Type>) -> Result<Type, String> {
    match t {
        Term::False | Term::True => Ok(Type::Boolean),
        Term::Number(_) => Ok(Type::Number),
        Term::Add(lhs, rhs) => {
            let ltype = typecheck_impl(lhs, dict)?;
            let rtype = typecheck_impl(rhs, dict)?;
            if ltype != Type::Number || rtype != Type::Number {
                return Err(String::from("number expected"));
            }

            Ok(Type::Number)
        }
        Term::If(cond, then, els) => {
            let cond_type = typecheck_impl(cond, dict)?;
            let then_type = typecheck_impl(then, dict)?;
            let els_type = typecheck_impl(els, dict)?;

            if cond_type != Type::Boolean {
                return Err(String::from("bool expected"));
            }
            if then_type != els_type {
                return Err(String::from("then and else have different types"));
            }

            Ok(then_type)
        }
        Term::Lambda { var_type, func } => {
            let mut old_vals = HashMap::new();
            for (var, typ) in var_type {
                if let Some(oldval) = dict.insert(var.clone(), typ.clone()) {
                    old_vals.insert(var, Some(oldval));
                } else {
                    old_vals.insert(var, None);
                }
            }
            let r = typecheck_impl(func, dict)?;

            for (k, v) in old_vals {
                if let Some(val) = v {
                    *dict.get_mut(k).unwrap() = val;
                } else {
                    dict.remove(k);
                }
            }
            Ok(Type::Func {
                params: var_type.to_vec(),
                ret: Box::new(r),
            })
        }
        Term::Var(name) => {
            if let Some(var_type) = dict.get(name) {
                Ok(var_type.clone())
            } else {
                Err(format!("variable '{}' is undefined.", name))
            }
        }
        Term::FunCall { func, args } => {
            let func_type = typecheck_impl(&func, dict)?;
            if let Type::Func { params, ret } = func_type {
                if params.len() != args.len() {
                    Err(format!(
                        "parameter length mismatch: expected {}, but got {}",
                        params.len(),
                        args.len()
                    ))
                } else {
                    for (param, arg) in params.iter().zip(args) {
                        let atype = typecheck_impl(arg, dict)?;
                        if param.1 != atype {
                            return Err(format!(
                                "parameter type mismatch: expected {:?}, but got {:?}",
                                param.1, atype
                            ));
                        }
                    }
                    Ok(*ret)
                }
            } else {
                Err(String::from("function type expected"))
            }
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests_typecheck {
    use std::vec;

    use super::*;
    use crate::parser::parse;

    #[test]
    fn type_constants() {
        assert_eq!(typecheck(&Term::True), Ok(Type::Boolean));
        assert_eq!(typecheck(&Term::False), Ok(Type::Boolean));
        assert_eq!(typecheck(&Term::Number(1234)), Ok(Type::Number));
    }

    #[test]
    fn type_add() {
        assert_eq!(typecheck(&parse("1 + 2").unwrap()), Ok(Type::Number));
        assert_eq!(typecheck(&parse("1 + (2 + 3)").unwrap()), Ok(Type::Number));
        assert!(typecheck(&parse("1 + true").unwrap()).is_err());
        assert!(typecheck(&parse("true + 1").unwrap()).is_err());
    }

    #[test]
    fn type_if() {
        assert_eq!(typecheck(&parse("true ? 1 : 2").unwrap()), Ok(Type::Number));
        assert_eq!(
            typecheck(&parse("true ? 1 + 2 : 2 + 3").unwrap()),
            Ok(Type::Number)
        );
        assert_eq!(
            typecheck(&parse("true ? false : true").unwrap()),
            Ok(Type::Boolean)
        );
        assert_eq!(
            typecheck(&parse("true ? (false ? true : false) : (true ? false : true)").unwrap()),
            Ok(Type::Boolean)
        );

        assert!(typecheck(&parse("true ? 1 : false").unwrap()).is_err());
        assert!(typecheck(&parse("(1 + 2) ? true : false").unwrap()).is_err());
        assert!(typecheck(&parse("(1 + true) ? 1 : 2").unwrap()).is_err());
    }

    #[test]
    fn type_lambda() {
        assert_eq!(
            typecheck(&parse("() => 1").unwrap()),
            Ok(Type::Func {
                params: vec![],
                ret: Box::new(Type::Number),
            })
        );

        assert_eq!(
            typecheck(&parse("(x: number, y: bool) => 1").unwrap()),
            Ok(Type::Func {
                params: vec![
                    (String::from("x"), Type::Number),
                    (String::from("y"), Type::Boolean)
                ],
                ret: Box::new(Type::Number),
            })
        );

        assert_eq!(
            typecheck(&parse("(x: number, y: bool) => y ? x : x + 1").unwrap()),
            Ok(Type::Func {
                params: vec![
                    (String::from("x"), Type::Number),
                    (String::from("y"), Type::Boolean)
                ],
                ret: Box::new(Type::Number),
            })
        );

        assert_eq!(
            typecheck(&parse("(f : (x: number) => number ) => 1").unwrap()),
            Ok(Type::Func {
                params: vec![(
                    String::from("f"),
                    Type::Func {
                        params: vec![(String::from("x"), Type::Number)],
                        ret: Box::new(Type::Number),
                    }
                ),],
                ret: Box::new(Type::Number),
            })
        );
        assert_eq!(
            typecheck(&parse("(x: bool) => ((x : number) => x + 1)").unwrap()),
            Ok(Type::Func {
                params: vec![(String::from("x"), Type::Boolean)],
                ret: Box::new(Type::Func {
                    params: vec![(String::from("x"), Type::Number)],
                    ret: Box::new(Type::Number),
                })
            })
        );
        assert!(typecheck(&parse("(x: number) => ((x : bool) => x + 1)").unwrap()).is_err());

        assert!(typecheck(&parse("(x: number, y: bool) => x + y").unwrap()).is_err());
        assert!(typecheck(&parse("(x: number, y: bool) => z").unwrap()).is_err());
        assert!(typecheck(&parse("(f: (x: number) => number) => x").unwrap()).is_err());
    }

    #[test]
    fn type_funcall() {
        assert_eq!(typecheck(&parse("(() => 1)()").unwrap()), Ok(Type::Number));
        assert_eq!(
            typecheck(
                &parse("((x : number, y : number, z: bool ) => z ? x : y)(1, 2, false)").unwrap()
            ),
            Ok(Type::Number)
        );
        assert_eq!(
            typecheck(
                &parse("((f : (x : number) => bool) => f(1))( (y: number) => true )").unwrap()
            ),
            Ok(Type::Boolean)
        );
        {
            let r = typecheck(
                &parse("((x : number, y : number, z: bool ) => z ? x : y)(x, y, z)").unwrap(),
            );
            assert!(r.is_err(), "actual type: {:?}", r);
        }
        {
            let r = typecheck(&parse("x()").unwrap());
            assert!(r.is_err(), "actual type: {:?}", r);
        }
        {
            let r = typecheck(&parse("((f : number ) => f)( (y: number) => true )").unwrap());
            assert!(r.is_err(), "actual type: {:?}", r);
        }
    }
}
