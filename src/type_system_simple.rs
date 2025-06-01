use std::cmp::Ordering;
use std::collections::HashMap;

use crate::parser::{Term, Type};

/**
 * typecheck for chapter1 - chapter6(without subtype, recursive type)
 */

#[allow(dead_code)]
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
                return Err(String::from("boolean expected"));
            }
            if then_type != els_type {
                return Err(String::from("then and else have different types"));
            }

            Ok(then_type)
        }
        Term::Lambda { var_type, func } => {
            let mut new_dict = dict.clone();
            for (var, typ) in var_type {
                new_dict.insert(var.clone(), typ.clone());
            }
            let r = typecheck_impl(func, &mut new_dict)?;
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
        Term::Seq { body, rest } => {
            typecheck_impl(&body, dict)?;
            typecheck_impl(&rest, dict)
        }
        Term::Assign { name, init } => {
            let ty = typecheck_impl(&init, dict)?;
            dict.insert(name.clone(), ty);
            Ok(Type::Unit)
        }
        Term::Object { map } => {
            let mut props_type = HashMap::new();
            for (name, prop) in map {
                props_type.insert(name.clone(), typecheck_impl(prop, dict)?);
            }
            Ok(Type::Object { map: props_type })
        }
        Term::ObjectRead { var, prop } => {
            let ty = typecheck_impl(var, dict)?;
            if let Type::Object { map } = ty {
                if let Some(t) = map.get(prop) {
                    Ok(t.clone())
                } else {
                    Err(format!("key {} is not found", prop))
                }
            } else {
                Err(format!("object type expected, t = {:?}", t))
            }
        }
        Term::RecFunc {
            name,
            var_type,
            func,
            ret_type,
        } => {
            let functype = Type::Func {
                params: var_type.to_vec(),
                ret: Box::new(ret_type.clone()),
            };
            dict.insert(name.clone(), functype);

            let mut new_dict = dict.clone();
            for (var, typ) in var_type {
                new_dict.insert(var.clone(), typ.clone());
            }
            let typ = typecheck_impl(func, &mut new_dict)?;
            if typ != *ret_type {
                Err(format!(
                    "return type is inconsistent, type {:?}, ret_type {:?}",
                    typ, ret_type
                ))
            } else {
                Ok(Type::Unit)
            }
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests_typecheck_simple {
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
            typecheck(&parse("(x: number, y: boolean) => 1").unwrap()),
            Ok(Type::Func {
                params: vec![
                    (String::from("x"), Type::Number),
                    (String::from("y"), Type::Boolean)
                ],
                ret: Box::new(Type::Number),
            })
        );

        assert_eq!(
            typecheck(&parse("(x: number, y: boolean) => y ? x : x + 1").unwrap()),
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
            typecheck(&parse("(x: boolean) => ((x : number) => x + 1)").unwrap()),
            Ok(Type::Func {
                params: vec![(String::from("x"), Type::Boolean)],
                ret: Box::new(Type::Func {
                    params: vec![(String::from("x"), Type::Number)],
                    ret: Box::new(Type::Number),
                })
            })
        );
        assert!(typecheck(&parse("(x: number) => ((x : boolean) => x + 1)").unwrap()).is_err());

        assert!(typecheck(&parse("(x: number, y: boolean) => x + y").unwrap()).is_err());
        assert!(typecheck(&parse("(x: number, y: boolean) => z").unwrap()).is_err());
        assert!(typecheck(&parse("(f: (x: number) => number) => x").unwrap()).is_err());
    }

    #[test]
    fn type_funcall() {
        assert_eq!(typecheck(&parse("(() => 1)()").unwrap()), Ok(Type::Number));
        assert_eq!(
            typecheck(
                &parse("((x : number, y : number, z: boolean ) => z ? x : y)(1, 2, false)")
                    .unwrap()
            ),
            Ok(Type::Number)
        );
        assert_eq!(
            typecheck(
                &parse("((f : (x : number) => boolean) => f(1))( (y: number) => true )").unwrap()
            ),
            Ok(Type::Boolean)
        );
        {
            let r = typecheck(
                &parse("((x : number, y : number, z: boolean ) => z ? x : y)(x, y, z)").unwrap(),
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

    #[test]
    fn type_assign() {
        assert_eq!(
            typecheck(&parse("const x = 1; x").unwrap()),
            Ok(Type::Number)
        );
        assert_eq!(
            typecheck(
                &parse(
                    "const f = (x : number, y : number, z: boolean ) => z ? x : y;
                    f(1, 2, false);"
                )
                .unwrap()
            ),
            Ok(Type::Number)
        );
        assert_eq!(
            typecheck(
                &parse(
                    "const g = ((f : (x : number) => boolean) => f(1));
                    const f = (y: number) => true;
                    g(f)"
                )
                .unwrap()
            ),
            Ok(Type::Boolean)
        );
        {
            let r = typecheck(
                &parse(
                    "const f = (x : number, y : number, z: boolean ) => z ? x : y;
                f(x, y, z)",
                )
                .unwrap(),
            );
            assert!(r.is_err(), "actual type: {:?}", r);
        }
        {
            let r = typecheck(
                &parse(
                    "const f = (x: number) => x + 1;
                    const y = true;
                    f(y);",
                )
                .unwrap(),
            );
            assert!(r.is_err(), "actual type: {:?}", r);
        }
    }

    #[test]
    fn type_object() {
        /*
        assert_eq!(
            typecheck(&parse("{ foo: 1, bar: true, inner: {bazz: true, hoge: false}}").unwrap()),
            Ok(Type::Object {
                map: HashMap::from([
                    (String::from("foo"), Type::Number),
                    (String::from("bar"), Type::Boolean),
                    (
                        String::from("inner"),
                        Type::Object {
                            map: HashMap::from([
                                (String::from("bazz"), Type::Boolean),
                                (String::from("hoge"), Type::Boolean),
                            ])
                        }
                    ),
                ])
            })
        );
         */

        assert_eq!(
            typecheck(
                &parse("const a = { foo: 1, bar: true, inner: {bazz: true, hoge: false}}; a.inner.hoge;")
                .unwrap()
            ),
            Ok(Type::Boolean)
        );

        {
            let r = typecheck(
                &parse("const a = { foo: 1, bar: true, inner: {bazz: true, hoge: false}}; a.not.found;")
                .unwrap(),
            );
            assert!(r.is_err(), "actual type: {:?}", r);
        }
    }

    #[test]
    fn type_recfunc() {
        assert_eq!(
            typecheck(
                &parse("function sum(x : number): number { true ? 0 : x + sum(x + -1) }; sum(5); ")
                    .unwrap()
            ),
            Ok(Type::Number)
        );
    }
}
