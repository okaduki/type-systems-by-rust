use std::collections::HashMap;
use std::{cmp::Ordering, fmt::Pointer};

use nom::Err;

use crate::parser::{Term, Type};

/**
 * typecheck for chapter9(generics)
 */

#[allow(dead_code)]
pub fn typecheck_with_generics(t: &Term) -> Result<Type, String> {
    let mut dict = HashMap::new();
    let mut tyvars = Vec::new();
    let mut factory = FreshTypeFactory::new();

    typecheck_impl(t, &mut dict, &mut tyvars, &mut factory)
}

struct FreshTypeFactory {
    counter: usize,
}

impl FreshTypeFactory {
    fn new() -> Self {
        FreshTypeFactory { counter: 1 }
    }

    fn create(&mut self, ty_vars: &Vec<String>, typ: &Type) -> (Vec<String>, Type) {
        let mut typ = typ.clone();
        let mut new_names = Vec::new();
        for ty_var in ty_vars {
            let new_var = format!("{}@{}", ty_var, self.counter);
            self.counter += 1;
            new_names.push(new_var.clone());

            let rep_ty_var = &Type::TypeVar { name: new_var };
            typ = subst(&typ, ty_var, rep_ty_var, self);
        }

        (new_names, typ)
    }
}

fn subst(t: &Type, ty_var_name: &str, rep_ty: &Type, factory: &mut FreshTypeFactory) -> Type {
    match t {
        Type::TypeVar { name } => {
            if name == ty_var_name {
                rep_ty.clone()
            } else {
                t.clone()
            }
        }
        Type::TypeAbs { name, typ } => {
            if name.iter().any(|n| n == ty_var_name) {
                return t.clone();
            }
            let (new_name, new_typ) = factory.create(name, typ);

            let subst_typ = subst(&new_typ, ty_var_name, rep_ty, factory);
            Type::TypeAbs {
                name: new_name,
                typ: Box::new(subst_typ),
            }
        }
        Type::Func { params, ret } => Type::Func {
            params: params
                .iter()
                .map(|(n, ty)| (n.clone(), subst(ty, ty_var_name, rep_ty, factory)))
                .collect(),
            ret: Box::new(subst(ret, ty_var_name, rep_ty, factory)),
        },
        Type::Object { map } => Type::Object {
            map: map
                .iter()
                .map(|(k, v)| (k.clone(), subst(v, ty_var_name, rep_ty, factory)))
                .collect(),
        },
        other => other.clone(),
    }
}

fn type_eq_sub(ty1: &Type, ty2: &Type, map: &mut HashMap<&str, &str>) -> bool {
    match (ty1, ty2) {
        (Type::Boolean, Type::Boolean)
        | (Type::Number, Type::Number)
        | (Type::Unit, Type::Unit) => true,
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
            l_params
                .iter()
                .zip(r_params)
                .all(|((_, l), (_, r))| type_eq_sub(l, r, map))
        }
        (Type::Object { map: mapl }, Type::Object { map: mapr }) => {
            mapl.iter().all(|(name, ltyp)| {
                mapr.get(name)
                    .and_then(|rtyp| type_eq_sub(ltyp, rtyp, map).then_some(()))
                    .is_some()
            })
        }
        (Type::TypeVar { name: lname }, Type::TypeVar { name: rname }) => {
            if let Some(n) = map.get(lname.as_str()) {
                n == rname
            } else {
                false
            }
        }
        (
            Type::TypeAbs {
                name: lname,
                typ: ltyp,
            },
            Type::TypeAbs {
                name: rname,
                typ: rtyp,
            },
        ) => {
            if lname.len() != rname.len() {
                return false;
            }

            let mut newmap = map.clone();
            for (lft, rgt) in lname.iter().zip(rname) {
                newmap.insert(lft, rgt);
            }
            type_eq_sub(&ltyp, &rtyp, &mut newmap)
        }
        _ => false,
    }
}

fn type_eq(ty1: &Type, ty2: &Type, tyvars: &Vec<&str>) -> bool {
    let mut map = HashMap::new();
    for &var in tyvars {
        map.insert(var, var);
    }

    type_eq_sub(ty1, ty2, &mut map)
}

fn typecheck_impl(
    t: &Term,
    dict: &mut HashMap<String, Type>,
    tyvars: &mut Vec<&str>,
    factory: &mut FreshTypeFactory,
) -> Result<Type, String> {
    match t {
        Term::False | Term::True => Ok(Type::Boolean),
        Term::Number(_) => Ok(Type::Number),
        Term::Add(lhs, rhs) => {
            let ltype = typecheck_impl(lhs, dict, tyvars, factory)?;
            let rtype = typecheck_impl(rhs, dict, tyvars, factory)?;
            if !type_eq(&ltype, &Type::Number, &tyvars) || !type_eq(&rtype, &Type::Number, &tyvars)
            {
                return Err(String::from("number expected"));
            }

            Ok(Type::Number)
        }
        Term::If(cond, then, els) => {
            let cond_type = typecheck_impl(cond, dict, tyvars, factory)?;
            let then_type = typecheck_impl(then, dict, tyvars, factory)?;
            let els_type = typecheck_impl(els, dict, tyvars, factory)?;

            if !type_eq(&cond_type, &Type::Boolean, &tyvars) {
                return Err(String::from("boolean expected"));
            }
            if !type_eq(&then_type, &els_type, &tyvars) {
                return Err(format!(
                    "then and else have different types, left = {:?}, right = {:?}",
                    &then_type, &els_type,
                ));
            }

            Ok(then_type)
        }
        Term::Lambda { var_type, func } => {
            let mut new_dict = dict.clone();
            for (var, typ) in var_type {
                new_dict.insert(var.clone(), typ.clone());
            }
            let r = typecheck_impl(func, &mut new_dict, tyvars, factory)?;
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
            let func_type = typecheck_impl(&func, dict, tyvars, factory)?;
            if let Type::Func { params, ret } = func_type {
                if params.len() != args.len() {
                    Err(format!(
                        "parameter length mismatch: expected {}, but got {}",
                        params.len(),
                        args.len()
                    ))
                } else {
                    for (param, arg) in params.iter().zip(args) {
                        let atype = typecheck_impl(arg, dict, tyvars, factory)?;
                        if !type_eq(&param.1, &atype, &tyvars) {
                            return Err(format!(
                                "parameter type mismatch: expected {:?}, but got {:?}",
                                param.1, atype
                            ));
                        }
                    }
                    Ok(*ret)
                }
            } else {
                Err(format!("function type expected, t = {:?}", &t))
            }
        }
        Term::Seq { body, rest } => {
            typecheck_impl(&body, dict, tyvars, factory)?;
            typecheck_impl(&rest, dict, tyvars, factory)
        }
        Term::Assign { name, init } => {
            let ty = typecheck_impl(&init, dict, tyvars, factory)?;
            dict.insert(name.clone(), ty);
            Ok(Type::Unit)
        }
        Term::Object { map } => {
            let mut props_type = HashMap::new();
            for (name, prop) in map {
                props_type.insert(name.clone(), typecheck_impl(prop, dict, tyvars, factory)?);
            }
            Ok(Type::Object { map: props_type })
        }
        Term::ObjectRead { var, prop } => {
            let ty = typecheck_impl(var, dict, tyvars, factory)?;
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
        Term::Generics {
            type_abs,
            var_type,
            func,
        } => {
            let mut new_tyvars = tyvars.clone();
            for tyvar in type_abs {
                new_tyvars.push(&tyvar);
            }

            let lambda = Term::Lambda {
                var_type: var_type.clone(),
                func: func.clone(),
            };
            let ret_type = typecheck_impl(&lambda, dict, &mut new_tyvars, factory)?;

            Ok(Type::TypeAbs {
                name: type_abs.clone(),
                typ: Box::new(ret_type),
            })
        }
        Term::TypeApp {
            type_abs,
            type_args,
        } => {
            let typ = typecheck_impl(&type_abs, dict, tyvars, factory)?;

            if let Type::TypeAbs {
                name: type_params,
                typ,
            } = typ
            {
                if type_args.len() != type_params.len() {
                    Err(format!("wrong number of type arguments, t = {:?}", t))
                } else {
                    let mut typ = *typ.clone();

                    for (param, arg) in type_params.iter().zip(type_args) {
                        typ = subst(&typ, &param, arg, factory);
                    }

                    Ok(typ)
                }
            } else {
                Err(format!("TypeAbs expected, t = {:?}", t))
            }
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests_typecheck_generics {
    use std::vec;

    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_generics() {
        let term = parse(
            "
            const f = <T>(x: T) => x;
            f<number>;
        ",
        );
        assert!(term.is_ok());
        assert_eq!(
            typecheck_with_generics(&term.unwrap()),
            Ok(Type::Func {
                params: vec![(String::from("x"), Type::Number)],
                ret: Box::new(Type::Number),
            })
        );

        let term = parse(
            "
            const f = <T>(x: T) => x;
            f<boolean>;
        ",
        );
        assert!(term.is_ok());
        assert_eq!(
            typecheck_with_generics(&term.unwrap()),
            Ok(Type::Func {
                params: vec![(String::from("x"), Type::Boolean)],
                ret: Box::new(Type::Boolean),
            })
        );

        let term = parse(
            "
            const select = <T>(cond: boolean, a: T, b: T) => (cond ? a : b);
            select<number>(true, 1, 2);
        ",
        );
        assert!(term.is_ok());
        assert_eq!(typecheck_with_generics(&term.unwrap()), Ok(Type::Number));

        let shadowing_term = parse(
            "
            const f = <T>(arg1: T, arg2: <T>(x: T) => boolean) => true;
            f<number>;
        ",
        );
        assert!(shadowing_term.is_ok());
        assert_eq!(
            typecheck_with_generics(&shadowing_term.unwrap()),
            Ok(Type::Func {
                params: vec![
                    (String::from("arg1"), Type::Number),
                    (
                        String::from("arg2"),
                        Type::TypeAbs {
                            name: vec![String::from("T")],
                            typ: Box::new(Type::Func {
                                params: vec![(
                                    String::from("x"),
                                    Type::TypeVar {
                                        name: String::from("T")
                                    }
                                )],
                                ret: Box::new(Type::Boolean)
                            }),
                        }
                    )
                ],
                ret: Box::new(Type::Boolean)
            })
        );

        let capture_term = parse(
            "
            const foo = <T>(arg1: T, arg2: <U>(x: T, y: U) => boolean) => true;
            const bar = <U>() => foo<U>;
            bar
            ",
        );
        assert!(capture_term.is_ok());
        assert_eq!(
            typecheck_with_generics(&capture_term.unwrap()),
            Ok(Type::TypeAbs {
                name: vec![String::from("U")],
                typ: Box::new(Type::Func {
                    params: vec![],
                    ret: Box::new(Type::Func {
                        params: vec![
                            (
                                String::from("arg1"),
                                Type::TypeVar {
                                    name: String::from("U")
                                }
                            ),
                            (
                                String::from("arg2"),
                                Type::TypeAbs {
                                    name: vec![String::from("U@1")],
                                    typ: Box::new(Type::Func {
                                        params: vec![
                                            (
                                                String::from("x"),
                                                Type::TypeVar {
                                                    name: String::from("U")
                                                }
                                            ),
                                            (
                                                String::from("y"),
                                                Type::TypeVar {
                                                    name: String::from("U@1")
                                                }
                                            )
                                        ],
                                        ret: Box::new(Type::Boolean)
                                    }),
                                }
                            )
                        ],
                        ret: Box::new(Type::Boolean)
                    })
                }),
            })
        );
    }
}
