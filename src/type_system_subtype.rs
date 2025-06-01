use std::cmp::Ordering;
use std::collections::HashMap;

use crate::parser::{Term, Type};

/**
 * typecheck for chapter7(subtype)
 */

#[allow(dead_code)]
pub fn typecheck_with_subtype(t: &Term) -> Result<Type, String> {
    let mut dict = HashMap::new();
    typecheck_with_subtype_impl(t, &mut dict)
}

impl Type {
    fn is_subtype(&self, other: &Self) -> bool {
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
                if l_params.len() != r_params.len() {
                    return false;
                }
                if !l_ret.is_subtype(&r_ret) {
                    return false;
                }

                l_params
                    .iter()
                    .zip(r_params)
                    .all(|((_, l), (_, r))| r.is_subtype(l))
            }
            (Type::Object { map: mapl }, Type::Object { map: mapr }) => {
                mapr.iter().all(|(lname, ltype)| {
                    mapl.get(lname)
                        .and_then(|rtype| ltype.is_subtype(rtype).then_some(()))
                        .is_some()
                })
            }
            _ => false,
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self.is_subtype(other), other.is_subtype(self)) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Less),
            (false, true) => Some(Ordering::Greater),
            _ => None,
        }
    }
}

fn typecheck_with_subtype_impl(t: &Term, dict: &mut HashMap<String, Type>) -> Result<Type, String> {
    match t {
        Term::False | Term::True => Ok(Type::Boolean),
        Term::Number(_) => Ok(Type::Number),
        Term::Add(lhs, rhs) => {
            let ltype = typecheck_with_subtype_impl(lhs, dict)?;
            let rtype = typecheck_with_subtype_impl(rhs, dict)?;
            if ltype <= Type::Number || rtype <= Type::Number {
                return Err(String::from("number expected"));
            }

            Ok(ltype) // 簡単実装
        }
        Term::If(_, _, _) => {
            unimplemented!("subtypecheck for if-statement is unsupported.");
        }
        Term::Lambda { var_type, func } => {
            let mut new_dict = dict.clone();
            for (var, typ) in var_type {
                new_dict.insert(var.clone(), typ.clone());
            }
            let r = typecheck_with_subtype_impl(func, &mut new_dict)?;
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
            let func_type = typecheck_with_subtype_impl(&func, dict)?;
            if let Type::Func { params, ret } = func_type {
                if params.len() != args.len() {
                    Err(format!(
                        "parameter length mismatch: expected {}, but got {}",
                        params.len(),
                        args.len()
                    ))
                } else {
                    for (param, arg) in params.iter().zip(args) {
                        let atype = typecheck_with_subtype_impl(arg, dict)?;
                        if !(atype <= param.1) {
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
            typecheck_with_subtype_impl(&body, dict)?;
            typecheck_with_subtype_impl(&rest, dict)
        }
        Term::Assign { name, init } => {
            let ty = typecheck_with_subtype_impl(&init, dict)?;
            dict.insert(name.clone(), ty);
            Ok(Type::Unit)
        }
        Term::Object { map } => {
            let mut props_type = HashMap::new();
            for (name, prop) in map {
                props_type.insert(name.clone(), typecheck_with_subtype_impl(prop, dict)?);
            }
            Ok(Type::Object { map: props_type })
        }
        Term::ObjectRead { var, prop } => {
            let ty = typecheck_with_subtype_impl(var, dict)?;
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
            let typ = typecheck_with_subtype_impl(func, &mut new_dict)?;
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
mod tests_typecheck_subtype {
    use std::vec;

    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_subtype() {
        // object subtype
        assert_eq!(
            typecheck_with_subtype(
                &parse(
                    "
                    const f = (a : { num : number }) => a.num;
                    const x = { num: 1, bar: true };
                    f(x);
                    "
                )
                .unwrap()
            ),
            Ok(Type::Number)
        );

        // covariant
        assert_eq!(
            typecheck_with_subtype(
                &parse(
                    "
                    const f = ( x: () => { n: number } ) => x().n;
                    const g = () => { n: 1, b: true };
                    f(g);
                    "
                )
                .unwrap()
            ),
            Ok(Type::Number)
        );

        // contravariant
        assert_eq!(
            typecheck_with_subtype(
                &parse(
                    "
                    const f = (getnum: (x: { n: number, b: boolean }) => number) => getnum({ n: 1, b: false });
                    const g = (x: { n : number }) => x.n;
                    f(g);
                    "
                )
                .unwrap()
            ),
            Ok(Type::Number)
        );

        // invalid
        {
            let r = typecheck_with_subtype(
                &parse(
                    "
                    const f = ( x: () => { n: number, b: boolean } ) => x().n;
                    const g = () => { n: 1 };
                    f(g);
                    ",
                )
                .unwrap(),
            );
            assert!(r.is_err(), "actual type: {:?}", r);
        }
        {
            let r = typecheck_with_subtype(
                &parse(
                    "
                    const f = (getnum: (x: { n: number }) => number) => getnum({ n: 1, b: false });
                    const g = (x: { n : number, b: boolean }) => x.n;
                    f(g);
                    ",
                )
                .unwrap(),
            );
            assert!(r.is_err(), "actual type: {:?}", r);
        }
    }
}
