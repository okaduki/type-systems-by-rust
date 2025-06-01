use std::cmp::Ordering;
use std::collections::HashMap;

use crate::parser::{Term, Type};

/**
 * typecheck for chapter8(recursive type)
 */

#[allow(dead_code)]
pub fn typecheck_with_rectype(t: &Term) -> Result<Type, String> {
    let mut dict = HashMap::new();
    let mut tyvar_dict = HashMap::new();
    typecheck_with_rectype_impl(t, &mut dict, &mut tyvar_dict)
}

fn expand_type(ty: &Type, ty_var_name: &str, rep_ty: &Type) -> Type {
    match ty {
        Type::Func { params, ret } => Type::Func {
            params: params
                .into_iter()
                .map(|(name, typ)| (name.clone(), expand_type(typ, ty_var_name, rep_ty)))
                .collect(),
            ret: Box::new(expand_type(&ret, ty_var_name, rep_ty)),
        },
        Type::Object { map } => Type::Object {
            map: map
                .into_iter()
                .map(|(name, prop)| (name.clone(), expand_type(prop, ty_var_name, rep_ty)))
                .collect(),
        },
        Type::Rec { name, typ } => {
            if name == ty_var_name {
                return ty.clone();
            }
            let new_type = expand_type(typ, ty_var_name, rep_ty);
            Type::Rec {
                name: name.clone(),
                typ: Box::new(new_type),
            }
        }
        Type::TypeVar { name } => {
            if name == ty_var_name {
                rep_ty.clone()
            } else {
                ty.clone()
            }
        }
        _ => ty.clone(),
    }
}

fn simplify_type(ty: &Type, tyvar_dict: &mut HashMap<String, Type>) -> Type {
    match ty {
        Type::TypeVar { name } => {
            if let Some(val) = tyvar_dict.get(name) {
                let val2 = val.clone();
                simplify_type(&val2, tyvar_dict)
            } else {
                ty.clone()
            }
        }
        Type::Rec { name, typ } => {
            let repty = Type::Rec {
                name: name.clone(),
                typ: typ.clone(),
            };
            let expanded = expand_type(typ, name.as_str(), &repty);
            simplify_type(&expanded, tyvar_dict)
        }
        _ => ty.clone(),
    }
}

fn type_eq_naive(ty1: &Type, ty2: &Type, map: &mut HashMap<&str, &str>) -> bool {
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
                .all(|((_, l), (_, r))| type_eq_naive(l, r, map))
        }
        (Type::Object { map: mapl }, Type::Object { map: mapr }) => {
            mapl.iter().all(|(name, ltyp)| {
                mapr.get(name)
                    .and_then(|rtyp| type_eq_naive(ltyp, rtyp, map).then_some(()))
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
            Type::Rec {
                name: lname,
                typ: ltype,
            },
            Type::Rec {
                name: rname,
                typ: rtype,
            },
        ) => {
            let mut newmap = map.clone();
            newmap.insert(lname, rname);
            type_eq_naive(&ltype, &rtype, &mut newmap)
        }
        _ => false,
    }
}

fn type_eq_sub(
    ty1: &Type,
    ty2: &Type,
    seen: &mut Vec<(&Type, &Type)>,
    tyvar_dict: &mut HashMap<String, Type>,
) -> bool {
    for (seen_ltyp, seen_rtyp) in seen.iter() {
        if type_eq_naive(&seen_ltyp, ty1, &mut HashMap::new())
            && type_eq_naive(&seen_rtyp, ty2, &mut HashMap::new())
        {
            return true;
        }
    }

    if let Type::Rec { .. } = ty1 {
        let mut new_seen = seen.clone();
        new_seen.push((ty1, ty2));
        return type_eq_sub(
            &simplify_type(ty1, tyvar_dict),
            ty2,
            &mut new_seen,
            tyvar_dict,
        );
    }
    if let Type::Rec { .. } = ty2 {
        let mut new_seen = seen.clone();
        new_seen.push((ty1, ty2));

        return type_eq_sub(
            ty1,
            &simplify_type(ty2, tyvar_dict),
            &mut new_seen,
            tyvar_dict,
        );
    }

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
            if l_params.len() != r_params.len() {
                return false;
            }

            let s = l_params
                .iter()
                .zip(r_params)
                .all(|((_, l), (_, r))| type_eq_sub(l, r, seen, tyvar_dict));

            s && type_eq_sub(&l_ret, &r_ret, seen, tyvar_dict)
        }
        (Type::Object { map: mapl }, Type::Object { map: mapr }) => {
            mapl.iter().all(|(name, ltyp)| {
                mapr.get(name)
                    .and_then(|rtyp| type_eq_sub(ltyp, rtyp, seen, tyvar_dict).then_some(()))
                    .is_some()
            })
        }
        (Type::TypeVar { .. }, _)
        | (_, Type::TypeVar { .. })
        | (Type::Rec { .. }, _)
        | (_, Type::Rec { .. }) => {
            unreachable!("ty1 {:?}, ty2 {:?}", ty1, ty2)
        }
        _ => false,
    }
}

fn type_eq(ty1: &Type, ty2: &Type, tyvar_dict: &mut HashMap<String, Type>) -> bool {
    type_eq_sub(ty1, ty2, &mut Vec::new(), tyvar_dict)
}

fn typecheck_with_rectype_impl(
    t: &Term,
    dict: &mut HashMap<String, Type>,
    tyvar_dict: &mut HashMap<String, Type>,
) -> Result<Type, String> {
    fn typecheck_and_simplify(
        term: &Term,
        dic: &mut HashMap<String, Type>,
        tyvar_dict: &mut HashMap<String, Type>,
    ) -> Result<Type, String> {
        typecheck_with_rectype_impl(term, dic, tyvar_dict).map(|ty| simplify_type(&ty, tyvar_dict))
    }

    match t {
        Term::False | Term::True => Ok(Type::Boolean),
        Term::Number(_) => Ok(Type::Number),
        Term::Add(lhs, rhs) => {
            let ltype = typecheck_and_simplify(lhs, dict, tyvar_dict)?;
            let rtype = typecheck_and_simplify(rhs, dict, tyvar_dict)?;

            if !type_eq(&ltype, &Type::Number, tyvar_dict)
                || !type_eq(&rtype, &Type::Number, tyvar_dict)
            {
                return Err(String::from("number expected"));
            }

            Ok(Type::Number)
        }
        Term::If(cond, then, els) => {
            let cond_type = typecheck_and_simplify(cond, dict, tyvar_dict)?;
            let then_type = typecheck_and_simplify(then, dict, tyvar_dict)?;
            let els_type = typecheck_and_simplify(els, dict, tyvar_dict)?;

            if !type_eq(&cond_type, &Type::Boolean, tyvar_dict) {
                return Err(String::from("boolean expected"));
            }
            if !type_eq(&then_type, &els_type, tyvar_dict) {
                return Err(String::from("then and else have different types"));
            }

            Ok(then_type)
        }
        Term::Lambda { var_type, func } => {
            let mut new_dict = dict.clone();
            for (var, typ) in var_type {
                new_dict.insert(var.clone(), simplify_type(typ, tyvar_dict));
            }
            let r = typecheck_with_rectype_impl(func, &mut new_dict, tyvar_dict)?;
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
            let func_type = typecheck_and_simplify(&func, dict, tyvar_dict)?;
            if let Type::Func { params, ret } = func_type {
                if params.len() != args.len() {
                    Err(format!(
                        "parameter length mismatch: expected {}, but got {}",
                        params.len(),
                        args.len()
                    ))
                } else {
                    for (param, arg) in params.iter().zip(args) {
                        let atype = typecheck_and_simplify(arg, dict, tyvar_dict)?;
                        if !type_eq(&param.1, &atype, tyvar_dict) {
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
            typecheck_with_rectype_impl(&body, dict, tyvar_dict)?;
            typecheck_with_rectype_impl(&rest, dict, tyvar_dict)
        }
        Term::Assign { name, init } => {
            let ty = typecheck_and_simplify(&init, dict, tyvar_dict)?;
            dict.insert(name.clone(), ty);
            Ok(Type::Unit)
        }
        Term::Object { map } => {
            let mut props_type = HashMap::new();
            for (name, prop) in map {
                props_type.insert(
                    name.clone(),
                    typecheck_and_simplify(prop, dict, tyvar_dict)?,
                );
            }
            Ok(Type::Object { map: props_type })
        }
        Term::ObjectRead { var, prop } => {
            let ty = typecheck_and_simplify(var, dict, tyvar_dict)?;
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
            let var_type: Vec<_> = var_type
                .into_iter()
                .map(|(n, t)| (n.clone(), simplify_type(t, tyvar_dict)))
                .collect();
            let ret_type = simplify_type(ret_type, tyvar_dict);

            let functype = Type::Func {
                params: var_type.clone(),
                ret: Box::new(ret_type.clone()),
            };
            dict.insert(name.clone(), functype);

            let mut new_dict = dict.clone();
            for (var, typ) in var_type {
                new_dict.insert(var.clone(), typ);
            }
            let typ = typecheck_and_simplify(func, &mut new_dict, tyvar_dict)?;
            if type_eq(&typ, &ret_type, tyvar_dict) {
                Ok(Type::Unit)
            } else {
                Err(format!(
                    "return type is inconsistent, type {:?}, ret_type {:?}",
                    typ, ret_type
                ))
            }
        }
        Term::TypeDecl { name, typ } => {
            let rec_ty = Type::Rec {
                name: name.clone(),
                typ: Box::new(typ.clone()),
            };
            tyvar_dict.insert(name.clone(), rec_ty);
            Ok(Type::Unit)
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests_typecheck_rectype {
    use std::vec;

    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_rectype() {
        // object subtype
        let term = parse(
            "
            type NumStream = { num: number, rest: () => NumStream };
            function numbers(n: number): NumStream {
                { num: n, rest: () => numbers(n+1) }
            };

            const nx1 = numbers(1);
            const nx2 = (nx1.rest)();
            const nx3 = (nx2.rest)();
            nx3;
        ",
        );

        assert!(term.is_ok());
        assert_eq!(
            typecheck_with_rectype(&term.unwrap()),
            Ok(
                Type::Object {
                    map: HashMap::from([
                        (String::from("num"), Type::Number),
                        (
                            String::from("rest"),
                            Type::Func {
                                params: vec![],
                                ret: Box::new(Type::Rec {
                                    name: String::from("NumStream"),
                                    typ: Box::new(Type::Object {
                                        map: HashMap::from([
                                            (String::from("num"), Type::Number),
                                            (
                                                String::from("rest"),
                                                Type::Func {
                                                    params: vec![],
                                                    ret: Box::new(Type::TypeVar {
                                                        name: String::from("NumStream")
                                                    })
                                                }
                                            ),
                                        ])
                                    })
                                })
                            }
                        ),
                    ]),
                } // { num: Number, rest: () => (\mu NumStream. {num: Number, rest: () => NumStream }) }
            )
        );
    }
}
