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

/**
 * typecheck for chapter8(recursive type)
 */

#[allow(dead_code)]
pub fn typecheck_with_rectype(t: &Term) -> Result<Type, String> {
    let mut dict = HashMap::new();
    let mut tyvar_dict = HashMap::new();
    typecheck_with_rectype_impl(t, &mut dict, &mut tyvar_dict)
}

fn expand_type(ty: &Type, tyVarName: &str, repTy: &Type) -> Type {
    match ty {
        Type::Func { params, ret } => Type::Func {
            params: params
                .into_iter()
                .map(|(name, typ)| (name.clone(), expand_type(typ, tyVarName, repTy)))
                .collect(),
            ret: Box::new(expand_type(&ret, tyVarName, repTy)),
        },
        Type::Object { map } => Type::Object {
            map: map
                .into_iter()
                .map(|(name, prop)| (name.clone(), expand_type(prop, tyVarName, repTy)))
                .collect(),
        },
        Type::Rec { name, typ } => {
            if name == tyVarName {
                return ty.clone();
            }
            let new_type = expand_type(typ, tyVarName, repTy);
            Type::Rec {
                name: name.clone(),
                typ: Box::new(new_type),
            }
        }
        Type::TypeVar { name } => {
            if name == tyVarName {
                repTy.clone()
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
