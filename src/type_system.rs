use crate::parser::{Term, Type};

pub fn typecheck(t: &Term) -> Result<Type, String> {
    match t {
        Term::False | Term::True => Ok(Type::Boolean),
        Term::Number(_) => Ok(Type::Number),
        Term::Add(lhs, rhs) => {
            let ltype = typecheck(lhs)?;
            let rtype = typecheck(rhs)?;
            if ltype != Type::Number || rtype != Type::Number {
                return Err(String::from("number expected"));
            }

            Ok(Type::Number)
        }
        Term::If(cond, then, els) => {
            let cond_type = typecheck(cond)?;
            let then_type = typecheck(then)?;
            let els_type = typecheck(els)?;

            if cond_type != Type::Boolean {
                return Err(String::from("bool expected"));
            }
            if then_type != els_type {
                return Err(String::from("then and else have different types"));
            }

            Ok(then_type)
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests_typecheck {
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
}
