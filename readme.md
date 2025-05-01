## EBNF

```
term = expr , "+" , term
    | expr , white-spaces , "?" , term , white-spaces , ":", term
    | expr;
expr = white-spaces , ( "(" , term , white-spaces , ")"
    | funcall_expr
    | lambda-expr
    | bool
    | number
    | var );
funcall_expr = ( "(" , term , ")" | var ) , "(" , [ term , ( "," , term ) * ] , ")"
lambda-expr = "(" , [ var , ":" , type , ( "," , var , ":" , type)* ] , ")" , '=>' , term
type = atomic-type
    | "(" , [ var , ":" , type , ( "," , var , ":" , type)* ] , ")" , '=>' , type
atomic-type = 'bool' | 'number'
var = name

name = alpha [ alphanumeric ]*
white-spaces = ? space0 ?;
bool = 'true' | 'false';
number = ? i64 ?;
```
