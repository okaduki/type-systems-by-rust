[![Rust](https://github.com/okaduki/type-systems-by-rust/actions/workflows/rust.yml/badge.svg)](https://github.com/okaduki/type-systems-by-rust/actions/workflows/rust.yml)

## Description

[型システムのしくみ ― TypeScriptで実装しながら学ぶ型とプログラミング言語](https://www.lambdanote.com/products/type-systems) をRust で実装したもの

## Note

- 本文中で使われる疑似TypeScript で、例をカバーできる簡単な範囲でパースするように制限

## EBNF

```
statements = ( statement , ";" )* , statement , ";"?
statement = term
    | 'const' , white-spaces , name , "=" , term
    | 'type' , white-spaces , name , "=" , type;
    | 'function' , white-spaces , name , params-expr , ":" , type , "{" term "}"
term = (expr , "+" , term
    | expr , "?" , term , ":", term
    | expr , generics-expr ? , funcall-expr *
    | expr) , ( "." , name )*;
expr = ( "(" , term , ")"
    | "{" , name , ":" , term , ( "," , name , ":" , term)* "}"
    | generics-expr ? , lambda-expr
    | bool
    | number
    | var );

funcall-expr = "(" , [ term , ( "," , term ) * ] , ")"
generics-expr = "<" , name , ( "," , name )* , ">"
lambda-expr = params-expr , '=>' , term
params-expr =  "(" , [ name , ":" , type , ( "," , name , ":" , type)* ] , ")"
type = atomic-type
    | name
    | generics-expr ? , "(" , [ name , ":" , type , ( "," , name , ":" , type)* ] , ")" , '=>' , type
    | "{" , [ name , ":" , type , ( "," , name , ":" , type)* ] , "}";
atomic-type = 'bool' | 'number'
var = name

name = alpha [ alphanumeric ]*
white-spaces = ? space1 ?;
bool = 'true' | 'false';
number = ? i64 ?;
```
