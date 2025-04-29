## EBNF

```
term = expr , "+" , term
    | expr , white-spaces , "?" , term , white-spaces , ":", term
    | expr;
expr = white-spaces , ( "(" , term , white-spaces , ")"
    | bool
    | number );

white-spaces = ? space0 ?;
bool = 'true' | 'false';
number = ? i64 ?;
```