# baka: a sussy programming language

## example code

```
include std;

foo :: (bar: int) -> int {
    x := std.random.uniform();
    if (x > 0.5) {
        bar = bar + bar;
    }
    bar *= bar;
    return bar;
}

main :: (argc: int, argv: const u8[]) -> void {
    
}
```

## grammar

```
Root:
    Declaration+

Declaration:
    ( VarDecl
    | AutoVarDecl
    | ConstDecl
    | AutoConstDecl
    | FuncDecl
    | StructDecl
    | UnionDecl )

Expr: Equality

Equality: Comparison ( ( COND_NEQ | COND_EQ ) Comparison )*

Comparison: Term ( ( COND_LT | COND_LEQ | COND_GT | COND_GEQ ) Term )*

Term: Factor ( ( MINUS | PLUS ) Factor )*

Factor: Unary ( ( FWD_SLASH | STAR | AT ) Unary )*

Unary: ( ( COND_NOT | MINUS ) Unary | Primary )

Primary:
    ( Literal
    | Identifier
    | TRUE
    | FALSE
    | NULL
    | PAREN_L Expr PAREN_R )

Statement:
    ( AssignExpr | Expr ) SEMI

AssignExpr:
    Identifier EQUALS Expr

WhileStatement:
    "while" PAREN_L Expr PAREN_R ( COLON PAREN_L AssignExpr PAREN_R )? Block

ForStatement:
    "for" PAREN_L Expr PAREN_R ( PIPE Identifier PIPE )? Block

IfStatement:
    "if" PAREN_L Expr PAREN_R Block ( ( "else" Block ) | ( "else" | IfStatement)+ )?

SwitchBlock:
    BRACE_L ( SwitchProng COMMA )* SwitchProng COMMA? BRACE_R

SwitchProng:
    ( ( Identifier | Literal | "else" ) | ( ( Identifier | Literal | "else" ) COMMA )+ )
    BIG_ARROW ( Statement | Block )

SwitchStatement:
    "switch" PAREN_L Expr PAREN_R SwitchBlock

BlockStatement:
    ( WhileStatement | ForStatement | IfStatement | SwitchStatement )

Block:
    ( Declaration | Statement | BlockStatement )*

VarDecl:
    Identifier COLON Identifier (EQUALS Expr)? SEMI

AutoVarDecl:
    Identifier COLON_EQUALS Expr SEMI

ConstDecl:
    Identifier COLON Identifier COLON Expr SEMI

AutoConstDecl:
    Identifier DOUBLE_COLON Expr SEMI

FuncSpecs:
    INLINE | EXTERN

ParamList: ( 
    ParamDecl? | (ParamDecl COMMA)+ ParamDecl
) 

ParamDecl:
    Identifier COLON Identifier (EQUALS Expr)?

FuncDecl:
    Identifier DOUBLE_COLON FuncSpecs* PAREN_L ParamList? PAREN_R ARROW Identifier Block


```
