# SAP Grammar

The order of statements is representative of the hierarchy of the AST.

## Foundation

    <program> -> <statements> `Eof`

    <block> -> `LCurly` <statements> `RCurly


## Statements

    <statement> -> <let_stmt> | <return_stmt> | <expression>

    <let_stmt> -> `Let` `Ident` `Assign` <expression>

    <return_stmt> -> `Return` <expression>

## Expressions

    <expression> -> <bool_expr>

    Infix operations

        <bool_expr> -> <eq_expr> ((`And` | `Or`) <eq_expr>)*

        <eq_expr> -> <comp_expr> ((`Eq`|`NotEq`) <comp_expr>)*

        <comp_expr> -> <sum_expr> ((`Less`|`LessEq`|`More`|`MoreEq`) <sum_expr>)*

        <sum_expr> -> <product_expr> ((`Plus` | `Minus`) <product_expr>)*

        <product_expr> -> <postfix_expr> ((`Mult` | `Div` | `Mod`) <postfix_expr>)*

    Postfix operations

        <postfix_expr> -> <prefix_expr> (<fn_call> | <array_index>)*

    Prefix operations

        <prefix_expr> -> (`Not`|`Minus`)* <ident_expr>

    Mono operations (not really operating on anything tho, could be an entity?)
        <ident_expr> -> `Ident` | <group_expr>

        <group_expr> -> `LParen` <expression> `RParen`

    Entities
        <entity_expr> -> <selection_expr>
                    | <fn_decl_expr>
                    | <array_expr>
                    | <literal>

        <selection_expr> -> `If` <expr> <block> `Else` <block>

        <fn_decl_expr> -> `Fn` `LParen` <fn_params> `RParen` <block>

        <array_expr> -> `LBracket` <expr_list>? `LBracket`

## Literals 

    <literal> -> `Int`
            | `Float`
            | `Bool`
            | `String`

## Aliases

    <statements> -> (<statement> `Semi`)* | <statement>

    <array_index> -> `LBracket` <expression> `RBracket`

    <fn_call> -> `LParen` <expr_list>? `RParen`

    <expr_list> -> <expression> (`,` <expression>)*

