# SAP Grammar

The order of statements is representative of the hierarchy of the AST.

## Foundation

    <program> -> <statements>? `Eof`

    <block> -> `LCurly` <statements>? `RCurly`


## Statements

    <statement> -> <set_stmt> | <return_stmt> | <expression> | <fn_decl_stmt>

    <set_stmt> -> `Set` `Ident` `Assign` <expression>

    <return_stmt> -> `Return` <expression>

    <fn_decl_stmt> -> `defineFunction` `LParen` <fn_params> `RParen` <statements>? `End`

## Expressions

    <expression> -> <or_expr>

    Infix operations

        <or_expr> -> <and_expr> (`Or` <and_expr>)*

        <and_expr> -> <eq_expr> (`And` <eq_expr>)*

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

        <group_expr> -> (`LParen` <expression> `RParen`) | <entity_expr>

    Entities
        <entity_expr> -> <selection_expr>
                    | <array_expr>
                    | <literal>

        <selection_expr> -> `If` <expr> `Then` <statements>? (`Otherwise` <statements>?)? `End`

        <array_expr> -> `LBracket` <expr_list>? `LBracket`

## Literals 

    <literal> -> `Int`
            | `Float`
            | `Bool`
            | `String`

## Aliases

    <statements> -> <statement> ((`Semi`|`NewLine`) <statement>)* (`Semi`|`NewLine`)?

    <array_index> -> `LBracket` <expression> `RBracket`

    <fn_call> -> `LParen` <expr_list>? `RParen`

    <expr_list> -> <expression> (`,` <expression>)*

