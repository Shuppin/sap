#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // < >
    Sum,         // + -
    Product,     // * /
    Prefix,      // -x or !x
    Call,        // print(x)
    Index,       // nums[x]
}
