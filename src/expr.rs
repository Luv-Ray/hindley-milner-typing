pub enum Expr {
    Var(Variable),
    Lit(Literal),
    Let(Variable, Box<Expr>),
    /// Lam(Expr)
    App(Box<Expr>, Box<Expr>),
    /// Lam alpha.beta
    Lam(Box<Expr>, Box<Expr>),
}

pub struct Variable {
    pub id: usize,
}

pub enum Literal {
    Int(i32),
    Bool(bool),
    Str(String),
}

impl Expr {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Into<Literal> for i32 {
    fn into(self) -> Literal {
        Literal::Int(self)
    }
}

impl Into<Literal> for bool {
    fn into(self) -> Literal {
        Literal::Bool(self)
    }
}

impl Into<Literal> for String {
    fn into(self) -> Literal {
        Literal::Str(self)
    }
}
