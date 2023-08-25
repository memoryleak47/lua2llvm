use crate::ast::*;

use std::any::Any;

pub trait AsAny {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Any> AsAny for T {
    fn as_any(&self) -> &dyn Any { self }
    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

pub trait Visitable: AsAny {
    fn children(&self) -> Vec<&dyn Visitable>;
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable>;
}

impl Visitable for Ast {
    fn children(&self) -> Vec<&dyn Visitable> {
        vec![&self.statements]
    }

    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        vec![&mut self.statements]
    }
}

impl Visitable for Statement {
    fn children(&self) -> Vec<&dyn Visitable> {
        use Statement::*;
        match self {
            Assign(d0, d1) => vec![d0, d1],
            FunctionCall(d0) => vec![d0],
            While(d0, d1) => vec![d0, d1],
            Repeat(d0, d1) => vec![d0, d1],
            NumericFor(d0, d1, d2, d3, d4) => vec![d0, d1, d2, d3, d4],
            GenericFor(d0, d1, d2) => vec![d0, d1, d2],
            If(d0, d1) => vec![d0, d1],
            Local(d0, d1) => vec![d0, d1],
            Return(d0) => vec![d0],
            Block(d0) => vec![d0],
            Break => vec![]
        }
    }

    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        use Statement::*;
        match self {
            Assign(d0, d1) => vec![d0, d1],
            FunctionCall(d0) => vec![d0],
            While(d0, d1) => vec![d0, d1],
            Repeat(d0, d1) => vec![d0, d1],
            NumericFor(d0, d1, d2, d3, d4) => vec![d0, d1, d2, d3, d4],
            GenericFor(d0, d1, d2) => vec![d0, d1, d2],
            If(d0, d1) => vec![d0, d1],
            Local(d0, d1) => vec![d0, d1],
            Return(d0) => vec![d0],
            Block(d0) => vec![d0],
            Break => vec![]
        }
    }
}

impl Visitable for Expr {
    fn children(&self) -> Vec<&dyn Visitable> {
        use Expr::*;
        match self {
            Ellipsis => vec![],
            Literal(d0) => vec![d0],
            LValue(d0) => vec![d0],
            BinOp(d0, d1, d2) => vec![d0, d1, d2],
            UnOp(d0, d1) => vec![d0, d1],
            FunctionCall(d0) => vec![d0],
        }
    }

    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        use Expr::*;
        match self {
            Ellipsis => vec![],
            Literal(d0) => vec![d0],
            LValue(d0) => vec![d0],
            BinOp(d0, d1, d2) => vec![d0, d1, d2],
            UnOp(d0, d1) => vec![d0, d1],
            FunctionCall(d0) => vec![d0],
        }
    }
}

impl Visitable for LValue {
    fn children(&self) -> Vec<&dyn Visitable> {
        use LValue::*;
        match self {
            Var(d0) => vec![d0],
            Dot(d0, d1) => vec![d0, d1],
            Index(d0, d1) => vec![d0, d1],
        }
    }

    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        use LValue::*;
        match self {
            Var(d0) => vec![d0],
            Dot(d0, d1) => vec![d0, d1],
            Index(d0, d1) => vec![d0, d1],
        }
    }
}



impl Visitable for FunctionCall {
    fn children(&self) -> Vec<&dyn Visitable> {
        use FunctionCall::*;
        match self {
            Direct(d0, d1) => vec![d0, d1],
            Colon(d0, d1, d2) => vec![d0, d1, d2],
        }
    }

    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        use FunctionCall::*;
        match self {
            Direct(d0, d1) => vec![d0, d1],
            Colon(d0, d1, d2) => vec![d0, d1, d2],
        }
    }
}

impl Visitable for IfBlock {
    fn children(&self) -> Vec<&dyn Visitable> {
        vec![&self.0, &self.1]
    }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        vec![&mut self.0, &mut self.1]
    }
}

impl Visitable for Literal {
    fn children(&self) -> Vec<&dyn Visitable> {
        use Literal::*;
        match self {
            Num(d0) => vec![d0],
            Str(d0) => vec![d0],
            Bool(d0) => vec![d0],
            Function(d0, d1, d2) => vec![d0, d1, d2],
            Table(d0) => vec![d0],
            Nil => vec![],
        }
    }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        use Literal::*;
        match self {
            Num(d0) => vec![d0],
            Str(d0) => vec![d0],
            Bool(d0) => vec![d0],
            Function(d0, d1, d2) => vec![d0, d1, d2],
            Table(d0) => vec![d0],
            Nil => vec![],
        }
    }
}

impl Visitable for Field {
    fn children(&self) -> Vec<&dyn Visitable> {
        use Field::*;
        match self {
            Expr(d0) => vec![d0],
            ExprToExpr(d0, d1) => vec![d0, d1],
            NameToExpr(d0, d1) => vec![d0, d1],
        }
    }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        use Field::*;
        match self {
            Expr(d0) => vec![d0],
            ExprToExpr(d0, d1) => vec![d0, d1],
            NameToExpr(d0, d1) => vec![d0, d1],
        }
    }
}



impl Visitable for UnOpKind {
    fn children(&self) -> Vec<&dyn Visitable> { vec![] }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> { vec![] }
}

impl Visitable for BinOpKind {
    fn children(&self) -> Vec<&dyn Visitable> { vec![] }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> { vec![] }
}

impl Visitable for Variadic {
    fn children(&self) -> Vec<&dyn Visitable> { vec![] }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> { vec![] }
}

impl Visitable for String {
    fn children(&self) -> Vec<&dyn Visitable> { vec![] }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> { vec![] }
}

impl Visitable for f64 {
    fn children(&self) -> Vec<&dyn Visitable> { vec![] }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> { vec![] }
}

impl Visitable for bool {
    fn children(&self) -> Vec<&dyn Visitable> { vec![] }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> { vec![] }
}

impl<T: Visitable + 'static> Visitable for Vec<T> {
    fn children(&self) -> Vec<&dyn Visitable> {
        self.iter().map(|x| x as &dyn Visitable).collect()
    }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        self.iter_mut().map(|x| x as &mut dyn Visitable).collect()
    }
}

impl<T: Visitable + 'static> Visitable for Option<T> {
    fn children(&self) -> Vec<&dyn Visitable> {
        self.iter().map(|x| x as &dyn Visitable).collect()
    }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        self.iter_mut().map(|x| x as &mut dyn Visitable).collect()
    }
}

impl<T: Visitable + 'static> Visitable for Box<T> {
    fn children(&self) -> Vec<&dyn Visitable> {
        vec![&**self as &dyn Visitable]
    }
    fn children_mut(&mut self) -> Vec<&mut dyn Visitable> {
        vec![&mut **self as &mut dyn Visitable]
    }
}
