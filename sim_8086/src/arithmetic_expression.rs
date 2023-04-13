use std::ops::{Mul, Add, Sub};

#[derive(PartialEq, Eq)]
enum Op {
    Add,
    Times,
}

enum BinaryExpr<T> {
    Const(T),
    /// Indices into the arena.
    Binary(Op, usize, usize),
}

pub(crate) struct ArithmeticExpression<T> {
    /// In reverse Polish notation.
    arena: Vec<BinaryExpr<T>>,
}

#[derive(Copy, Clone)]
pub(crate) enum Token<T> {
    Literal(T),
    Add,
    Times,
}

pub(crate) trait HasMax {
    const MAX: Self;
}

impl HasMax for u16 {
    const MAX: u16 = u16::MAX;
}

impl HasMax for u8 {
    const MAX: u8 = u8::MAX;
}

impl<T> ArithmeticExpression<T> where T: Copy {
    fn eval_index(&self, index: usize) -> Result<T, ()> where T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + HasMax + PartialOrd, u64: From<T> {
        match &self.arena[index] {
            BinaryExpr::Const(x) => Ok(*x),
            BinaryExpr::Binary(op, left, right) => {
                let left = self.eval_index(*left)?;
                let right = self.eval_index(*right)?;
                match op {
                    Op::Add => {
                        if left < T::MAX - right {
                            Ok(left + right)
                        } else {
                            Err(())
                        }
                    }
                    Op::Times => {
                        let x = u64::from(left);
                        let y = u64::from(right);
                        if x * y < u64::from(T::MAX) {
                            Ok(left * right)
                        } else {
                            Err(())
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn eval(&self) -> Result<T, ()> where T: Add<Output = T> + Mul<Output = T> + HasMax + PartialOrd + Sub<Output = T>, u64: From<T> {
        self.eval_index(self.arena.len() - 1)
    }

    pub(crate) fn of_tokens<Iter: AsRef<[Token<T>]>>(t: Iter) -> ArithmeticExpression<T> {
        let mut arena = vec![];
        let mut outstanding_ops = vec![];
        let mut outstanding_exprs = vec![];
        for token in t.as_ref() {
            match token {
                Token::Literal(l) => {
                    arena.push(BinaryExpr::Const(*l));
                    let to_add = match outstanding_ops.pop() {
                        None => arena.len() - 1,
                        Some(Op::Times) => match outstanding_exprs.pop() {
                            None => {
                                panic!("Bad token stream, had Times operation with no left operand")
                            }
                            Some(left_operand) => {
                                let right = arena.len() - 1;

                                arena.push(BinaryExpr::Binary(Op::Times, left_operand, right));
                                arena.len() - 1
                            }
                        },
                        Some(Op::Add) => {
                            outstanding_ops.push(Op::Add);
                            arena.len() - 1
                        }
                    };
                    outstanding_exprs.push(to_add);
                }
                Token::Add => outstanding_ops.push(Op::Add),
                Token::Times => outstanding_ops.push(Op::Times),
            }
        }

        while let Some(op) = outstanding_ops.pop() {
            assert!(op == Op::Add);
            let left = outstanding_exprs.pop().expect("needed a right operand");
            let right = outstanding_exprs.pop().expect("needed a left operand");
            arena.push(BinaryExpr::Binary(Op::Add, left, right));
            outstanding_exprs.push(arena.len() - 1);
        }
        if !outstanding_ops.is_empty() {
            panic!("Imbalanced expression");
        }
        if outstanding_exprs.len() > 1 {
            panic!("Not enough operations");
        }
        if outstanding_exprs.is_empty() {
            panic!("Empty expression?!");
        }

        ArithmeticExpression { arena }
    }
}

#[cfg(test)]
mod test_arithmetic_expression {
    use super::{ArithmeticExpression, Token};

    #[test]
    fn test_expr() {
        let tokens = vec![
            Token::Literal(10),
            Token::Add,
            Token::Literal(33),
            Token::Times,
            Token::Literal(12),
        ];

        assert_eq!(
            ArithmeticExpression::of_tokens(tokens).eval().unwrap(),
            10u16 + (33 * 12)
        );

        let tokens = vec![
            Token::Literal(10),
            Token::Times,
            Token::Literal(33),
            Token::Add,
            Token::Literal(12),
        ];

        assert_eq!(
            ArithmeticExpression::of_tokens(tokens).eval().unwrap(),
            (10u16 * 33) + 12
        );
    }
}
