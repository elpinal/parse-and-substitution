use std::vec::IntoIter;

enum Token {
    Lambda,
    Dot,
    Ident(String),
}

fn ident(s: &str) -> Token {
    Token::Ident(s.to_string())
}

#[derive(Clone, Debug, PartialEq)]
enum Term {
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
    Var(usize, String),
}

fn abs(t: Term) -> Term {
    Term::Abs(Box::new(t))
}

fn app(t1: Term, t2: Term) -> Term {
    Term::App(Box::new(t1), Box::new(t2))
}

fn var(n: usize, s: &str) -> Term {
    Term::Var(n, s.to_string())
}

fn parse(v: Vec<Token>) -> Option<Term> {
    Parser {
        ts: v.into_iter(),
        ctx: Vec::new(),
    }.parse1()
}

struct Parser {
    ts: IntoIter<Token>,
    ctx: Vec<String>,
}

impl Parser {
    fn parse1(&mut self) -> Option<Term> {
        use self::Token::*;
        match self.ts.next()? {
            Lambda => self.parse_abs(),
            Ident(s) => {
                let t = self.string_to_var(s)?;
                self.parse_app(t)
            }
            _ => None,
        }
    }

    fn parse_app(&mut self, t: Term) -> Option<Term> {
        use self::Token::*;
        match self.ts.next() {
            None => Some(t),
            Some(Ident(s)) => {
                let x = app(t, self.string_to_var(s)?);
                self.parse_app(x)
            }
            Some(Lambda) => {
                let x = app(t, self.parse_abs()?);
                self.parse_app(x)
            }
            _ => None,
        }
    }

    fn string_to_var(&mut self, s: String) -> Option<Term> {
        Some(Term::Var(self.ctx.iter().rev().position(|x| x == &s)?, s))
    }

    fn parse_abs(&mut self) -> Option<Term> {
        use self::Token::*;
        match self.ts.next()? {
            Ident(s) => match self.ts.next()? {
                Dot => {
                    self.ctx.push(s);
                    self.parse1().map(|t| abs(t))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl Term {
    fn map<F>(&mut self, f: &F, c: usize)
    where
        F: Fn(usize, usize, String, &mut Term),
    {
        use self::Term::*;
        match self {
            &mut Var(n, _) => {
                let s = match self {
                    &mut Var(_, ref s) => s.clone(),
                    _ => unreachable!(),
                };
                f(c, n, s, self)
            }
            &mut Abs(ref mut t) => t.map(f, c + 1),
            &mut App(ref mut t1, ref mut t2) => {
                t1.map(f, c + 1);
                t2.map(f, c + 1);
            }
        }
    }

    fn shift_above(&mut self, c: usize, d: isize) {
        let f = |c, n, s: String, t: &mut Term| {
            if c <= n {
                *t = Term::Var((n as isize + d) as usize, s);
            }
        };
        self.map(&f, c);
    }

    /// Shifts free variables by `d`.
    fn shift(&mut self, d: isize) {
        self.shift_above(0, d);
    }

    fn subst(&mut self, j: usize, t: Term) {
        let f = |c, n, _, t0: &mut Term| {
            if c + j == n {
                let mut t = t.clone();
                t.shift(c as isize);
                *t0 = t;
            }
        };
        self.map(&f, 0);
    }

    /// Substitutes `t` for the variables `0` in `self`, assuming `t` is under a context whose
    /// length is one more than `self`'s context.
    fn subst_top(&mut self, mut t: Term) {
        t.shift(1);
        self.subst(0, t);
        self.shift(-1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        use self::Token::*;
        let ts = vec![
            Lambda,
            ident("x"),
            Dot,
            ident("x"),
            Lambda,
            ident("y"),
            Dot,
            ident("y"),
            ident("x"),
        ];
        assert_eq!(
            parse(ts),
            Some(abs(app(var(0, "x"), abs(app(var(0, "y"), var(1, "x"))))))
        );

        let ts = vec![Lambda, ident("x"), Dot, ident("x"), ident("x"), ident("x")];
        assert_eq!(
            parse(ts),
            Some(abs(app(app(var(0, "x"), var(0, "x")), var(0, "x"))))
        );
    }

    #[test]
    fn test_shift_closed_term() {
        let mut t = abs(app(var(0, "x"), abs(app(var(0, "y"), var(1, "x")))));
        let t0 = t.clone();
        t.shift(0);
        assert_eq!(t, t0);

        t.shift(1);
        assert_eq!(t, t0);

        let mut t = abs(app(app(var(0, "x"), var(0, "x")), var(0, "x")));
        let t0 = t.clone();
        t.shift(10);
        assert_eq!(t, t0);
    }

    #[test]
    fn test_shift() {
        let mut t = var(0, "x");
        let t0 = t.clone();
        t.shift(0);
        assert_eq!(t, t0);

        t.shift(1);
        assert_eq!(t, var(1, "x"));
    }
}
