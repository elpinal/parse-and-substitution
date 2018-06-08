use std::vec::IntoIter;

enum Token {
    Lambda,
    Dot,
    Ident(String),
}

fn ident(s: &str) -> Token {
    Token::Ident(s.to_string())
}

#[derive(Debug, PartialEq)]
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
}
