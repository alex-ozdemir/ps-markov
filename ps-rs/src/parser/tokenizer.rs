use std::iter;

#[derive(Debug, PartialEq, Eq)]
pub enum LispToken {
    LParen,
    RParen,
    Ident(String),
}

pub fn tokenize<I: Iterator<Item = char>>(iter: I) -> impl Iterator<Item = LispToken> {
    LispTokenizer::new(iter)
}

pub struct LispTokenizer<I: Iterator<Item = char>> {
    underlying: iter::Peekable<I>,
}

impl<I: Iterator<Item = char>> LispTokenizer<I> {
    fn new(iter: I) -> Self {
        LispTokenizer { underlying: iter.peekable() }
    }
}

enum Move {
    // Return current token (an identifier)
    ReturnIdent,
    // Add next to current token
    Take,
    // Discard next
    Shift,
    // Discard next, return `LParen`
    ShiftReturnLParen,
    // Discard next, return `RParen`
    ShiftReturnRParen,
    // We're all done
    ReturnNone,
}

impl<I: Iterator<Item = char>> Iterator for LispTokenizer<I> {
    type Item = LispToken;

    fn next(&mut self) -> Option<Self::Item> {
        use self::Move::*;
        let mut nxt = String::new();
        loop {
            let next_move = match self.underlying.peek() {
                Some(&'(') if nxt.is_empty() => ShiftReturnLParen,
                Some(&')') if nxt.is_empty() => ShiftReturnRParen,
                Some(&'(') | Some(&')') => ReturnIdent,
                Some(c) if c.is_whitespace() && nxt.is_empty() => Shift,
                Some(c) if c.is_whitespace() => ReturnIdent,
                None if nxt.is_empty() => ReturnNone,
                None => ReturnIdent,
                _ => Take,
            };

            // Pop next character?
            match next_move {
                Take => nxt.push(self.underlying.next().unwrap()),
                ShiftReturnLParen | ShiftReturnRParen | Shift => {
                    self.underlying.next();
                }
                _ => {}
            }

            // Return?
            match next_move {
                ReturnIdent => return Some(LispToken::Ident(nxt)),
                ReturnNone => return None,
                ShiftReturnLParen => return Some(LispToken::LParen),
                ShiftReturnRParen => return Some(LispToken::RParen),
                _ => {}
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.underlying.size_hint()
    }
}

mod tests {

    use super::*;

    // This macro is an assertion with nicely formatted failure output
    macro_rules! assert_expected_eq_actual {
        ($a:expr, $b:expr) => ({
            let (a, b) = (&$a, &$b);
            assert!(*a == *b,
                    "\nExpected `{:?}` is not equal to Actual `{:?}`\
                    \nAssertion: `assert_expected_eq_actual!({}, {})`",
                    *a,
                    *b,
                    stringify!($a),
                    stringify!($b));
        })
    }

    #[test]
    fn empty() {
        let s = String::from("");
        let mut iter = tokenize(s.chars());
        assert_expected_eq_actual!(None, iter.next());
    }

    #[test]
    fn empty_save_whitespace() {
        let s = String::from(" \t \n \r");
        let mut iter = tokenize(s.chars());
        assert_expected_eq_actual!(None, iter.next());
    }

    #[test]
    fn two_idents() {
        let s = String::from("hi there");
        let mut iter = tokenize(s.chars());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("hi"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("there"))), iter.next());
        assert_expected_eq_actual!(None, iter.next());
    }

    #[test]
    fn two_idents_in_parens() {
        let s = String::from("(hi there)");
        let mut iter = tokenize(s.chars());
        assert_expected_eq_actual!(Some(LispToken::LParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("hi"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("there"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::RParen), iter.next());
        assert_expected_eq_actual!(None, iter.next());
    }

    #[test]
    fn two_idents_in_mulitiple_parens() {
        let s = String::from("(hi (there))");
        let mut iter = tokenize(s.chars());
        assert_expected_eq_actual!(Some(LispToken::LParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("hi"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::LParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("there"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::RParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::RParen), iter.next());
        assert_expected_eq_actual!(None, iter.next());
    }

    #[test]
    fn two_idents_in_mulitiple_parens_with_whitespace() {
        let s = String::from("(hi(\tthere \n)\r\n )");
        let mut iter = tokenize(s.chars());
        assert_expected_eq_actual!(Some(LispToken::LParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("hi"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::LParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::Ident(String::from("there"))), iter.next());
        assert_expected_eq_actual!(Some(LispToken::RParen), iter.next());
        assert_expected_eq_actual!(Some(LispToken::RParen), iter.next());
        assert_expected_eq_actual!(None, iter.next());
    }
}
