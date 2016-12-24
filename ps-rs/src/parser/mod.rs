mod tokenizer;

use self::tokenizer::{tokenize,LispToken};

#[derive(Debug, PartialEq, Eq)]
pub struct SExpr {
    pub head: String,
    pub children: Vec<SExpr>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    ExpectedIdentifierAtSExprStart,
    MustStartWithOpenParen,
    NoSExprToClose,
    ExpectedClosingParen,
}

impl SExpr {
    fn from_ident(ident: String) -> Self {
        SExpr { head: ident, children: Vec::new() }
    }
    fn add_child(&mut self, child: SExpr) {
        self.children.push(child)
    }
}

pub fn parse<I: Iterator<Item = char>>(iter: I) -> Result<SExpr,ParseError> {
    parse_tokens(tokenize(iter))
}

fn parse_tokens<I: Iterator<Item = LispToken>>(iter: I) -> Result<SExpr,ParseError> {

    // The SExpr's we're currently constructing
    let mut stack: Vec<SExpr> = vec![SExpr::from_ident(String::from("DUMMY"))];

    // We've just seen a '(' and are starting a new SExpr (expecting an identifier)
    let mut starting_new_expr = false;

    let mut peek_iter = iter.peekable();
    if peek_iter.peek() != Some(&LispToken::LParen) {
        return Err(ParseError::MustStartWithOpenParen);
    }
    for token in peek_iter {
        match (token, starting_new_expr) {
            (LispToken::LParen, true) => return Err(ParseError::ExpectedIdentifierAtSExprStart),
            (LispToken::LParen, false) => {
                starting_new_expr = true;
            },
            (LispToken::RParen, true) => return Err(ParseError::ExpectedIdentifierAtSExprStart),
            (LispToken::RParen, false) => {
                try!(stack.pop().and_then(|child| {
                    stack.last_mut().map(|parent_mut| {
                        parent_mut.add_child(child)
                    })
                }).ok_or(ParseError::NoSExprToClose))
            },
            (LispToken::Ident(ident), true) => {
                stack.push(SExpr::from_ident(ident));
                starting_new_expr = false;
            },
            (LispToken::Ident(ident), false) => {
                stack.last_mut().expect("Programmer bug").add_child(SExpr::from_ident(ident))
            },
        }
    }
    if stack.len() == 1 {
        Ok(stack.pop().expect("Programmer bug").children.pop().expect("Programmer bug"))
    } else {
        Err(ParseError::ExpectedClosingParen)
    }
}

mod test {
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
    fn just_one() {
        let s = String::from("(hi)");
        assert_expected_eq_actual!(Ok(SExpr::from_ident(String::from("hi"))), parse(s.chars()));
    }

    #[test]
    fn no_opening() {
        let s = String::from("hi");
        assert_expected_eq_actual!(Err(ParseError::MustStartWithOpenParen), parse(s.chars()));
    }

    #[test]
    fn two_in_one_list() {
        let s = String::from("(hi there)");
        let inner = SExpr::from_ident(String::from("there"));
        let expected = SExpr{ head: String::from("hi"), children: vec![inner] };
        assert_expected_eq_actual!(Ok(expected), parse(s.chars()));
    }

    #[test]
    fn nested() {
        let s = String::from("(hi (there you))");
        let mut inner = SExpr::from_ident(String::from("there"));
        inner.add_child(SExpr::from_ident(String::from("you")));
        let expected = SExpr{ head: String::from("hi"), children: vec![inner] };
        assert_expected_eq_actual!(Ok(expected), parse(s.chars()));
    }

}
