use super::{tokenize, LispToken, SynTree, Tree, convert};
use std::convert;
#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    ExpectedIdentifierAtSynTreeStart,
    MustStartWithOpenParen,
    NoSynTreeToClose,
    ExpectedClosingParen,
}

#[derive(PartialEq,Eq,Hash,Clone,Copy)]
pub enum Tag {
    OQUOTE,
    COMMA,
    COLON,
    DOT,
    CQUOTE,
    DOLLAR,
    ADJP,
    ADVP,
    CC,
    CD,
    CONJP,
    DT,
    EX,
    FRAG,
    FW,
    IN,
    JJ,
    JJR,
    JJS,
    LRB,
    LST,
    MD,
    NN,
    NNP,
    NNPS,
    NNS,
    NP,
    NX,
    PDT,
    POS,
    PP,
    PRN,
    PRP,
    PRT,
    QP,
    RB,
    RBR,
    RBS,
    ROOT,
    RP,
    RRB,
    RRC,
    S,
    SBAR,
    SINV,
    TO,
    UCP,
    VB,
    VBD,
    VBG,
    VBN,
    VBP,
    VBZ,
    VP,
    WDT,
    WHADJP,
    WHADVP,
    WHNP,
    WHPP,
    WP,
    WRB,
    Unknown,
}

impl Tag {
    pub fn from_str(s: &str) -> Self {
        match s {
            "``" => Tag::OQUOTE,
            "," => Tag::COMMA,
            ":" => Tag::COLON,
            "." => Tag::DOT,
            "''" => Tag::CQUOTE,
            "$" => Tag::DOLLAR,
            "ADJP" => Tag::ADJP,
            "ADVP" => Tag::ADVP,
            "CC" => Tag::CC,
            "CD" => Tag::CD,
            "CONJP" => Tag::CONJP,
            "DT" => Tag::DT,
            "EX" => Tag::EX,
            "FRAG" => Tag::FRAG,
            "FW" => Tag::FW,
            "IN" => Tag::IN,
            "JJ" => Tag::JJ,
            "JJR" => Tag::JJR,
            "JJS" => Tag::JJS,
            "-LRB-" => Tag::LRB,
            "LST" => Tag::LST,
            "MD" => Tag::MD,
            "NN" => Tag::NN,
            "NNP" => Tag::NNP,
            "NNPS" => Tag::NNPS,
            "NNS" => Tag::NNS,
            "NP" => Tag::NP,
            "NX" => Tag::NX,
            "PDT" => Tag::PDT,
            "POS" => Tag::POS,
            "PP" => Tag::PP,
            "PRN" => Tag::PRN,
            "PRP" => Tag::PRP,
            "PRP$" => Tag::PRP,
            "PRT" => Tag::PRT,
            "QP" => Tag::QP,
            "RB" => Tag::RB,
            "RBR" => Tag::RBR,
            "RBS" => Tag::RBS,
            "ROOT" => Tag::ROOT,
            "RP" => Tag::RP,
            "-RRB-" => Tag::RRB,
            "RRC" => Tag::RRC,
            "S" => Tag::S,
            "SBAR" => Tag::SBAR,
            "SINV" => Tag::SINV,
            "TO" => Tag::TO,
            "UCP" => Tag::UCP,
            "VB" => Tag::VB,
            "VBD" => Tag::VBD,
            "VBG" => Tag::VBG,
            "VBN" => Tag::VBN,
            "VBP" => Tag::VBP,
            "VBZ" => Tag::VBZ,
            "VP" => Tag::VP,
            "WDT" => Tag::WDT,
            "WHADJP" => Tag::WHADJP,
            "WHADVP" => Tag::WHADVP,
            "WHNP" => Tag::WHNP,
            "WHPP" => Tag::WHPP,
            "WP" => Tag::WP,
            "WP$" => Tag::WP,
            "WRB" => Tag::WRB,
            _ => Tag::Unknown,
        }
    }
}

impl convert::AsRef<str> for Tag {
    fn as_ref(&self) -> &str {
        match self {
            &Tag::OQUOTE => "``",
            &Tag::COMMA => ",",
            &Tag::COLON => ":",
            &Tag::DOT => ".",
            &Tag::CQUOTE => "''",
            &Tag::DOLLAR => "$",
            &Tag::ADJP => "ADJP",
            &Tag::ADVP => "ADVP",
            &Tag::CC => "CC",
            &Tag::CD => "CD",
            &Tag::CONJP => "CONJP",
            &Tag::DT => "DT",
            &Tag::EX => "EX",
            &Tag::FRAG => "FRAG",
            &Tag::FW => "FW",
            &Tag::IN => "IN",
            &Tag::JJ => "JJ",
            &Tag::JJR => "JJR",
            &Tag::JJS => "JJS",
            &Tag::LRB => "-LRB-",
            &Tag::LST => "LST",
            &Tag::MD => "MD",
            &Tag::NN => "NN",
            &Tag::NNP => "NNP",
            &Tag::NNPS => "NNPS",
            &Tag::NNS => "NNS",
            &Tag::NP => "NP",
            &Tag::NX => "NX",
            &Tag::PDT => "PDT",
            &Tag::POS => "POS",
            &Tag::PP => "PP",
            &Tag::PRN => "PRN",
            &Tag::PRP => "PRP",
            &Tag::PRT => "PRT",
            &Tag::QP => "QP",
            &Tag::RB => "RB",
            &Tag::RBR => "RBR",
            &Tag::RBS => "RBS",
            &Tag::ROOT => "ROOT",
            &Tag::RP => "RP",
            &Tag::RRB => "-RRB-",
            &Tag::RRC => "RRC",
            &Tag::S => "S",
            &Tag::SBAR => "SBAR",
            &Tag::SINV => "SINV",
            &Tag::TO => "TO",
            &Tag::UCP => "UCP",
            &Tag::VB => "VB",
            &Tag::VBD => "VBD",
            &Tag::VBG => "VBG",
            &Tag::VBN => "VBN",
            &Tag::VBP => "VBP",
            &Tag::VBZ => "VBZ",
            &Tag::VP => "VP",
            &Tag::WDT => "WDT",
            &Tag::WHADJP => "WHADJP",
            &Tag::WHADVP => "WHADVP",
            &Tag::WHNP => "WHNP",
            &Tag::WHPP => "WHPP",
            &Tag::WP => "WP",
            &Tag::WRB => "WRB",
            &Tag::Unknown => "Unknown",
        }
    }
}

type SynTreeT = Tree<Tag, String>;

pub fn parse<I: Iterator<Item = char>>(iter: I) -> Result<SynTreeT, ParseError> {
    parse_internal(iter).map(|mut tree| {
        tree.fix_terminals();
        convert(tree)
    })
}

pub fn parse_internal<I: Iterator<Item = char>>(iter: I) -> Result<SynTree, ParseError> {
    parse_tokens(tokenize(iter))
}

fn parse_tokens<I: Iterator<Item = LispToken>>(iter: I) -> Result<SynTree, ParseError> {

    // The SynTree's we're currently constructing
    let mut stack: Vec<SynTree> = vec![SynTree::from_ident(String::from("DUMMY"))];

    // We've just seen a '(' and are starting a new SynTree (expecting an identifier)
    let mut starting_new_expr = false;

    let mut peek_iter = iter.peekable();
    if peek_iter.peek() != Some(&LispToken::LParen) {
        return Err(ParseError::MustStartWithOpenParen);
    }
    for token in peek_iter {
        match (token, starting_new_expr) {
            (LispToken::LParen, true) => return Err(ParseError::ExpectedIdentifierAtSynTreeStart),
            (LispToken::LParen, false) => {
                starting_new_expr = true;
            }
            (LispToken::RParen, true) => return Err(ParseError::ExpectedIdentifierAtSynTreeStart),
            (LispToken::RParen, false) => {
                try!(stack.pop().and_then(|child| {
                    stack.last_mut().map(|parent_mut| {
                        parent_mut.add_child(child)
                    })
                }).ok_or(ParseError::NoSynTreeToClose))
            }
            (LispToken::Ident(ident), true) => {
                stack.push(SynTree::from_ident(ident));
                starting_new_expr = false;
            }
            (LispToken::Ident(ident), false) => {
                stack.last_mut().expect("Programmer bug").add_child(SynTree::from_ident(ident))
            }
        }
    }
    if stack.len() == 1 {
        Ok(stack.pop().expect("Programmer bug").children.pop().expect("Programmer bug"))
    } else {
        Err(ParseError::ExpectedClosingParen)
    }
}

#[cfg(test)]
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
    fn just_one() {
        let s = String::from("(hi)");
        assert_expected_eq_actual!(Ok(SynTree::from_ident(String::from("hi"))),
                                   parse(s.chars()));
    }

    #[test]
    fn no_opening() {
        let s = String::from("hi");
        assert_expected_eq_actual!(Err(ParseError::MustStartWithOpenParen), parse(s.chars()));
    }

    #[test]
    fn two_in_one_list() {
        let s = String::from("(hi there)");
        let inner = SynTree::from_ident(String::from("there"));
        let expected = SynTree {
            head: String::from("hi"),
            children: vec![inner],
        };
        assert_expected_eq_actual!(Ok(expected), parse(s.chars()));
    }

    #[test]
    fn nested() {
        let s = String::from("(hi (there you))");
        let mut inner = SynTree::from_ident(String::from("there"));
        inner.add_child(SynTree::from_ident(String::from("you")));
        let expected = SynTree {
            head: String::from("hi"),
            children: vec![inner],
        };
        assert_expected_eq_actual!(Ok(expected), parse(s.chars()));
    }

    #[test]
    fn unclosed() {
        let s = String::from("(hi (there you)");
        assert_expected_eq_actual!(Err(ParseError::ExpectedClosingParen), parse(s.chars()));
    }

    #[test]
    fn s_expr_doesnt_start_with_id() {
        let s = String::from("((hi) (there you))");
        assert_expected_eq_actual!(Err(ParseError::ExpectedIdentifierAtSynTreeStart),
                                   parse(s.chars()));
    }

    #[test]
    fn extra_closing_paren() {
        let s = String::from("(hi (there you)))");
        assert_expected_eq_actual!(Err(ParseError::NoSynTreeToClose), parse(s.chars()));
    }

}
