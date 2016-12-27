mod tokenizer;
mod parser;

use self::tokenizer::{tokenize, LispToken};
use std::fmt;

pub use self::parser::parse;

#[derive(PartialEq, Eq, Clone)]
pub struct SynTree {
    pub head: String,
    pub children: Vec<SynTree>,
}

impl SynTree {
    pub fn new(head: String, children: Vec<SynTree>) -> Self {
        SynTree {
            head: head,
            children: children,
        }
    }

    /// Construct a leaf/terminal `SynTree` from the `String` for it
    pub fn from_ident(ident: String) -> Self {
        SynTree::new(ident, Vec::new())
    }

    fn add_child(&mut self, child: SynTree) {
        self.children.push(child)
    }

    /// Is this node a leaf (a.k.a. a "terminal")?
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    /// Is this node an internal proper-noun node?
    pub fn is_proper(&self) -> bool {
        self.head.as_str() == "NNP" || self.head.as_str() == "NNPS"
    }

    /// Should this node always be capitalized?
    pub fn always_capitalize(&self) -> bool {
        self.head.as_str() == "I"
    }

    /// Iterates over the terminals.
    pub fn terminal_iter<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.into_iter().filter_map(|expr| {
            if expr.is_leaf() {
                Some(expr.head.as_str())
            } else {
                None
            }
        })
    }

    /// Capitalize this like a sentence
    ///
    /// ## Details
    ///
    /// Capitalize the first word in this `SynTree` and any proper nouns. Uncapitalize the rest.
    pub fn fix_caps(&mut self) {
        self.fix_caps_with_force(true)
    }

    /// Changes the capitalization in the SynTree so that the first word is capitalized iff
    /// `force_first` is, proper nouns are, and the rest are not.
    ///
    /// Assumes that all terminals that are part of a proper noun are the sole child of a
    /// proper-noun non-terminal.
    fn fix_caps_with_force(&mut self, mut force_first: bool) {
        if self.is_leaf() {
            if force_first || self.always_capitalize() {
                if self.head.chars().next().unwrap().is_lowercase() {
                    let uppercase = self.head.remove(0).to_uppercase().collect::<String>();
                    self.head.insert_str(0, uppercase.as_str());
                }
            } else {
                if self.head.chars().next().unwrap().is_uppercase() {
                    self.head = self.head.to_lowercase();
                }
            }
        } else {
            let is_proper = self.is_proper();
            for child in &mut self.children {
                child.fix_caps_with_force(force_first || is_proper);
                force_first = false;
            }
        }
    }
}

impl fmt::Debug for SynTree {
    /// Format it as a tree
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.children.len() > 0 {
            try!(write!(f, "({}", self.head));
            for child in &self.children {
                try!(write!(f, " {:?}", child));
            }
            write!(f, ")")
        } else {
            write!(f, "{}", self.head)
        }
    }
}

impl fmt::Display for SynTree {
    /// Format it as a sentance
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn requires_pre_space(terminal: &str) -> bool {
            terminal.chars().next().map(char::is_alphabetic).unwrap_or(false)
        }

        let mut first = true;

        for terminal in self.terminal_iter() {
            if requires_pre_space(terminal) && !first {
                try!(write!(f, " "));
            }
            try!(write!(f, "{}", terminal));
            first = false;
        }
        Ok(())
    }
}

// This is the ideal, but isn't possible right now b/c impl Trait is bad :(
//
// impl<'a> IntoIterator for &'a SynTree {
//     type Item = &'a SynTree;
//     type IntoIter = impl Iterator<Item = &'a SynTree>;
//     fn into_iter(self) -> Self::IntoIter {
//         iter::once(self).chain(self.children.iter().flat_map(|child| child.into_iter()))
//     }
// }

impl<'a> IntoIterator for &'a SynTree {
    type Item = &'a SynTree;
    type IntoIter = PreOrderIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        PreOrderIter { stack: vec![self] }
    }
}

pub struct PreOrderIter<'a> {
    stack: Vec<&'a SynTree>,
}

impl<'a> Iterator for PreOrderIter<'a> {
    type Item = &'a SynTree;
    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().map(|next| {
            self.stack.extend(next.children.iter().rev());
            next
        })
    }
}

impl<'a> IntoIterator for &'a mut SynTree {
    type Item = (&'a mut String, bool);
    type IntoIter = HeadPreOrderIterMut<'a>;
    fn into_iter(self) -> Self::IntoIter {
        HeadPreOrderIterMut { stack: vec![self] }
    }
}

pub struct HeadPreOrderIterMut<'a> {
    stack: Vec<&'a mut SynTree>,
}

impl<'a> Iterator for HeadPreOrderIterMut<'a> {
    //          (Head          , whether it's a leaf)
    type Item = (&'a mut String, bool);
    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().map(|next| {
            let is_leaf = next.is_leaf();
            self.stack.extend(next.children.iter_mut().rev());
            (&mut next.head, is_leaf)
        })
    }
}
