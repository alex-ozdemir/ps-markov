mod tokenizer;
mod parser;

use self::tokenizer::{tokenize, LispToken};
use std::fmt;
use std::slice;
use std::sync::Arc;

pub use self::parser::{parse, Tag};

/// A tree which stores different types at leaves and internal nodes
#[derive(PartialEq, Eq, Clone)]
pub enum Tree<I, L> {
    /// An inner node's data and children
    InnerNode(I, Vec<Tree<I, L>>),
    /// A leaf's data
    Leaf(L),
}

/// Data stored at some tree node.
#[derive(PartialEq, Eq, Hash, Clone)]
pub enum TreeData<I, L> {
    InnerNodeData(I),
    LeafData(L),
}

/// A reference to data stored at some tree node.
#[derive(PartialEq, Eq, Hash)]
pub enum TreeDataRef<'a, I: 'a, L: 'a> {
    InnerNodeData(&'a I),
    LeafData(&'a L),
}

/// We implement `Clone` and `Copy` by hand so that the bounds are correct.
impl<'a, I, L> Clone for TreeDataRef<'a, I, L> {
    fn clone(&self) -> Self {
        match self {
            &TreeDataRef::InnerNodeData(data) => TreeDataRef::InnerNodeData(data),
            &TreeDataRef::LeafData(data) => TreeDataRef::LeafData(data),
        }
    }
}

impl<'a, I, L> Copy for TreeDataRef<'a, I, L> {}


impl<'a, I, L> TreeDataRef<'a, I, L> {
    pub fn map_inner<J, F: FnOnce(&'a I) -> &'a J>(self, f: F) -> TreeDataRef<'a, J, L> {
        match self {
            TreeDataRef::InnerNodeData(data) => TreeDataRef::InnerNodeData(f(data)),
            TreeDataRef::LeafData(data) => TreeDataRef::LeafData(data),
        }
    }
}

impl<I, L> TreeData<I, L> {
    pub fn map_inner<J, F: FnOnce(I) -> J>(self, f: F) -> TreeData<J, L> {
        match self {
            TreeData::InnerNodeData(data) => TreeData::InnerNodeData(f(data)),
            TreeData::LeafData(data) => TreeData::LeafData(data),
        }
    }
    pub fn map_leaves<J, F: FnOnce(L) -> J>(self, f: F) -> TreeData<I, J> {
        match self {
            TreeData::InnerNodeData(data) => TreeData::InnerNodeData(data),
            TreeData::LeafData(data) => TreeData::LeafData(f(data)),
        }
    }
    pub fn as_ref(&self) -> TreeDataRef<I, L> {
        match self {
            &TreeData::InnerNodeData(ref data) => TreeDataRef::InnerNodeData(data),
            &TreeData::LeafData(ref data) => TreeDataRef::LeafData(data),
        }
    }
}
impl<'a, I: Clone, L: Clone> TreeDataRef<'a, I, L> {
    pub fn to_owned(&self) -> TreeData<I, L> {
        match self {
            &TreeDataRef::InnerNodeData(data) => TreeData::InnerNodeData(data.clone()),
            &TreeDataRef::LeafData(data) => TreeData::LeafData(data.clone()),
        }
    }
}

impl<'a, I, L> Tree<&'a I, &'a L> {
    /// A representation of the fork in the tree at the root of this tree
    pub fn fork_copy(&self) -> Option<(&'a I, Vec<TreeDataRef<'a, I, L>>)> {
        match self {
            &Tree::InnerNode(ref data, ref children) => {
                Some((data, children.iter().map(Self::data).collect()))
            }
            &Tree::Leaf(_) => None,
        }
    }

    pub fn data(&self) -> TreeDataRef<'a, I, L> {
        match self {
            &Tree::InnerNode(data, _) => TreeDataRef::InnerNodeData(data),
            &Tree::Leaf(data) => TreeDataRef::LeafData(data),
        }
    }
}

impl<I, L> Tree<I, L> {
    /// A representation of the fork in the tree at the root of this tree
    pub fn fork(&self) -> Option<(&I, Vec<TreeDataRef<I, L>>)> {
        match self {
            &Tree::InnerNode(ref data, ref children) => {
                Some((data, children.iter().map(Self::data_ref).collect()))
            }
            &Tree::Leaf(_) => None,
        }
    }

    pub fn data_ref(&self) -> TreeDataRef<I, L> {
        match self {
            &Tree::InnerNode(ref data, _) => TreeDataRef::InnerNodeData(data),
            &Tree::Leaf(ref data) => TreeDataRef::LeafData(data),
        }
    }

    pub fn unwrap_as_inner(self) -> (I, Vec<Tree<I, L>>) {
        match self {
            Tree::InnerNode(data, children) => (data, children),
            Tree::Leaf(_) => panic!("Tried to unwrap leaf as inner node"),
        }
    }

    pub fn children_iter(&self) -> Option<slice::Iter<Tree<I, L>>> {
        match self {
            &Tree::InnerNode(_, ref children) => Some(children.iter()),
            &Tree::Leaf(_) => None,
        }
    }

    /// Iterates over the terminals.
    pub fn terminal_iter<'a>(&'a self) -> impl Iterator<Item = &'a L> {
        self.into_iter().filter_map(|expr| match expr {
            &Tree::InnerNode(_, _) => None,
            &Tree::Leaf(ref data) => Some(data),
        })
    }

    /// Is this node a leaf (a.k.a. a "terminal")?
    pub fn is_leaf(&self) -> bool {
        match self {
            &Tree::InnerNode(_, _) => false,
            &Tree::Leaf(_) => true,
        }
    }

    pub fn transform<'a, T: TreeTransformer<'a, I, L>>(&'a self,
                                                       t: &mut T)
                                                       -> Tree<T::OutputI, T::OutputL> {
        match self {
            &Tree::InnerNode(ref data, ref children) => {
                t.pre_inner_node(data);
                let new_data = t.make_inner_node_data(data);
                let new_children = children.into_iter().map(|child| child.transform(t)).collect();
                t.post_inner_node(data);
                Tree::InnerNode(new_data, new_children)
            }
            &Tree::Leaf(ref data) => {
                t.pre_leaf(data);
                let new_data = t.make_leaf_data(data);
                t.post_leaf(data);
                Tree::Leaf(new_data)
            }
        }
    }
}

impl<'a, I, L> IntoIterator for &'a Tree<I, L> {
    type Item = &'a Tree<I, L>;
    type IntoIter = PreOrderTreeIter<'a, I, L>;
    fn into_iter(self) -> Self::IntoIter {
        PreOrderTreeIter { stack: vec![self] }
    }
}

pub struct PreOrderTreeIter<'a, I: 'a, L: 'a> {
    stack: Vec<&'a Tree<I, L>>,
}

impl<'a, I, L> Iterator for PreOrderTreeIter<'a, I, L> {
    type Item = &'a Tree<I, L>;
    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().map(|next| {
            next.children_iter().map(|iter| self.stack.extend(iter.rev()));
            next
        })
    }
}

#[allow(unused_variables)]
pub trait TreeTransformer<'a, I, L> {
    type OutputI;
    type OutputL;

    fn make_inner_node_data(&mut self, data: &'a I) -> Self::OutputI;
    fn make_leaf_data(&mut self, data: &'a L) -> Self::OutputL;
    fn pre_inner_node(&mut self, data: &'a I) {}
    fn post_inner_node(&mut self, data: &'a I) {}
    fn pre_leaf(&mut self, data: &'a L) {}
    fn post_leaf(&mut self, data: &'a L) {}
}


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

    /// A representation of the fork in the tree at the root of this tree
    pub fn fork(&self) -> (&str, Vec<&str>) {
        (self.head.as_str(), self.children.iter().map(|child| child.head.as_str()).collect())
    }


    /// Is this node a leaf (a.k.a. a "terminal")?
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }


    /// Iterates over the terminals.
    pub fn terminal_iter<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.into_iter().filter_map(|expr| if expr.is_leaf() {
            Some(expr.head.as_str())
        } else {
            None
        })
    }

    // Is this node an internal proper-noun node?
    fn is_proper(&self) -> bool {
        self.head.as_str() == "NNP" || self.head.as_str() == "NNPS"
    }

    // Should this node always be capitalized?
    fn always_capitalize(&self) -> bool {
        self.head.as_str() == "I"
    }

    /// Capitalize this like a sentence
    ///
    /// ## Details
    ///
    /// Capitalize the first word in this `SynTree` and any proper nouns. Uncapitalize the rest.
    pub fn fix_caps(&mut self) {
        self.fix_caps_with_force(true)
    }

    /// Performs the following substitutions
    pub fn fix_terminals(&mut self) {
        self.terminal_sub("\\/", '/');
        self.terminal_sub("-LRB-", '(');
        self.terminal_sub("-RRB-", ')');
        self.terminal_sub("``", '“');
        self.terminal_sub("''", '”');
        self.terminal_sub("`", '‘');
        self.terminal_sub("'", '’');
    }

    fn terminal_sub(&mut self, from: &str, to: char) {
        for (string, terminal) in self {
            if terminal {
                if string.as_str() == from {
                    string.clear();
                    string.push(to);
                }
            }
        }
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

impl<I: AsRef<str>, L: AsRef<str>> fmt::Debug for Tree<I, L> {
    /// Format it as a tree
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Tree::InnerNode(ref data, ref children) => {
                try!(write!(f, "({}", data.as_ref()));
                for child in children {
                    try!(write!(f, " {:?}", child));
                }
                write!(f, ")")
            }
            &Tree::Leaf(ref data) => write!(f, "{}", data.as_ref()),
        }
    }
}

impl<I: AsRef<str>, L: AsRef<str>> fmt::Display for Tree<I, L> {
    /// Format it as a sentance
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn requires_pre_space(terminal: &str) -> bool {
            terminal.chars().next().map(char::is_alphabetic).unwrap_or(false)
        }

        let mut first = true;

        for terminal in self.terminal_iter() {
            if requires_pre_space(terminal.as_ref()) && !first {
                try!(write!(f, " "));
            }
            try!(write!(f, "{}", terminal.as_ref()));
            first = false;
        }
        Ok(())
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
            terminal.chars()
                .next()
                .map(|c| c.is_alphanumeric() || c == '(' || c == ')')
                .unwrap_or(false)
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

pub fn convert(tree: SynTree) -> Tree<Tag, Arc<String>> {
    if tree.children.len() == 0 {
        Tree::Leaf(Arc::new(tree.head))
    } else {
        Tree::InnerNode(Tag::from_str(tree.head.as_str()),
                        tree.children.into_iter().map(convert).collect())
    }
}
