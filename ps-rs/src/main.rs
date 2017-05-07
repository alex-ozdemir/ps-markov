#![feature(conservative_impl_trait, proc_macro, plugin)]
#![plugin(rocket_codegen)]

#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate rocket_contrib;
extern crate rand;

use rand::distributions::{IndependentSample, Range};

use rocket_contrib::Template;

use std::collections::{HashMap, VecDeque};
use std::fs::{self, File};
use std::hash::Hash;
use std::io::Read;
use std::path::Path;
use std::sync::Arc;


mod syn_tree;

use syn_tree::{Tree, TreeTransformer, TreeData, TreeDataRef, Tag};

type S = Arc<String>;
type SynTree = Tree<Tag, S>;

type ParentDistribution = Distribution<Vec<Tag>, S>;
type PrecursorDistribution = Distribution<(Vec<Tag>, Vec<S>), S>;

pub struct Distribution<I: Eq + Hash + Clone, L: Clone> {
    parent_to_children_pdf: HashMap<I, (Vec<Vec<TreeData<I, L>>>, Vec<usize>, Range<usize>)>,
}

struct Distributions {
    parent_distro: ParentDistribution,
    precursor_distro: PrecursorDistribution,
}

impl<I: Eq + Hash + Clone, L: Eq + Hash + Clone> Distribution<I, L> {
    fn new(trees: &Vec<Tree<I, L>>) -> Self {
        let mut parent_to_children_pdf: HashMap<I, HashMap<Vec<TreeData<I, L>>, usize>> =
            HashMap::new();
        trees.iter()
            .flat_map(|t| t)
            .filter_map(|t| t.fork())
            .map(|(parent, children)| {
                let children_pdf = parent_to_children_pdf.entry(parent.to_owned())
                    .or_insert_with(HashMap::new);
                *children_pdf.entry(children.into_iter().map(|c| c.to_owned()).collect())
                    .or_insert(0) += 1;
            })
            .count();
        Distribution {
            parent_to_children_pdf: parent_to_children_pdf.into_iter()
                .map(|(parent, children_pdf): (I, HashMap<Vec<TreeData<I, L>>, usize>)| {
                    let (children, counts): (Vec<Vec<TreeData<I, L>>>, Vec<usize>) =
                        children_pdf.into_iter().unzip();
                    let mut acc = 0usize;
                    let cumulative: Vec<usize> = counts.iter()
                        .map(|i| {
                            acc += *i;
                            acc
                        })
                        .collect();
                    (parent.to_owned(), (children, cumulative, Range::new(1, acc + 1)))
                })
                .collect(),
        }
    }

    fn choose<R: rand::Rng>(&self, parent: &I, rng: &mut R) -> Option<&Vec<TreeData<I, L>>> {
        self.parent_to_children_pdf.get(parent).map(|&(ref children, ref weights, ref range)| {
            let idx = match weights.as_slice().binary_search(&range.ind_sample(rng)) {
                Err(i) => i,
                Ok(i) => i,
            };
            &children[idx]
        })
    }
}


const N_PARENTS: usize = 4;
const N_PRECURSORS: usize = 1;

// We initialize the distributions we use to generate random sentences
// We do it with lazy static so that the distributions can be seen as living for 'static

fn generate<'a, R: rand::Rng>(fallback_distro: &'a ParentDistribution,
                              distro: &'a PrecursorDistribution,
                              rng: &mut R)
                              -> Tree<Tag, &'a str> {
    // Generate a sentence
    let generated = conditioned_on_ancestors_and_last::generate_expr(&vec![Tag::ROOT],
                                                                     distro,
                                                                     fallback_distro,
                                                                     N_PRECURSORS,
                                                                     rng);

    generated
}

fn generate_no_markov<'a, R: rand::Rng>(distro: &'a ParentDistribution,
                                        rng: &mut R)
                                        -> Tree<Tag, &'a str> {
    // Generate a sentence
    let generated = conditioned_on_ancestors::generate_expr(&vec![Tag::ROOT], distro, rng);

    generated
}

#[get("/tree")]
fn tree(distros: rocket::State<Distributions>) -> String {
    let generated = generate(&distros.parent_distro,
                             &distros.precursor_distro,
                             &mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    format!("{:?}", generated)
}

#[get("/raw")]
fn raw(distros: rocket::State<Distributions>) -> String {
    let generated = generate(&distros.parent_distro,
                             &distros.precursor_distro,
                             &mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    format!("{}", generated)
}

#[get("/no-markov")]
fn no_markov(distros: rocket::State<Distributions>) -> String {
    let generated = generate_no_markov(&distros.parent_distro, &mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    format!("{}", generated)
}

/// Structure for rendering the statement template
#[derive(Serialize, Deserialize)]
struct TemplateContext {
    statement: String,
}

#[get("/formatted")]
fn formatted(distros: rocket::State<Distributions>) -> Template {
    let generated = generate(&distros.parent_distro,
                             &distros.precursor_distro,
                             &mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    Template::render("statement",
                     &TemplateContext { statement: format!("{}", generated) })
}

pub fn main() {
    let trees = parse_files("../data/parsed");
    let parent_trees =
        trees.iter().map(|t| conditioned_on_ancestors::record_parents(t, N_PARENTS)).collect();
    let precursor_trees = trees.iter()
        .map(|t| conditioned_on_ancestors_and_last::record_precursor(t, N_PARENTS, N_PRECURSORS))
        .collect();
    let parent_distro = Distribution::new(&parent_trees);
    let precursor_distro = Distribution::new(&precursor_trees);
    rocket::ignite()
        .manage(Distributions {
            parent_distro: parent_distro,
            precursor_distro: precursor_distro,
        })
        .mount("/", routes![raw, tree, formatted, no_markov])
        .launch();
}

fn parse_files<P: AsRef<Path>>(path: P) -> Vec<SynTree> {
    fs::read_dir(path)
        .expect("Could not read directory")
        .flat_map(|entry| parse_file(entry.expect("Could not read directory entry").path()))
        .collect()
}

fn parse_file<P: AsRef<Path>>(path: P) -> Vec<SynTree> {
    let mut contents = String::new();
    let mut file = File::open(path).expect("Couldn't find file");
    contents.push_str("(ALL ");
    file.read_to_string(&mut contents).expect("Couldn't read file");
    contents.push(')');
    syn_tree::parse(contents.chars()).expect("failed parse").unwrap_as_inner().1
}

mod conditioned_on_ancestors {
    use super::{Distribution, ParentDistribution, SynTree, rand, Tree, TreeTransformer, TreeData,
                Hash, Tag, Arc};


    struct ParentRecorder<I> {
        parents: Vec<I>,
        n_parents: usize,
    }

    impl<I> ParentRecorder<I> {
        fn new(n_parents: usize) -> Self {
            ParentRecorder {
                parents: vec![],
                n_parents: n_parents,
            }
        }
    }

    impl<'a, I: Copy, L: Clone> TreeTransformer<'a, I, L> for ParentRecorder<I> {
        type OutputI = Vec<I>;
        type OutputL = L;
        fn pre_inner_node(&mut self, data: &'a I) {
            // Before transforming each inner node, push it on
            self.parents.push(data.clone());
        }
        fn make_inner_node_data(&mut self, _: &'a I) -> Self::OutputI {
            // Read off the last n ancestors
            self.parents.iter().rev().take(self.n_parents).cloned().collect()
        }
        fn post_inner_node(&mut self, _: &'a I) {
            self.parents.pop();
        }
        fn make_leaf_data(&mut self, data: &'a L) -> Self::OutputL {
            data.clone()
        }
    }

    pub fn strip_parents<'a>(tree: Tree<Vec<Tag>, &'a str>) -> Tree<Tag, &'a str> {
        match tree {
            Tree::InnerNode(parent_list, children) => {
                Tree::InnerNode(parent_list.into_iter().last().unwrap(),
                                children.into_iter().map(strip_parents).collect())
            }
            Tree::Leaf(word) => Tree::Leaf(word),
        }
    }

    pub fn record_parents(expr: &SynTree, n_parents: usize) -> Tree<Vec<Tag>, Arc<String>> {
        expr.transform(&mut ParentRecorder::new(n_parents))
    }

    #[allow(dead_code)]
    pub fn generate_expr<'a, 'b, R: rand::Rng>(root_vec: &'b Vec<Tag>,
                                               distro: &'a ParentDistribution,
                                               rng: &mut R)
                                               -> Tree<Tag, &'a str> {
        strip_parents(generate_expr_from(root_vec, distro, rng))
    }

    pub fn generate_expr_from<'a, 'b, I: Eq + Hash + Clone, R: rand::Rng>
        (root: &'b I,
         distro: &'a Distribution<I, Arc<String>>,
         rng: &mut R)
         -> Tree<I, &'a str> {
        let children_heads = distro.choose(root, rng).expect("Failed choice!");
        let children = children_heads.iter()
            .map(|data| match data {
                &TreeData::InnerNodeData(ref internal) => generate_expr_from(internal, distro, rng),
                &TreeData::LeafData(ref data) => Tree::Leaf(data.as_str()),
            })
            .collect();
        Tree::InnerNode(root.clone(), children)
    }
}

mod conditioned_on_ancestors_and_last {
    use super::{ParentDistribution, PrecursorDistribution, rand, Tree, TreeTransformer, VecDeque,
                TreeData, Tag, Distribution, TreeDataRef, Arc};

    struct PrecursorRecorder<'a, I: 'a, L: 'a> {
        terminals: VecDeque<&'a L>,
        n_terminals: usize,
        parents: Vec<&'a I>,
        n_parents: usize,
    }

    impl<'a, I, L> PrecursorRecorder<'a, I, L> {
        fn new(n_parents: usize, n_terminals: usize) -> Self {
            PrecursorRecorder {
                terminals: VecDeque::new(),
                n_terminals: n_terminals,
                parents: vec![],
                n_parents: n_parents,
            }
        }
    }

    impl<'a, I: Copy, L: Clone> TreeTransformer<'a, I, L> for PrecursorRecorder<'a, I, L> {
        type OutputI = (Vec<I>, Vec<L>);
        type OutputL = L;
        fn pre_inner_node(&mut self, data: &'a I) {
            // Before transforming each inner node, push it on
            self.parents.push(data);
        }
        fn make_inner_node_data(&mut self, _: &'a I) -> Self::OutputI {
            // Read off the last n ancestors
            (self.parents.iter().rev().take(self.n_parents).cloned().cloned().collect(),
             self.terminals.iter().cloned().cloned().collect())
        }
        fn post_inner_node(&mut self, _: &'a I) {
            self.parents.pop();
        }
        fn make_leaf_data(&mut self, data: &'a L) -> Self::OutputL {
            data.clone()
        }
        fn post_leaf(&mut self, data: &'a L) {
            self.terminals.push_front(data);
            if self.terminals.len() < self.n_terminals {
                self.terminals.pop_back();
            }
        }
    }

    pub fn strip_precursors<'a>(tree: Tree<Vec<Tag>, &'a str>) -> Tree<Tag, &'a str> {
        match tree {
            Tree::InnerNode(parent_list, children) => {
                Tree::InnerNode(parent_list.into_iter().last().unwrap(),
                                children.into_iter().map(strip_precursors).collect())
            }
            Tree::Leaf(word) => Tree::Leaf(word),
        }
    }

    pub fn record_precursor(expr: &Tree<Tag, Arc<String>>,
                            n_parents: usize,
                            n_terminals: usize)
                            -> Tree<(Vec<Tag>, Vec<Arc<String>>), Arc<String>> {
        expr.transform(&mut PrecursorRecorder::new(n_parents, n_terminals))
    }

    #[allow(dead_code)]
    pub fn generate_expr<'a, R: rand::Rng>(root_vec: &Vec<Tag>,
                                           distro: &'a PrecursorDistribution,
                                           fallback_distro: &'a ParentDistribution,
                                           n_terminals: usize,
                                           rng: &mut R)
                                           -> Tree<Tag, &'a str> {
        let mut terminals = VecDeque::new();
        strip_precursors(generate_expr_from(root_vec,
                                            &mut terminals,
                                            n_terminals,
                                            distro,
                                            fallback_distro,
                                            rng))
    }

    pub fn generate_expr_from<'a, R>(root: &Vec<Tag>,
                                     terminals: &mut VecDeque<Arc<String>>,
                                     n_terminals: usize,
                                     distro: &'a Distribution<(Vec<Tag>, Vec<Arc<String>>),
                                                              Arc<String>>,
                                     fallback_distro: &'a Distribution<Vec<Tag>, Arc<String>>,
                                     rng: &mut R)
                                     -> (Tree<Vec<Tag>, &'a str>)
        where R: rand::Rng
    {
        let root_with_precursor = (root.clone(), terminals.iter().cloned().collect());
        let children_heads: Vec<TreeDataRef<'a, Vec<Tag>, Arc<String>>> =
            distro.choose(&root_with_precursor, rng)
                .map(|data: &'a Vec<TreeData<(Vec<Tag>, Vec<Arc<String>>), Arc<String>>>| {
                    data.iter()
                        .map(|data_ref| data_ref.as_ref().map_inner(|inner| &inner.0))
                        .collect()
                })
                .unwrap_or_else(|| {
                    fallback_distro.choose(root, rng).expect("Failed selection").iter().map(|data| data.as_ref()).collect()
                });
        let children = children_heads.iter()
            .map(|data| match data {
                &TreeDataRef::InnerNodeData(internal) => {
                    generate_expr_from(&internal,
                                       terminals,
                                       n_terminals,
                                       distro,
                                       fallback_distro,
                                       rng)
                }
                &TreeDataRef::LeafData(data) => {
                    terminals.push_front(data.clone());
                    if terminals.len() < n_terminals {
                        terminals.pop_back();
                    }
                    Tree::Leaf(data.as_str())
                }
            })
            .collect();
        Tree::InnerNode(root.clone(), children)
    }
}
