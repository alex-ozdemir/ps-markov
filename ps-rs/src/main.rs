#![feature(insert_str, conservative_impl_trait, proc_macro, plugin)]
#![plugin(rocket_codegen)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate rocket_contrib;
extern crate rand;

use rand::distributions::{IndependentSample, Range};

use rocket_contrib::Template;

use std::fs::{self, File};
use std::io::Read;
use std::path::Path;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;


mod syn_tree;

use syn_tree::{Tree, TreeTransformer, TreeDataRef, Tag};

type S = String;
type SynTree = Tree<Tag, S>;
type ParentSynTree<'a> = Tree<Vec<Tag>, &'a S>;
type PrecursorSynTree<'a> = Tree<(Vec<Tag>, Vec<&'a S>), &'a S>;

type ParentDistribution<'a> = Distribution<'a, Vec<Tag>, &'a S>;
type PrecursorDistribution<'a> = Distribution<'a, (Vec<Tag>, Vec<&'a S>), &'a S>;

const N_PARENTS: usize = 4;
const N_PRECURSORS: usize = 1;

// We initialize the distributions we use to generate random sentences
// We do it with lazy static so that the distributions can be seen as living for 'static
lazy_static! {
    static ref BASE_TREE: Vec<SynTree> = parse_files("../data/parsed");
    static ref TREES: (Vec<ParentSynTree<'static>>, Vec<PrecursorSynTree<'static>>) = {
        // Parse the trees
        let trees = &*BASE_TREE as *const Vec<SynTree>;

        // Annotate nodes with ancestral syntactic nodes
        let ref_t : &'static Vec<SynTree> = unsafe { trees.as_ref().unwrap() };
        let parent_trees = ref_t.iter()
                                .map(|t| conditioned_on_ancestors::record_parents(t, N_PARENTS))
                                .collect();

        // Annotate nodes with parents and previous (precursor) terminals
        let parents_and_precursor_trees = ref_t.iter()
            .map(|t| conditioned_on_ancestors_and_last::record_precursor(t, N_PARENTS, N_PRECURSORS))
            .collect();

        (parent_trees, parents_and_precursor_trees)
    };
    static ref DISTRIBUTIONS: (ParentDistribution<'static>, PrecursorDistribution<'static>) = {
        let (ref parent_trees, ref precursor_trees) = *TREES;

        // Generate distribution
        let fallback_distro = Distribution::new(parent_trees);

        // Generate distribution
        let distro = Distribution::new(precursor_trees);

        (fallback_distro, distro)
    };
}

fn generate<R: rand::Rng>(rng: &mut R) -> Tree<Tag, &'static String> {
    // Get the distributions
    let (ref fallback_distro, ref distro) = *DISTRIBUTIONS;

    // Generate a sentence
    let generated = conditioned_on_ancestors_and_last::generate_expr(distro,
                                                                     fallback_distro,
                                                                     N_PRECURSORS,
                                                                     rng);

    generated
}

fn generate_no_markov<R: rand::Rng>(rng: &mut R) -> Tree<Tag, &'static String> {
    // Get the distributions
    let (ref distro, _) = *DISTRIBUTIONS;

    // Generate a sentence
    let generated = conditioned_on_ancestors::generate_expr(distro, rng);

    generated
}

#[get("/tree")]
fn tree() -> String {
    let generated = generate(&mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    format!("{:?}", generated)
}

#[get("/raw")]
fn raw() -> String {
    let generated = generate(&mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    format!("{}", generated)
}

#[get("/no-markov")]
fn no_markov() -> String {
    let generated = generate_no_markov(&mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    format!("{}", generated)
}

/// Structure for rendering the statement template
#[derive(Serialize)]
struct TemplateContext {
    statement: String,
}

#[get("/formatted")]
fn formatted() -> Template {
    let generated = generate(&mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    Template::render("statement",
                     &TemplateContext { statement: format!("{}", generated) })
}

pub fn main() {
    rocket::ignite().mount("/", routes![raw, tree, formatted, no_markov]).launch();
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

pub struct Distribution<'a, I: 'a + Eq + Hash, J: 'a> {
    parent_to_children_pdf: HashMap<&'a I,
                                    (Vec<Vec<TreeDataRef<'a, I, J>>>, Vec<usize>, Range<usize>)>,
}

impl<'a, I: Eq + Hash, L: Eq + Hash> Distribution<'a, I, L> {
    fn new(trees: &'a Vec<Tree<I, L>>) -> Self {
        let mut parent_to_children_pdf: HashMap<&'a I, HashMap<_, usize>> = HashMap::new();
        trees.iter()
            .flat_map(|t| t)
            .filter_map(|t| t.fork())
            .map(|(parent, children)| {
                let children_pdf = parent_to_children_pdf.entry(parent)
                    .or_insert_with(HashMap::new);
                *children_pdf.entry(children).or_insert(0) += 1;
            })
            .count();
        Distribution {
            parent_to_children_pdf: parent_to_children_pdf.into_iter()
                .map(|(parent, children_pdf): (&'a I,
                                               HashMap<Vec<TreeDataRef<'a, I, L>>, usize>)| {
                    let (children, counts): (Vec<Vec<TreeDataRef<'a, I, L>>>, Vec<usize>) =
                        children_pdf.into_iter().unzip();
                    let mut acc = 0usize;
                    let cumulative: Vec<usize> = counts.iter()
                        .map(|i| {
                            acc += *i;
                            acc
                        })
                        .collect();
                    (parent, (children, cumulative, Range::new(1, acc + 1)))
                })
                .collect(),
        }
    }

    fn choose<R: rand::Rng>(&self, parent: &I, rng: &mut R) -> Option<&Vec<TreeDataRef<'a, I, L>>> {
        self.parent_to_children_pdf.get(parent).map(|&(ref children, ref weights, ref range)| {
            let idx = match weights.as_slice().binary_search(&range.ind_sample(rng)) {
                Err(i) => i,
                Ok(i) => i,
            };
            &children[idx]
        })
    }
}

mod conditioned_on_ancestors {
    use super::{Distribution, ParentDistribution, SynTree, rand, Tree, TreeTransformer,
                TreeDataRef, Hash, Tag};


    struct ParentRecorder<'a, I: 'a> {
        parents: Vec<&'a I>,
        n_parents: usize,
    }

    impl<'a, I> ParentRecorder<'a, I> {
        fn new(n_parents: usize) -> Self {
            ParentRecorder {
                parents: vec![],
                n_parents: n_parents,
            }
        }
    }

    impl<'a, I: Copy, L: 'a> TreeTransformer<'a, I, L> for ParentRecorder<'a, I> {
        type OutputI = Vec<I>;
        type OutputL = &'a L;
        fn pre_inner_node(&mut self, data: &'a I) {
            // Before transforming each inner node, push it on
            self.parents.push(data);
        }
        fn make_inner_node_data(&mut self, _: &'a I) -> Self::OutputI {
            // Read off the last n ancestors
            self.parents.iter().rev().take(self.n_parents).cloned().cloned().collect()
        }
        fn post_inner_node(&mut self, _: &'a I) {
            self.parents.pop();
        }
        fn make_leaf_data(&mut self, data: &'a L) -> Self::OutputL {
            data
        }
    }

    pub fn strip_parents<'a>(tree: Tree<&Vec<Tag>, &'a String>) -> Tree<Tag, &'a String> {
        match tree {
            Tree::InnerNode(parent_list, children) => {
                Tree::InnerNode(*parent_list.into_iter().last().unwrap(),
                                children.into_iter().map(strip_parents).collect())
            }
            Tree::Leaf(word) => Tree::Leaf(word),
        }
    }

    pub fn record_parents(expr: &SynTree, n_parents: usize) -> Tree<Vec<Tag>, &String> {
        expr.transform(&mut ParentRecorder::new(n_parents))
    }

    #[allow(dead_code)]
    pub fn generate_expr<'a, R: rand::Rng>(distro: &ParentDistribution<'a>,
                                           rng: &mut R)
                                           -> Tree<Tag, &'a String> {
        let root_vec = vec![Tag::ROOT];
        strip_parents(generate_expr_from(&root_vec, distro, rng))
    }

    pub fn generate_expr_from<'a, 'b: 'a, I: Eq + Hash, L: Eq + Hash, R: rand::Rng>
        (root: &'a I,
         distro: &Distribution<'b, I, &'b L>,
         rng: &mut R)
         -> Tree<&'a I, &'b L> {
        let children_heads = distro.choose(&root, rng).expect("Failed choice!");
        let children = children_heads.iter()
            .map(|data| match data {
                &TreeDataRef::InnerNodeData(ref internal) => {
                    generate_expr_from(internal.clone(), distro, rng)
                }
                &TreeDataRef::LeafData(data) => Tree::Leaf(*data),
            })
            .collect();
        Tree::InnerNode(root, children)
    }
}

mod conditioned_on_ancestors_and_last {
    use super::{ParentDistribution, PrecursorDistribution, rand, Tree, TreeTransformer, VecDeque,
                Tag, Distribution, TreeDataRef};

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

    impl<'a, I: Copy, L> TreeTransformer<'a, I, L> for PrecursorRecorder<'a, I, L> {
        type OutputI = (Vec<I>, Vec<&'a L>);
        type OutputL = &'a L;
        fn pre_inner_node(&mut self, data: &'a I) {
            // Before transforming each inner node, push it on
            self.parents.push(data);
        }
        fn make_inner_node_data(&mut self, _: &'a I) -> Self::OutputI {
            // Read off the last n ancestors
            (self.parents.iter().rev().take(self.n_parents).cloned().cloned().collect(),
             self.terminals.iter().cloned().collect())
        }
        fn post_inner_node(&mut self, _: &'a I) {
            self.parents.pop();
        }
        fn make_leaf_data(&mut self, data: &'a L) -> Self::OutputL {
            data
        }
        fn post_leaf(&mut self, data: &'a L) {
            self.terminals.push_front(data);
            if self.terminals.len() < self.n_terminals {
                self.terminals.pop_back();
            }
        }
    }

    pub fn strip_precursors<'a>(tree: Tree<&Vec<Tag>, &'a String>) -> Tree<Tag, &'a String> {
        match tree {
            Tree::InnerNode(parent_list, children) => {
                Tree::InnerNode(*parent_list.iter().last().unwrap(),
                                children.into_iter().map(strip_precursors).collect())
            }
            Tree::Leaf(word) => Tree::Leaf(word),
        }
    }

    pub fn record_precursor<'a>(expr: &'a Tree<Tag, String>,
                                n_parents: usize,
                                n_terminals: usize)
                                -> Tree<(Vec<Tag>, Vec<&'a String>), &'a String> {
        expr.transform(&mut PrecursorRecorder::new(n_parents, n_terminals))
    }

    #[allow(dead_code)]
    pub fn generate_expr<'a, R: rand::Rng>(distro: &PrecursorDistribution<'a>,
                                           fallback_distro: &ParentDistribution<'a>,
                                           n_terminals: usize,
                                           rng: &mut R)
                                           -> Tree<Tag, &'a String> {
        let root_vec = vec![Tag::ROOT];
        let mut terminals = VecDeque::new();
        strip_precursors(generate_expr_from(&root_vec,
                                            &mut terminals,
                                            n_terminals,
                                            distro,
                                            fallback_distro,
                                            rng))
    }

    pub fn generate_expr_from<'a, 'b, R>(root: &'a Vec<Tag>,
                                         terminals: &mut VecDeque<&'b String>,
                                         n_terminals: usize,
                                         distro: &Distribution<'b,
                                                               (Vec<Tag>, Vec<&'b String>),
                                                               &'b String>,
                                         fallback_distro: &Distribution<'b, Vec<Tag>, &'b String>,
                                         rng: &mut R)
                                         -> (Tree<&'a Vec<Tag>, &'b String>)
        where 'b: 'a,
              R: rand::Rng
    {
        let root_with_precursor = (root.clone(), terminals.iter().cloned().collect());
        let children_heads: Vec<TreeDataRef<Vec<Tag>, &String>> =
            distro.choose(&root_with_precursor, rng)
                .map(|data_refs| {
                    data_refs.into_iter()
                        .map(|data_ref| data_ref.map_inner(|inner| &inner.0))
                        .collect()
                })
                .unwrap_or_else(|| {
                    fallback_distro.choose(root, rng).expect("Failed selection").clone()
                });
        let children = children_heads.iter()
            .map(|data| match data {
                &TreeDataRef::InnerNodeData(ref internal) => {
                    generate_expr_from(&internal,
                                       terminals,
                                       n_terminals,
                                       distro,
                                       fallback_distro,
                                       rng)
                }
                &TreeDataRef::LeafData(data) => {
                    terminals.push_front(data);
                    if terminals.len() < n_terminals {
                        terminals.pop_back();
                    }
                    Tree::Leaf(*data)
                }
            })
            .collect();
        Tree::InnerNode(root, children)
    }
}
