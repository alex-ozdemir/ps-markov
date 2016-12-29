#![feature(insert_str, conservative_impl_trait, proc_macro, plugin)]
#![plugin(rocket_codegen)]

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate rocket_contrib;
extern crate rand;

use rand::distributions::{IndependentSample, Range};

use rocket_contrib::Template;

use std::fs::{self, File};
use std::io::Read;
use std::path::Path;
use std::collections::HashMap;


mod syn_tree;

use syn_tree::SynTree;

// We initialize the distributions we use to generate random sentences
lazy_static! {
    static ref TREES: (Vec<SynTree>, Vec<SynTree>) = {
        // Parse the trees
        let mut trees = parse_files("../data/parsed");

        trees.iter_mut().map(|t| t.fix_terminals()).count();

        // Annotate nodes with ancestral syntactic nodes
        trees.iter_mut().map(|t| conditioned_on_ancestors::record_parents(t, 4)).count();

        // Annotate nodes with previous (precursor) terminal
        let mut trees_with_precursors = trees.clone();
        trees_with_precursors.iter_mut()
            .map(conditioned_on_ancestors_and_last::record_precursor)
            .count();

        (trees, trees_with_precursors)
    };
    static ref DISTRIBUTIONS: (Distribution<'static>, Distribution<'static>) = {
        let (ref trees, ref trees_with_precursors) = *TREES;

        // Generate distribution
        let fallback_distro = Distribution::new(trees);

        // Generate distribution
        let distro = Distribution::new(trees_with_precursors);

        (distro, fallback_distro)
    };
}

fn generate<R: rand::Rng>(rng: &mut R) -> SynTree {
    // Get the distributions
    let (ref distro, ref fallback_distro) = *DISTRIBUTIONS;

    // Generate a sentence
    // let mut generated = conditioned_on_ancestors::generate_expr(distro, &mut rng);
    let mut generated =
        conditioned_on_ancestors_and_last::generate_expr_with_last_term(distro,
                                                                        fallback_distro,
                                                                        rng);

    // Strip out annotations
    conditioned_on_ancestors::strip_parents(&mut generated);

    generated.fix_caps();
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

#[get("/formatted")]
fn formatted() -> Template {
    let generated = generate(&mut rand::thread_rng());

    println!("\n{}\n\n{:?}", generated, generated);

    #[derive(Serialize)]
    struct TemplateContext {
        statement: String
    }

    Template::render("statement", &TemplateContext { statement: format!("{}", generated) })
}

pub fn main() {
    rocket::ignite().mount("/", routes![raw, tree, formatted]).launch();
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
    syn_tree::parse(contents.chars()).expect("failed parse").children
}

pub struct Distribution<'a> {
    // Stores (All possible child lists, their weights, total weight range) for each parent
    parent_to_children_pdf: HashMap<&'a str, (Vec<Vec<&'a str>>, Vec<usize>, Range<usize>)>,
}

impl<'a> Distribution<'a> {
    fn new(trees: &'a Vec<SynTree>) -> Self {
        let mut parent_to_children_pdf: HashMap<&str, HashMap<Vec<&str>, usize>> = HashMap::new();
        trees.iter()
            .flat_map(|t| t)
            .map(|t| t.fork())
            .map(|(parent, children)| {
                let children_pdf = parent_to_children_pdf.entry(parent)
                    .or_insert_with(HashMap::new);
                *children_pdf.entry(children).or_insert(0) += 1;
            })
            .count();
        Distribution {
            parent_to_children_pdf: parent_to_children_pdf.into_iter()
                .map(|(parent, children_cdf)| {
                    let (children, counts): (_, Vec<_>) = children_cdf.into_iter().unzip();
                    let mut acc = 0usize;
                    let cumulative = counts.iter()
                        .map(|i| {
                            acc += *i;
                            acc
                        })
                        .collect();
                    (parent,
                     (children, cumulative, Range::new(1, counts.iter().sum::<usize>() + 1)))
                })
                .collect(),
        }
    }

    fn choose<R: rand::Rng>(&self, parent: &str, rng: &mut R) -> Option<&Vec<&'a str>> {
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
    use super::{Distribution, SynTree, rand};

    pub fn strip_parents(expr: &mut SynTree) {
        expr.head.find("_").map(|idx| {
            expr.head.truncate(idx);
        });
        for child in &mut expr.children {
            strip_parents(child);
        }
    }

    pub fn record_parents(expr: &mut SynTree, n_parents: usize) {
        record_parents_with_parent(expr, &mut vec![], n_parents)
    }

    fn record_parents_with_parent<'a>(expr: &'a mut SynTree,
                                      parents: &mut Vec<&'a str>,
                                      n_parents: usize) {
        let n = expr.head.len();
        if !expr.is_leaf() {
            for parent in parents.iter().rev().take(n_parents) {
                expr.head.push_str("_");
                expr.head.push_str(parent);
            }
        }
        parents.push(&expr.head[..n]);
        for child in &mut expr.children {
            record_parents_with_parent(child, parents, n_parents);
        }
        parents.pop();
    }

    #[allow(dead_code)]
    pub fn generate_expr<R: rand::Rng>(distro: &Distribution, rng: &mut R) -> SynTree {
        generate_expr_from(String::from("ROOT"), distro, rng)
    }

    fn generate_expr_from<R: rand::Rng>(root: String,
                                        distro: &Distribution,
                                        rng: &mut R)
                                        -> SynTree {
        let children_heads = distro.choose(root.as_str(), rng).expect("Failed choice!");
        let children = children_heads.iter()
            .cloned()
            .map(String::from)
            .map(|string| generate_expr_from(string, distro, rng))
            .collect();
        SynTree::new(root, children)
    }
}

mod conditioned_on_ancestors_and_last {
    use super::{Distribution, SynTree, rand};

    pub fn record_precursor(expr: &mut SynTree) {
        let mut prior = None;
        for (head, is_leaf) in expr {
            if is_leaf {
                prior = Some(head.as_str());
            } else {
                prior.as_ref().map(|prior_str| {
                    head.push('>');
                    head.push_str(prior_str);
                });
            }
        }
    }

    #[allow(dead_code)]
    pub fn generate_expr_with_last_term<R: rand::Rng>(distro: &Distribution,
                                                      fallback_distro: &Distribution,
                                                      rng: &mut R)
                                                      -> SynTree {
        generate_expr_from_and_last_term(String::from("ROOT"), None, distro, fallback_distro, rng).0
    }

    fn generate_expr_from_and_last_term<R: rand::Rng>(mut root: String,
                                                      mut last_term: Option<String>,
                                                      distro: &Distribution,
                                                      fallback_distro: &Distribution,
                                                      rng: &mut R)
                                                      -> (SynTree, Option<String>) {
        // If the root contains an annotated precursor terminal, remove it
        root.rfind(">").map(|index| root.truncate(index));

        // Construct a lookup string with the appropriate annoted precursor terminal
        let mut lookup_root = root.clone();
        last_term.as_ref().map(|last| {
            lookup_root.push('>');
            lookup_root.push_str(last.as_str());
        });

        // Lookup what the heads of the children _should_ be
        let children_heads = distro.choose(lookup_root.as_str(), rng).unwrap_or_else(|| {
            // Fallback to a lookup uninformed by the precursor terminal if necessary
            fallback_distro.choose(root.as_str(), rng).unwrap()
        });

        // Generate all of the children, keeping the precursor terminal up-to-date
        let mut children = Vec::new();
        for string in children_heads.iter().cloned().map(String::from) {
            let (expr, new_term) = generate_expr_from_and_last_term(string,
                                                                    last_term.take(),
                                                                    distro,
                                                                    fallback_distro,
                                                                    rng);
            last_term = new_term;
            children.push(expr);
        }

        let generated = SynTree::new(root, children);
        let terminal = if generated.is_leaf() {
            Some(generated.head.clone())
        } else {
            last_term
        };
        (generated, terminal)
    }
}
