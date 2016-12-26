#![feature(conservative_impl_trait)]
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate rand;

use std::fs::{self, File};
use std::io::{Read};
use std::path::Path;
use std::collections::HashMap;

use rand::distributions::{IndependentSample, Range};

mod parser;

use parser::SExpr;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

pub fn main() {
    let trees = parse_files("../data/parsed");
    println!("{:?}", trees[0]);
    let distro = Distribution::new(&trees);
    let mut rng = rand::thread_rng();
    let generated = generate_expr(&distro, &mut rng);
    println!("{}\n{:?}", generated, generated);

    rocket::ignite().mount("/", routes![index]).launch();
}

fn parse_files<P: AsRef<Path>>(path: P) -> Vec<SExpr> {
    fs::read_dir(path).expect("Could not read directory").flat_map(|entry| {
        parse_file(entry.expect("Could not read directory entry").path())
    }).collect()
}

fn parse_file<P: AsRef<Path>>(path: P) -> Vec<SExpr> {
    let mut contents = String::new();
    let mut file = File::open(path).expect("Couldn't find file");
    contents.push_str("(ALL ");
    file.read_to_string(&mut contents).expect("Couldn't read file");
    contents.push(')');
    parser::parse(contents.chars()).expect("failed parse").children
}

fn fork(expr: &SExpr) -> (&str, Vec<&str>) {
    (expr.head.as_str(), expr.children.iter().map(|child| child.head.as_str()).collect())
}

struct Distribution<'a> {
    // Stores (All possible child lists, their weights, total weight range) for each parent
    parent_to_children_pdf: HashMap<&'a str, (Vec<Vec<&'a str>>, Vec<usize>, Range<usize>)>,
}

impl<'a> Distribution<'a> {
    fn new(trees: &'a Vec<SExpr>) -> Self {
        let mut parent_to_children_pdf: HashMap<&str, HashMap<Vec<&str>, usize>> = HashMap::new();
        trees.iter().flat_map(|x| x).map(fork).map(|(parent, children)| {
            let children_pdf = parent_to_children_pdf.entry(parent).or_insert_with(HashMap::new);
            *children_pdf.entry(children).or_insert(0) += 1;
        }).count();
        Distribution {
            parent_to_children_pdf: parent_to_children_pdf.into_iter().map(|(parent, children_cdf)| {
                let (children, counts): (_, Vec<_>) = children_cdf.into_iter().unzip();
                let mut acc = 0usize;
                let cumulative = counts.iter().map(|i| {
                    acc += *i;
                    acc
                }).collect();
                (parent, (children, cumulative, Range::new(1, counts.iter().sum::<usize>()+1)))
            }).collect(),
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

fn generate_expr<R: rand::Rng>(distro: &Distribution, rng: &mut R) -> SExpr {
    generate_expr_from(String::from("ROOT"), distro, rng)
}

fn generate_expr_from<R: rand::Rng>(root: String, distro: &Distribution, rng: &mut R) -> SExpr {
    let children_heads = distro.choose(root.as_str(), rng).expect("Failed choice!");
    let children = children_heads.iter().cloned().map(String::from).map(|string| generate_expr_from(string, distro, rng)).collect();
    SExpr::new(root, children)
}
