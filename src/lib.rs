mod lexer;
mod parser;

use std::fmt::Display;

pub use self::{lexer::*, parser::*};

fn flatten(items: &[impl Display], separator: &str) -> String {
    let strings = items.iter().map(|s| s.to_string()).collect::<Vec<_>>();
    strings.join(separator)
}
