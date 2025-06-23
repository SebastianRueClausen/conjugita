use conjugita_types::Verb;
use std::{collections::HashMap, fs};

fn main() {
    let verbs: HashMap<String, Verb> =
        ron::de::from_str(include_str!("src/verbs.ron")).expect("failed to read verbs");
    let verbs: Vec<_> = verbs.values().cloned().collect();
    let binary =
        bincode::serde::encode_to_vec(verbs, bincode::config::standard()).expect("failed to read");
    fs::write("src/verbs.bin", binary).expect("failed to write");
}
