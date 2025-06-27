use std::io;
use std::ops::Range;

use clap::Parser;
use colored::Colorize;
use conjugita_types::{
    Conjugation, Ending, ImperativeTense, Number, Person, Reflexiveness, Regularity,
    SubjunctiveTense, Translation, TranslationKind,
};
use conjugita_types::{IndicativeTense, Verb};
use rand::prelude::IndexedRandom;
use rand::{Rng, SeedableRng};

fn english_pronoun(person: Person, number: Number) -> &'static str {
    match (person, number) {
        (Person::First, Number::Singular) => "I",
        (Person::First, Number::Plural) => "we",
        (Person::Second, Number::Singular) => "you",
        (Person::Second, Number::Plural) => "you all",
        (Person::Third, Number::Singular) => "he/she/it",
        (Person::Third, Number::Plural) => "they",
    }
}

fn english_present_to_be(person: Person, number: Number) -> &'static str {
    match (number, person) {
        (Number::Singular, Person::First) => "am",
        (Number::Singular, Person::Third) => "is",
        (Number::Singular, Person::Second) => "are",
        (Number::Plural, _) => "are",
    }
}

fn english_past_to_be(person: Person, number: Number) -> &'static str {
    match (number, person) {
        (Number::Singular, Person::Second) | (Number::Plural, _) => "were",
        (Number::Singular, _) => "was",
    }
}

fn english_present(person: Person, number: Number, translation: &Translation) -> &str {
    if let (Person::Third, Number::Singular) = (person, number) {
        &translation.present_third_person
    } else {
        &translation.present
    }
}

fn english_present_to_have(person: Person, number: Number) -> &'static str {
    if let (Person::Third, Number::Singular) = (person, number) {
        "has"
    } else {
        "have"
    }
}

fn conjugate_translation(translation: &Translation, conjugation: &Conjugation) -> String {
    match *conjugation {
        Conjugation::Indicative(tense, person, number) => match tense {
            IndicativeTense::Present => {
                let pronoun = english_pronoun(person, number);
                let to_be = english_present_to_be(person, number);
                let present = english_present(person, number, translation);
                match translation.kind {
                    TranslationKind::ToDoSomething => {
                        format!(
                            "{pronoun} {present}, {pronoun} {to_be} {}",
                            translation.gerund
                        )
                    }
                    TranslationKind::ToBeSomething => {
                        format!("{pronoun} {to_be} {}", translation.past_participle)
                    }
                    TranslationKind::ToBe => {
                        format!("{pronoun} {to_be}")
                    }
                }
            }
            IndicativeTense::Imperfect => {
                let pronoun = english_pronoun(person, number);
                let to_be = english_past_to_be(person, number);
                match translation.kind {
                    TranslationKind::ToDoSomething => {
                        format!(
                            "{pronoun} {to_be} {}, {pronoun} used to {}, {pronoun} {}",
                            translation.gerund, translation.present, translation.past
                        )
                    }
                    TranslationKind::ToBeSomething => {
                        format!(
                            "{pronoun} {to_be} {}, {pronoun} used to {}",
                            translation.past_participle, translation.present
                        )
                    }
                    TranslationKind::ToBe => {
                        format!("{pronoun} {to_be}, {pronoun} used to be")
                    }
                }
            }
            IndicativeTense::Preterite => {
                let pronoun = english_pronoun(person, number);
                let to_be = english_past_to_be(person, number);
                match translation.kind {
                    TranslationKind::ToDoSomething => {
                        format!("{pronoun} {}", translation.past)
                    }
                    TranslationKind::ToBeSomething => {
                        format!("{pronoun} {to_be} {}", translation.past_participle)
                    }
                    TranslationKind::ToBe => {
                        format!("{pronoun} {to_be}")
                    }
                }
            }
            _ => {
                let to_be = if tense == IndicativeTense::Conditional {
                    "would"
                } else {
                    "will"
                };
                let pronoun = english_pronoun(person, number);
                match translation.kind {
                    TranslationKind::ToDoSomething | TranslationKind::ToBeSomething => {
                        format!("{pronoun} {to_be} {}", translation.present)
                    }
                    TranslationKind::ToBe => {
                        format!("{pronoun} {to_be} be")
                    }
                }
            }
        },
        Conjugation::Subjunctive(tense, person, number) => {
            let indicative_tense = match tense {
                SubjunctiveTense::Present => IndicativeTense::Present,
                SubjunctiveTense::Imperfect => IndicativeTense::Imperfect,
                SubjunctiveTense::Future => IndicativeTense::Future,
            };
            let translation = conjugate_translation(
                translation,
                &Conjugation::Indicative(indicative_tense, person, number),
            );
            format!("(I think/hope/doubt that) {}", translation)
        }
        Conjugation::Imperative(tense, person, number) => {
            let command = match (person, number) {
                (Person::First, Number::Singular) => todo!(),
                (Person::First, Number::Plural) => match tense {
                    ImperativeTense::Affirmative => "let's",
                    ImperativeTense::Negative => "let's not",
                },
                (Person::Second, Number::Singular) => match tense {
                    ImperativeTense::Affirmative => "you,",
                    ImperativeTense::Negative => "you, don't",
                },
                (Person::Second, Number::Plural) => match tense {
                    ImperativeTense::Affirmative => "you all,",
                    ImperativeTense::Negative => "you all, don't",
                },
                (Person::Third, Number::Singular) => match tense {
                    ImperativeTense::Affirmative => "you (polite),",
                    ImperativeTense::Negative => "you (polite), don't",
                },
                (Person::Third, Number::Plural) => match tense {
                    ImperativeTense::Affirmative => "you all (polite),",
                    ImperativeTense::Negative => "you all (polite), don't",
                },
            };
            format!("{command} {}!", translation.present)
        }
        Conjugation::Progressive(tense, person, number) => {
            let pronoun = english_pronoun(person, number);
            match tense {
                IndicativeTense::Present => match translation.kind {
                    TranslationKind::ToDoSomething | TranslationKind::ToBeSomething => {
                        let to_be = english_present_to_be(person, number);
                        format!("{pronoun} {to_be} {}", translation.gerund)
                    }
                    TranslationKind::ToBe => {
                        let to_be = english_present_to_be(person, number);
                        format!("{pronoun} {to_be} being")
                    }
                },
                IndicativeTense::Preterite | IndicativeTense::Imperfect => match translation.kind {
                    TranslationKind::ToDoSomething | TranslationKind::ToBeSomething => {
                        let to_be = english_past_to_be(person, number);
                        format!("{pronoun} {to_be} {}", translation.gerund)
                    }
                    TranslationKind::ToBe => {
                        let to_be = english_past_to_be(person, number);
                        format!("{pronoun} {to_be} being")
                    }
                },
                IndicativeTense::Conditional => match translation.kind {
                    TranslationKind::ToDoSomething | TranslationKind::ToBeSomething => {
                        format!("{pronoun} would be {}", translation.gerund)
                    }
                    TranslationKind::ToBe => format!("{pronoun} would be"),
                },
                IndicativeTense::Future => match translation.kind {
                    TranslationKind::ToDoSomething | TranslationKind::ToBeSomething => {
                        format!("{pronoun} will be {}", translation.gerund)
                    }
                    TranslationKind::ToBe => format!("{pronoun} will be"),
                },
            }
        }
        Conjugation::Perfect(tense, person, number) => {
            let pronoun = english_pronoun(person, number);
            match tense {
                IndicativeTense::Present => {
                    let to_have = english_present_to_have(person, number);
                    format!("{pronoun} {to_have} {}", translation.past_participle)
                }
                IndicativeTense::Preterite | IndicativeTense::Imperfect => {
                    format!("{pronoun} had {}", translation.past_participle)
                }
                IndicativeTense::Conditional => {
                    format!("{pronoun} would have {}", translation.past_participle)
                }
                IndicativeTense::Future => {
                    format!("{pronoun} will have {}", translation.past_participle)
                }
            }
        }
        Conjugation::SubjunctivePerfect(tense, person, number) => {
            let indicative_tense = match tense {
                SubjunctiveTense::Present => IndicativeTense::Present,
                SubjunctiveTense::Imperfect => IndicativeTense::Imperfect,
                SubjunctiveTense::Future => IndicativeTense::Future,
            };
            let translation = conjugate_translation(
                translation,
                &Conjugation::Perfect(indicative_tense, person, number),
            );
            format!("(I think/hope/doubt that) {}", translation)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Explination {
    stem_range: Range<usize>,
    ending_range: Range<usize>,
    stem_comment: String,
    ending_comment: String,
}

impl Explination {
    fn render(&self, conjugated: &str) -> String {
        assert_eq!(self.stem_range.end, self.ending_range.start);
        let prefix = &conjugated[..self.stem_range.start];
        let postfix = &conjugated[self.ending_range.end..];
        let stem = &conjugated[self.stem_range.clone()];
        let ending = &conjugated[self.ending_range.clone()];
        format!(
            "\t\t\"{prefix}{}{}{postfix}\"\n\t\t\tstem \"{}\": {}\n\t\t\tending \"{}\": {}",
            stem.red(),
            ending.green(),
            stem.red(),
            self.stem_comment,
            ending.green(),
            self.ending_comment,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Conjugated {
    word: String,
    explination: Option<Explination>,
}

/// Conjugate `Verb` according to `Conjugation`.
///
/// If `use_irregular` is `false`, it will conjugate as if the verb is regular no matter what.
fn conjugate(
    verb: &Verb,
    conjugation: &Conjugation,
    use_irregular: bool,
) -> (Regularity, Vec<Conjugated>) {
    if use_irregular {
        if let Some(words) = verb.irregulars.get_vec(conjugation) {
            let conjugated = words
                .iter()
                .map(|word| Conjugated {
                    explination: None,
                    word: word.clone(),
                })
                .collect();
            return (Regularity::Irregular, conjugated);
        }
    }

    let mut regularity = Regularity::Regular;

    let words = match *conjugation {
        Conjugation::Indicative(tense, person, number) => {
            let (ending, ending_comment) = match tense {
                IndicativeTense::Preterite => {
                    let ending = match verb.ending {
                        Ending::Ar => match (person, number) {
                            (Person::First, Number::Singular) => "é",
                            (Person::First, Number::Plural) => "amos",
                            (Person::Second, Number::Singular) => "aste",
                            (Person::Second, Number::Plural) => "asties",
                            (Person::Third, Number::Singular) => "ó",
                            (Person::Third, Number::Plural) => "aron",
                        },
                        Ending::Er | Ending::Ir => match (person, number) {
                            (Person::First, Number::Singular) => "í",
                            (Person::First, Number::Plural) => "imos",
                            (Person::Second, Number::Singular) => "iste",
                            (Person::Second, Number::Plural) => "isteis",
                            (Person::Third, Number::Singular) => "ió",
                            (Person::Third, Number::Plural) => "ieron",
                        },
                    };
                    let comment = format!(
                        "regular preterite {}-verb ending for {person} {number}",
                        verb.ending.past_ending_category()
                    );
                    (ending, comment)
                }
                IndicativeTense::Imperfect => {
                    let ending = match verb.ending {
                        Ending::Ar => match (person, number) {
                            (Person::First | Person::Third, Number::Singular) => "aba",
                            (Person::First, Number::Plural) => "abamos",
                            (Person::Second, Number::Singular) => "abas",
                            (Person::Second, Number::Plural) => "abais",
                            (Person::Third, Number::Plural) => "aban",
                        },
                        Ending::Er | Ending::Ir => match (person, number) {
                            (Person::First | Person::Third, Number::Singular) => "ía",
                            (Person::First, Number::Plural) => "íamos",
                            (Person::Second, Number::Singular)
                            | (Person::Second, Number::Plural) => "ías",
                            (Person::Third, Number::Plural) => "ían",
                        },
                    };
                    let comment = format!(
                        "regular imperfect {}-verb ending for {person} {number}",
                        verb.ending.past_ending_category()
                    );
                    (ending, comment)
                }
                IndicativeTense::Present => {
                    let ending = match verb.ending {
                        Ending::Ar => match (person, number) {
                            (Person::First, Number::Singular) => "o",
                            (Person::First, Number::Plural) => "amos",
                            (Person::Second, Number::Singular) => "as",
                            (Person::Second, Number::Plural) => "áis",
                            (Person::Third, Number::Singular) => "a",
                            (Person::Third, Number::Plural) => "an",
                        },
                        Ending::Er => match (person, number) {
                            (Person::First, Number::Singular) => "o",
                            (Person::First, Number::Plural) => "emos",
                            (Person::Second, Number::Singular) => "es",
                            (Person::Second, Number::Plural) => "éis",
                            (Person::Third, Number::Singular) => "e",
                            (Person::Third, Number::Plural) => "en",
                        },
                        Ending::Ir => match (person, number) {
                            (Person::First, Number::Singular) => "o",
                            (Person::First, Number::Plural) => "imos",
                            (Person::Second, Number::Singular) => "es",
                            (Person::Second, Number::Plural) => "ís",
                            (Person::Third, Number::Singular) => "e",
                            (Person::Third, Number::Plural) => "en",
                        },
                    };
                    let comment = format!(
                        "regular present {}-verb ending for {person} {number}",
                        verb.ending
                    );
                    (ending, comment)
                }
                IndicativeTense::Future => {
                    let ending = match (person, number) {
                        (Person::First, Number::Singular) => "é",
                        (Person::First, Number::Plural) => "emos",
                        (Person::Second, Number::Singular) => "ás",
                        (Person::Second, Number::Plural) => "éis",
                        (Person::Third, Number::Singular) => "á",
                        (Person::Third, Number::Plural) => "án",
                    };
                    let comment = format!("regular future verb ending for {person} {number}");
                    (ending, comment)
                }
                IndicativeTense::Conditional => {
                    let ending = match (person, number) {
                        (Person::First | Person::Third, Number::Singular) => "ía",
                        (Person::First, Number::Plural) => "íamos",
                        (Person::Second, Number::Singular) => "ías",
                        (Person::Second, Number::Plural) => "íais",
                        (Person::Third, Number::Plural) => "ían",
                    };
                    let comment = format!("regular conditional verb  ending for {person} {number}");
                    (ending, comment)
                }
            };
            let (stem, stem_comment) = if matches!(
                tense,
                IndicativeTense::Imperfect | IndicativeTense::Preterite | IndicativeTense::Present
            ) {
                let comment = format!(
                    "formed by removing {} from {}",
                    verb.ending,
                    verb.infinitive_without_reflexive()
                );
                (verb.stem.clone(), comment)
            } else {
                let (stem, stem_regularity) = verb.future_stem();
                regularity = stem_regularity;
                let comment = match stem_regularity {
                    Regularity::Regular => {
                        format!(
                            "formed by the infinitive \"{}\"",
                            verb.infinitive_without_reflexive()
                        )
                    }
                    Regularity::Irregular => {
                        String::from("irregular stem for the future and conditional tenses")
                    }
                };
                (stem, comment)
            };
            let word = if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                format!("{reflexive_pronoun} {}{ending}", stem)
            } else {
                format!("{}{ending}", stem)
            };
            let words = vec![Conjugated {
                explination: Some(Explination {
                    stem_range: 0..stem.len(),
                    ending_range: stem.len()..word.len(),
                    stem_comment,
                    ending_comment,
                }),
                word,
            }];
            return (regularity, words);
        }
        Conjugation::Subjunctive(tense, person, number) => {
            let prefixes = match tense {
                SubjunctiveTense::Present => vec![match verb.ending {
                    Ending::Ar if (person, number) == (Person::Second, Number::Plural) => "é",
                    Ending::Ar => "e",
                    Ending::Er | Ending::Ir
                        if (person, number) == (Person::Second, Number::Plural) =>
                    {
                        "á"
                    }
                    Ending::Er | Ending::Ir => "a",
                }],
                SubjunctiveTense::Imperfect => match verb.ending {
                    Ending::Ar => vec!["ara", "ase"],
                    Ending::Er | Ending::Ir => vec!["iera", "iese"],
                },
                SubjunctiveTense::Future => match verb.ending {
                    Ending::Ar => vec!["are"],
                    Ending::Er | Ending::Ir => vec!["iere"],
                },
            };
            let suffix = match (person, number) {
                (Person::First | Person::Third, Number::Singular) => "",
                (Person::First, Number::Plural) => "mos",
                (Person::Second, Number::Singular) => "s",
                (Person::Second, Number::Plural) => "is",
                (Person::Third, Number::Plural) => "n",
            };
            prefixes
                .into_iter()
                .map(|prefix| {
                    if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                        format!("{reflexive_pronoun} {}{prefix}{suffix}", verb.stem)
                    } else {
                        format!("{}{prefix}{suffix}", verb.stem)
                    }
                })
                .collect()
        }
        Conjugation::Imperative(tense, person, number) => match tense {
            ImperativeTense::Affirmative => {
                let ending = match verb.ending {
                    Ending::Ar => match (person, number) {
                        (Person::First | Person::Second, Number::Singular) => "a",
                        (Person::First, Number::Plural) => "emos",
                        (Person::Second, Number::Plural) => "ad",
                        (Person::Third, Number::Singular) => "e",
                        (Person::Third, Number::Plural) => "en",
                    },
                    Ending::Er => match (person, number) {
                        (Person::First | Person::Second, Number::Singular) => "e",
                        (Person::First, Number::Plural) => "amos",
                        (Person::Second, Number::Plural) => "ed",
                        (Person::Third, Number::Singular) => "a",
                        (Person::Third, Number::Plural) => "an",
                    },
                    Ending::Ir => match (person, number) {
                        (Person::First | Person::Second, Number::Singular) => "e",
                        (Person::First, Number::Plural) => "amos",
                        (Person::Second, Number::Plural) => "id",
                        (Person::Third, Number::Singular) => "a",
                        (Person::Third, Number::Plural) => "an",
                    },
                };
                let reflexive_pronoun = verb.reflexive_pronoun(person, number).unwrap_or("");
                vec![format!("¡{}{ending}{reflexive_pronoun}!", verb.stem)]
            }
            ImperativeTense::Negative => {
                let ending = match verb.ending {
                    Ending::Ar => match (person, number) {
                        (Person::First | Person::Second, Number::Singular) => "es",
                        (Person::First, Number::Plural) => "emos",
                        (Person::Second, Number::Plural) => "éis",
                        (Person::Third, Number::Singular) => "e",
                        (Person::Third, Number::Plural) => "en",
                    },
                    Ending::Er | Ending::Ir => match (person, number) {
                        (Person::First | Person::Second, Number::Singular) => "as",
                        (Person::First, Number::Plural) => "amos",
                        (Person::Second, Number::Plural) => "áis",
                        (Person::Third, Number::Singular) => "a",
                        (Person::Third, Number::Plural) => "an",
                    },
                };
                vec![
                    if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                        format!("¡no {reflexive_pronoun} {}{ending}!", verb.stem)
                    } else {
                        format!("¡no {}{ending}!", verb.stem)
                    },
                ]
            }
        },
        Conjugation::Progressive(tense, person, number) => {
            let gerund;
            let (estar_regularity, estar) = conjugate(
                &estar(),
                &Conjugation::Indicative(tense, person, number),
                true,
            );
            (gerund, regularity) = verb.gerund();
            regularity = regularity.combine(estar_regularity);
            estar
                .into_iter()
                .flat_map(|estar| {
                    if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                        vec![
                            format!("{reflexive_pronoun} {} {gerund}", estar.word),
                            format!("{} {gerund}{reflexive_pronoun}", estar.word),
                        ]
                    } else {
                        vec![format!("{} {gerund}", estar.word)]
                    }
                })
                .collect()
        }
        Conjugation::Perfect(tense, person, number) => {
            let past_participle;
            (past_participle, regularity) = verb.past_participle();
            let (haber_regularity, haber) = conjugate(
                &haber(),
                &Conjugation::Indicative(tense, person, number),
                use_irregular,
            );
            regularity = regularity.combine(haber_regularity);
            haber
                .into_iter()
                .map(|haber| {
                    if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                        format!("{reflexive_pronoun} {} {past_participle}", haber.word)
                    } else {
                        format!("{} {past_participle}", haber.word)
                    }
                })
                .collect()
        }
        Conjugation::SubjunctivePerfect(tense, person, number) => {
            let past_participle;
            (past_participle, regularity) = verb.past_participle();
            let (haber_regularity, haber) = conjugate(
                &haber(),
                &Conjugation::Subjunctive(tense, person, number),
                use_irregular,
            );
            regularity = regularity.combine(haber_regularity);
            haber
                .into_iter()
                .map(|haber| {
                    if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                        format!("{reflexive_pronoun} {} {past_participle}", haber.word)
                    } else {
                        format!("{} {past_participle}", haber.word)
                    }
                })
                .collect()
        }
    };

    let conjugated = words
        .into_iter()
        .map(|word| Conjugated {
            word,
            explination: None,
        })
        .collect();
    (regularity, conjugated)
}

#[rustfmt::skip]
fn haber() -> Verb {
    use Conjugation::*;
    let irregulars = multimap::multimap!(
        // Present:
        Indicative(IndicativeTense::Present, Person::First, Number::Singular) => "he".to_owned(),
        Indicative(IndicativeTense::Present, Person::First, Number::Plural) => "hemos".to_owned(),
        Indicative(IndicativeTense::Present, Person::Second, Number::Singular) => "has".to_owned(),
        Indicative(IndicativeTense::Present, Person::Third, Number::Singular) => "ha".to_owned(),
        Indicative(IndicativeTense::Present, Person::Third, Number::Plural) => "han".to_owned(),

        // Preterite:
        Indicative(IndicativeTense::Preterite, Person::First, Number::Singular) => "hube".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::First, Number::Plural) => "hubimos".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Second, Number::Singular) => "hubiste".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Second, Number::Plural) => "hubisteis".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Third, Number::Singular) => "hubo".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Third, Number::Plural) => "hubieron".to_owned(),

        // Subjunctive Present:
        Subjunctive(SubjunctiveTense::Present, Person::First, Number::Singular) => "haya".to_owned(),
        Subjunctive(SubjunctiveTense::Present, Person::First, Number::Plural) => "hayamos".to_owned(),
        Subjunctive(SubjunctiveTense::Present, Person::Second, Number::Singular) => "hayas".to_owned(),
        Subjunctive(SubjunctiveTense::Present, Person::Second, Number::Plural) => "hayáis".to_owned(),
        Subjunctive(SubjunctiveTense::Present, Person::Third, Number::Singular) => "haya".to_owned(),
        Subjunctive(SubjunctiveTense::Present, Person::Third, Number::Plural) => "hayan".to_owned(),

        // Subjunctive Imperfect:
        Subjunctive(SubjunctiveTense::Imperfect, Person::First, Number::Singular) => "hubiera".to_owned(),
        Subjunctive(SubjunctiveTense::Imperfect, Person::First, Number::Plural) => "hubiéramos".to_owned(),
        Subjunctive(SubjunctiveTense::Imperfect, Person::Second, Number::Singular) => "huberias".to_owned(),
        Subjunctive(SubjunctiveTense::Imperfect, Person::Second, Number::Plural) => "hubierais".to_owned(),
        Subjunctive(SubjunctiveTense::Imperfect, Person::Third, Number::Singular) => "hubiera".to_owned(),
        Subjunctive(SubjunctiveTense::Imperfect, Person::Third, Number::Plural) => "hubieran".to_owned(),

        // Subjunctive Future:
        Subjunctive(SubjunctiveTense::Future, Person::First, Number::Singular) => "hubiere".to_owned(),
        Subjunctive(SubjunctiveTense::Future, Person::First, Number::Plural) => "hubiéremos".to_owned(),
        Subjunctive(SubjunctiveTense::Future, Person::Second, Number::Singular) => "huberies".to_owned(),
        Subjunctive(SubjunctiveTense::Future, Person::Second, Number::Plural) => "hubiereis".to_owned(),
        Subjunctive(SubjunctiveTense::Future, Person::Third, Number::Singular) => "hubiere".to_owned(),
        Subjunctive(SubjunctiveTense::Future, Person::Third, Number::Plural) => "hubieren".to_owned(),
    );
    Verb {
        stem: "hab".to_owned(),
        ending: Ending::Er,
        reflexiveness: Reflexiveness::NonReflexive,
        irregular_future_stem: Some("habr".to_owned()),
        irregular_gerund: None,
        irregular_past_participle: None,
        translation: Translation::default(),
        irregulars,
    }
}

#[rustfmt::skip]
fn estar() -> Verb {
    use Conjugation::*;
    let irregulars = multimap::multimap!(
        // Present:
        Indicative(IndicativeTense::Present, Person::First, Number::Singular) => "estoy".to_owned(),
        Indicative(IndicativeTense::Present, Person::Second, Number::Singular) => "estás".to_owned(),
        Indicative(IndicativeTense::Present, Person::Third, Number::Singular) => "está".to_owned(),
        Indicative(IndicativeTense::Present, Person::Third, Number::Plural) => "están".to_owned(),

        // Preterite:
        Indicative(IndicativeTense::Preterite, Person::First, Number::Singular) => "estuve".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::First, Number::Plural) => "estuvimos".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Second, Number::Singular) => "estuviste".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Second, Number::Plural) => "estuvisteis".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Third, Number::Singular) => "estuvo".to_owned(),
        Indicative(IndicativeTense::Preterite, Person::Third, Number::Plural) => "estuvieron".to_owned(),
    );
    Verb {
        stem: "est".to_owned(),
        ending: Ending::Ar,
        reflexiveness: Reflexiveness::NonReflexive,
        irregular_future_stem: None,
        irregular_gerund: None,
        irregular_past_participle: None,
        translation: Translation::default(),
        irregulars,
    }
}

#[derive(clap::Subcommand)]
enum Command {
    /// Conjugate a Spanish verb to a specific tense.
    Conjugate,
    /// Translate an English verb to a Spanish verb with a specific tense.
    Translate,
}

/// A command line tool to practice Spanish verbs.
#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

fn get_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Correctness {
    Perfect,
    Punctuation,
    Wrong,
}

fn correctness(input: &str, answer: &str) -> Correctness {
    if input == answer {
        return Correctness::Perfect;
    } else {
        let (mut input, mut answer) = (String::from(input), String::from(answer));
        let is_not_punctuation = |c: char| !c.is_ascii_punctuation() && c != '¡';
        input.retain(is_not_punctuation);
        answer.retain(is_not_punctuation);
        let remove_accents = |string: &mut String| {
            for (from, to) in [("é", "e"), ("á", "a"), ("í", "i"), ("ó", "o"), ("ñ", "n")] {
                *string = string.replace(from, to);
            }
        };
        remove_accents(&mut input);
        remove_accents(&mut answer);
        if input == answer {
            return Correctness::Punctuation;
        } else {
            return Correctness::Wrong;
        }
    }
}

fn highlight_irregular_letters(regular: &str, irregular: &str) -> String {
    let irregular: Vec<char> = irregular.chars().collect();
    let regular: Vec<char> = regular.chars().collect();
    let strategy = seal::pair::NeedlemanWunsch::new(1, -1, -1, -1);
    let set: seal::pair::AlignmentSet<seal::pair::InMemoryAlignmentMatrix> =
        seal::pair::AlignmentSet::new(irregular.len(), regular.len(), strategy, |x, y| {
            irregular[x] == regular[y]
        })
        .unwrap();
    let alignment = set.global_alignment();
    let mut output = String::new();
    for step in alignment.steps() {
        match step {
            seal::pair::Step::Delete { x } => {
                output = format!("{output}{}", String::from(irregular[x]).red());
            }
            seal::pair::Step::Insert { .. } => (),
            seal::pair::Step::Align { x, y } => {
                if irregular[x] == regular[y] {
                    output.push(irregular[x]);
                } else {
                    output = format!("{output}{}", String::from(irregular[x]).red());
                }
            }
        }
    }
    output
}

pub fn cli() {
    let cli = Cli::parse();
    let mut rng = rand::rngs::StdRng::seed_from_u64(1);

    let (verbs, _): (Vec<Verb>, _) =
        bincode::serde::decode_from_slice(include_bytes!("verbs.bin"), bincode::config::standard())
            .expect("failed to read verbs");

    let verb = verbs.choose(&mut rng).unwrap();
    let conjugation: Conjugation = rng.random();
    let translation = conjugate_translation(&verb.translation, &conjugation);
    let (regularity, answer) = conjugate(verb, &conjugation, true);

    match cli.command {
        Command::Conjugate => {
            println!(
                "verb: {}\ntense: {conjugation}\ntranslation: \"{translation}\"",
                verb.infinitive()
            );
        }
        Command::Translate => {
            println!("translate: \"{translation}\" ({conjugation})");
        }
    }

    loop {
        let input = get_line().to_lowercase();
        let input = input.trim();

        let correct_message =
            answer
                .iter()
                .find_map(|answer| match correctness(input, &answer.word) {
                    Correctness::Perfect => Some(format!("correct!")),
                    Correctness::Punctuation => Some(format!(
                        "correct, but keep punctuation in mind (\"{input}\" vs \"{}\")",
                        answer.word,
                    )),
                    _ => None,
                });
        if let Some(message) = correct_message {
            println!("{message}");
            break;
        }

        println!("incorrect, want to try again? (y/n)");

        if get_line().to_lowercase().trim() == "y" {
            if regularity == Regularity::Irregular {
                println!("try again! (hint: it is irregular)");
            } else {
                println!("try again!");
            }
        } else {
            println!(
                "correct answer{}",
                if answer.len() > 1 { "s: " } else { ": " }
            );

            if regularity == Regularity::Irregular {
                let (_, regulars) = conjugate(verb, &conjugation, false);
                for (regular, answer) in regulars.iter().zip(answer.iter()) {
                    let print = highlight_irregular_letters(&regular.word, &answer.word);
                    println!("\t\"{print}\" ({})", "irregular".red());

                    if let Some(explination) = &answer.explination {
                        println!("{}", explination.render(&answer.word))
                    }
                }
            } else {
                for answer in &answer {
                    println!("\t\"{}\" ({})", answer.word, "regular".white());

                    if let Some(explination) = &answer.explination {
                        println!("{}", explination.render(&answer.word))
                    }
                }
            }

            break;
        }
    }
}
