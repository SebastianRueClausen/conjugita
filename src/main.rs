use std::io;
use std::{collections::HashMap, fmt};

use clap::Parser;
use colored::Colorize;
use rand::distributions::{Distribution, Standard};
use rand::seq::SliceRandom;
use rand::Rng;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Ending {
    Ar,
    Er,
    Ir,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Reflexiveness {
    Reflexive,
    NonReflexive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
enum TranslationKind {
    #[default]
    ToDoSomething,
    ToBeSomething,
    ToBe,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
struct Translation {
    kind: TranslationKind,
    present: String,
    present_third_person: String,
    past: String,
    past_participle: String,
    gerund: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct Verb {
    stem: String,
    ending: Ending,
    reflexiveness: Reflexiveness,
    irregular_future_stem: Option<String>,
    irregular_gerund: Option<String>,
    irregular_past_participle: Option<String>,
    irregulars: HashMap<Conjugation, String>,
    translation: Translation,
}

impl Verb {
    fn reflexive_pronoun(&self, person: Person, number: Number) -> Option<&'static str> {
        match self.reflexiveness {
            Reflexiveness::Reflexive => Some(match (person, number) {
                (Person::First, Number::Singular) => "me",
                (Person::First, Number::Plural) => "nos",
                (Person::Second, Number::Singular) => "te",
                (Person::Second, Number::Plural) => "os",
                (Person::Third, Number::Singular | Number::Plural) => "se",
            }),
            Reflexiveness::NonReflexive => None,
        }
    }

    fn infinitive(&self) -> String {
        let reflexive_pronoun = match self.reflexiveness {
            Reflexiveness::Reflexive => "se",
            Reflexiveness::NonReflexive => "",
        };
        format!("{}{reflexive_pronoun}", self.infinitive_without_reflexive())
    }

    fn infinitive_without_reflexive(&self) -> String {
        let ending = match self.ending {
            Ending::Ar => "ar",
            Ending::Er => "er",
            Ending::Ir => "ir",
        };
        format!("{}{ending}", self.stem)
    }

    fn future_stem(&self) -> (String, Regularity) {
        self.irregular_future_stem
            .clone()
            .map(|stem| (stem, Regularity::Irregular))
            .unwrap_or_else(|| (self.infinitive_without_reflexive(), Regularity::Regular))
    }

    fn gerund(&self) -> (String, Regularity) {
        self.irregular_gerund
            .clone()
            .map(|gerund| (gerund, Regularity::Irregular))
            .unwrap_or_else(|| {
                let ending = match self.ending {
                    Ending::Ar => "ando",
                    Ending::Er | Ending::Ir => "iendo",
                };
                (format!("{}{ending}", self.stem), Regularity::Regular)
            })
    }

    fn past_participle(&self) -> (String, Regularity) {
        self.irregular_past_participle
            .clone()
            .map(|participle| (participle, Regularity::Irregular))
            .unwrap_or_else(|| {
                let ending = match self.ending {
                    Ending::Ar => "ado",
                    Ending::Er | Ending::Ir => "ido",
                };
                (format!("{}{ending}", self.stem), Regularity::Regular)
            })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Person {
    First,
    Second,
    Third,
}

impl fmt::Display for Person {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::First => write!(f, "first person"),
            Self::Second => write!(f, "second person"),
            Self::Third => write!(f, "third person"),
        }
    }
}

impl Distribution<Person> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Person {
        match rng.gen_range(0..3) {
            0 => Person::First,
            1 => Person::Second,
            _ => Person::Third,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Number {
    Singular,
    Plural,
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Singular => write!(f, "singular"),
            Self::Plural => write!(f, "plural"),
        }
    }
}

impl Distribution<Number> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Number {
        match rng.gen_range(0..2) {
            0 => Number::Singular,
            _ => Number::Plural,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum IndicativeTense {
    Present,
    Preterite,
    Imperfect,
    Conditional,
    Future,
}

impl fmt::Display for IndicativeTense {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Present => write!(f, "present"),
            Self::Preterite => write!(f, "preterite"),
            Self::Imperfect => write!(f, "imperfect"),
            Self::Conditional => write!(f, "conditional"),
            Self::Future => write!(f, "future"),
        }
    }
}

impl Distribution<IndicativeTense> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> IndicativeTense {
        match rng.gen_range(0..5) {
            0 => IndicativeTense::Present,
            1 => IndicativeTense::Preterite,
            2 => IndicativeTense::Imperfect,
            3 => IndicativeTense::Conditional,
            _ => IndicativeTense::Future,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum SubjunctiveTense {
    Present,
    Imperfect,
    Future,
}

impl fmt::Display for SubjunctiveTense {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Present => write!(f, "present"),
            Self::Imperfect => write!(f, "imperfect"),
            Self::Future => write!(f, "future"),
        }
    }
}

impl Distribution<SubjunctiveTense> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> SubjunctiveTense {
        match rng.gen_range(0..3) {
            0 => SubjunctiveTense::Present,
            1 => SubjunctiveTense::Imperfect,
            _ => SubjunctiveTense::Future,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum ImperativeTense {
    Affirmative,
    Negative,
}

impl fmt::Display for ImperativeTense {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Affirmative => write!(f, "affirmative"),
            Self::Negative => write!(f, "negative"),
        }
    }
}

impl Distribution<ImperativeTense> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> ImperativeTense {
        match rng.gen_range(0..2) {
            0 => ImperativeTense::Affirmative,
            _ => ImperativeTense::Negative,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Conjugation {
    Indicative(IndicativeTense, Person, Number),
    Subjunctive(SubjunctiveTense, Person, Number),
    Imperative(ImperativeTense, Person, Number),
    Progressive(IndicativeTense, Person, Number),
    Perfect(IndicativeTense, Person, Number),
    SubjunctivePerfect(SubjunctiveTense, Person, Number),
}

impl fmt::Display for Conjugation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! display {
            ($name:literal, $tense:ident, $person:ident, $number:ident) => {
                write!(f, "{} {} - {} {}", $name, $tense, $person, $number)
            };
        }
        match self {
            Self::Indicative(tense, person, number) => {
                display!("indicative", tense, person, number)
            }
            Self::Subjunctive(tense, person, number) => {
                display!("subjunctive", tense, person, number)
            }
            Self::Imperative(tense, person, number) => {
                display!("imperative", tense, person, number)
            }
            Self::Progressive(tense, person, number) => {
                display!("progressive", tense, person, number)
            }
            Self::Perfect(tense, person, number) => {
                display!("perfect", tense, person, number)
            }
            Self::SubjunctivePerfect(tense, person, number) => {
                display!("subjunctive perfect", tense, person, number)
            }
        }
    }
}

impl Distribution<Conjugation> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Conjugation {
        match rng.gen_range(0..6) {
            0 => Conjugation::Indicative(rng.gen(), rng.gen(), rng.gen()),
            1 => Conjugation::Subjunctive(rng.gen(), rng.gen(), rng.gen()),
            2 => loop {
                let (person, number) = (rng.gen(), rng.gen());
                if !(person == Person::First && number == Number::Singular) {
                    break Conjugation::Imperative(rng.gen(), person, number);
                }
            },
            3 => Conjugation::Progressive(rng.gen(), rng.gen(), rng.gen()),
            4 => Conjugation::Perfect(rng.gen(), rng.gen(), rng.gen()),
            _ => Conjugation::SubjunctivePerfect(rng.gen(), rng.gen(), rng.gen()),
        }
    }
}

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

fn english_past_to_be(number: Number) -> &'static str {
    match number {
        Number::Singular => "was",
        Number::Plural => "were",
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
                let to_be = english_past_to_be(number);
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
                let to_be = english_past_to_be(number);
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
                        let to_be = english_past_to_be(number);
                        format!("{pronoun} {to_be} {}", translation.gerund)
                    }
                    TranslationKind::ToBe => {
                        let to_be = english_past_to_be(number);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Regularity {
    Regular,
    Irregular,
}

fn conjugate(verb: &Verb, conjugation: &Conjugation, use_irregular: bool) -> (String, Regularity) {
    if use_irregular {
        if let Some(word) = verb.irregulars.get(conjugation) {
            return (word.clone(), Regularity::Irregular);
        }
    }

    let mut regularity = Regularity::Regular;

    let conjugated = match *conjugation {
        Conjugation::Indicative(tense, person, number) => {
            let ending = match tense {
                IndicativeTense::Preterite => match verb.ending {
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
                },
                IndicativeTense::Imperfect => match verb.ending {
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
                        (Person::Second, Number::Singular) | (Person::Second, Number::Plural) => {
                            "ías"
                        }
                        (Person::Third, Number::Plural) => "ían",
                    },
                },
                IndicativeTense::Present => match verb.ending {
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
                },
                IndicativeTense::Future => match (person, number) {
                    (Person::First, Number::Singular) => "é",
                    (Person::First, Number::Plural) => "emos",
                    (Person::Second, Number::Singular) => "ás",
                    (Person::Second, Number::Plural) => "éis",
                    (Person::Third, Number::Singular) => "á",
                    (Person::Third, Number::Plural) => "án",
                },
                IndicativeTense::Conditional => match (person, number) {
                    (Person::First | Person::Third, Number::Singular) => "ía",
                    (Person::First, Number::Plural) => "íamos",
                    (Person::Second, Number::Singular) => "ías",
                    (Person::Second, Number::Plural) => "íais",
                    (Person::Third, Number::Plural) => "ían",
                },
            };
            let stem;
            if matches!(
                tense,
                IndicativeTense::Imperfect | IndicativeTense::Preterite | IndicativeTense::Present
            ) {
                stem = verb.stem.clone();
            } else {
                (stem, regularity) = verb.future_stem();
            };
            if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                format!("{reflexive_pronoun} {}{ending}", stem)
            } else {
                format!("{}{ending}", stem)
            }
        }
        Conjugation::Subjunctive(tense, person, number) => {
            let prefix = match tense {
                SubjunctiveTense::Present => match verb.ending {
                    Ending::Ar if (person, number) == (Person::Second, Number::Plural) => "é",
                    Ending::Ar => "e",
                    Ending::Er | Ending::Ir
                        if (person, number) == (Person::Second, Number::Plural) =>
                    {
                        "á"
                    }
                    Ending::Er | Ending::Ir => "a",
                },
                SubjunctiveTense::Imperfect => match verb.ending {
                    Ending::Ar => "ara",
                    Ending::Er | Ending::Ir => "iera",
                },
                SubjunctiveTense::Future => match verb.ending {
                    Ending::Ar => "are",
                    Ending::Er | Ending::Ir => "iere",
                },
            };
            let suffix = match (person, number) {
                (Person::First | Person::Third, Number::Singular) => "",
                (Person::First, Number::Plural) => "mos",
                (Person::Second, Number::Singular) => "s",
                (Person::Second, Number::Plural) => "is",
                (Person::Third, Number::Plural) => "n",
            };
            if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                format!("{reflexive_pronoun} {}{prefix}{suffix}", verb.stem)
            } else {
                format!("{}{prefix}{suffix}", verb.stem)
            }
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
                format!("¡{}{ending}{reflexive_pronoun}!", verb.stem)
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
                if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                    format!("¡no {reflexive_pronoun} {}{ending}!", verb.stem)
                } else {
                    format!("¡no {}{ending}!", verb.stem)
                }
            }
        },
        Conjugation::Progressive(tense, person, number) => {
            let (estar, _) = conjugate(
                &estar(),
                &Conjugation::Indicative(tense, person, number),
                true,
            );
            let gerund;
            (gerund, regularity) = verb.gerund();
            if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                format!("{reflexive_pronoun} {estar} {gerund}")
            } else {
                format!("{estar} {gerund}")
            }
        }
        Conjugation::Perfect(tense, person, number) => {
            let past_participle;
            (past_participle, regularity) = verb.past_participle();
            let (haber, _) = conjugate(
                &haber(),
                &Conjugation::Indicative(tense, person, number),
                true,
            );
            if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                format!("{reflexive_pronoun} {haber} {past_participle}")
            } else {
                format!("{haber} {past_participle}")
            }
        }
        Conjugation::SubjunctivePerfect(tense, person, number) => {
            let past_participle;
            (past_participle, regularity) = verb.past_participle();
            let (haber, _) = conjugate(
                &haber(),
                &Conjugation::Subjunctive(tense, person, number),
                true,
            );
            if let Some(reflexive_pronoun) = verb.reflexive_pronoun(person, number) {
                format!("{reflexive_pronoun} {haber} {past_participle}")
            } else {
                format!("{haber} {past_participle}")
            }
        }
    };

    (conjugated, regularity)
}

#[rustfmt::skip]
fn haber() -> Verb {
    let indicative = |tense, person, number, word: &str| -> (Conjugation, String) {
        (Conjugation::Indicative(tense, person, number), word.to_owned())
    };
    let subjunctive = |tense, person, number, word: &str| -> (Conjugation, String) {
        (Conjugation::Subjunctive(tense, person, number), word.to_owned())
    };
    let irregulars = [
        // Present:
        indicative(IndicativeTense::Present, Person::First, Number::Singular, "he"),
        indicative(IndicativeTense::Present, Person::First, Number::Plural, "hemos"),
        indicative(IndicativeTense::Present, Person::Second, Number::Singular, "has"),
        indicative(IndicativeTense::Present, Person::Third, Number::Singular, "ha"),
        indicative(IndicativeTense::Present, Person::Third, Number::Plural, "han"),
        // Preterite:
        indicative(IndicativeTense::Preterite, Person::First, Number::Singular, "hube"),
        indicative(IndicativeTense::Preterite, Person::First, Number::Plural, "hubimos"),
        indicative(IndicativeTense::Preterite, Person::Second, Number::Singular, "hubiste"),
        indicative(IndicativeTense::Preterite, Person::Second, Number::Plural, "hubisteis"),
        indicative(IndicativeTense::Preterite, Person::Third, Number::Singular, "hubo"),
        indicative(IndicativeTense::Preterite, Person::Third, Number::Plural, "hubieron"),
        // Subjunctive Present:
        subjunctive(SubjunctiveTense::Present, Person::First, Number::Singular, "haya"),
        subjunctive(SubjunctiveTense::Present, Person::First, Number::Plural, "hayamos"),
        subjunctive(SubjunctiveTense::Present, Person::Second, Number::Singular, "hayas"),
        subjunctive(SubjunctiveTense::Present, Person::Second, Number::Plural, "hayáis"),
        subjunctive(SubjunctiveTense::Present, Person::Third, Number::Singular, "haya"),
        subjunctive(SubjunctiveTense::Present, Person::Third, Number::Plural, "hayan"),
        // Subjunctive Imperfect:
        subjunctive(SubjunctiveTense::Imperfect, Person::First, Number::Singular, "hubiera"),
        subjunctive(SubjunctiveTense::Imperfect, Person::First, Number::Plural, "hubiéramos"),
        subjunctive(SubjunctiveTense::Imperfect, Person::Second, Number::Singular, "huberias"),
        subjunctive(SubjunctiveTense::Imperfect, Person::Second, Number::Plural, "hubierais"),
        subjunctive(SubjunctiveTense::Imperfect, Person::Third, Number::Singular, "hubiera"),
        subjunctive(SubjunctiveTense::Imperfect, Person::Third, Number::Plural, "hubieran"),
        // Subjunctive Future:
        subjunctive(SubjunctiveTense::Future, Person::First, Number::Singular, "hubiere"),
        subjunctive(SubjunctiveTense::Future, Person::First, Number::Plural, "hubiéremos"),
        subjunctive(SubjunctiveTense::Future, Person::Second, Number::Singular, "huberies"),
        subjunctive(SubjunctiveTense::Future, Person::Second, Number::Plural, "hubiereis"),
        subjunctive(SubjunctiveTense::Future, Person::Third, Number::Singular, "hubiere"),
        subjunctive(SubjunctiveTense::Future, Person::Third, Number::Plural, "hubieren"),
    ];
    Verb {
        stem: "hab".to_owned(),
        ending: Ending::Er,
        reflexiveness: Reflexiveness::NonReflexive,
        irregular_future_stem: Some("habr".to_owned()),
        irregular_gerund: None,
        irregular_past_participle: None,
        irregulars: HashMap::from(irregulars),
        translation: Translation::default(),
    }
}

#[rustfmt::skip]
fn estar() -> Verb {
    let indicative = |tense, person, number, word: &str| -> (Conjugation, String) {
        (Conjugation::Indicative(tense, person, number), word.to_owned())
    };
    let irregulars = [
        // Present:
        indicative(IndicativeTense::Present, Person::First, Number::Singular, "estoy"),
        indicative(IndicativeTense::Present, Person::Second, Number::Singular, "estás"),
        indicative(IndicativeTense::Present, Person::Third, Number::Singular, "está"),
        indicative(IndicativeTense::Present, Person::Third, Number::Plural, "están"),
        // Preterite:
        indicative(IndicativeTense::Preterite, Person::First, Number::Singular, "estuve"),
        indicative(IndicativeTense::Preterite, Person::First, Number::Plural, "estuvimos"),
        indicative(IndicativeTense::Preterite, Person::Second, Number::Singular, "estuviste"),
        indicative(IndicativeTense::Preterite, Person::Second, Number::Plural, "estuvisteis"),
        indicative(IndicativeTense::Preterite, Person::Third, Number::Singular, "estuvo"),
        indicative(IndicativeTense::Preterite, Person::Third, Number::Plural, "estuvieron"),
    ];
    Verb {
        stem: "est".to_owned(),
        ending: Ending::Ar,
        reflexiveness: Reflexiveness::NonReflexive,
        irregular_future_stem: None,
        irregular_gerund: None,
        irregular_past_participle: None,
        irregulars: HashMap::from(irregulars),
        translation: Translation::default(),
    }
}

#[derive(clap::Subcommand)]
enum Command {
    /// Conjugate a Spanish verb to a specific tense.
    Conjugate,
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

fn highlight_irregular_letters(verb: &Verb, conjugation: &Conjugation, irregular: &str) -> String {
    let (regular, _) = conjugate(verb, conjugation, false);
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

fn main() {
    let cli = Cli::parse();
    let mut rng = rand::thread_rng();

    let verbs: HashMap<String, Verb> = ron::de::from_str(include_str!("verbs.ron")).unwrap();
    let verbs: Vec<_> = verbs.values().cloned().collect();

    match cli.command {
        Command::Conjugate => {
            let verb = verbs.choose(&mut rng).unwrap();
            let conjugation: Conjugation = rng.gen();
            let translation = conjugate_translation(&verb.translation, &conjugation);
            let (answer, regularity) = conjugate(verb, &conjugation, true);

            println!(
                "verb: {}\ntense: {conjugation}\ntranslation: \"{translation}\"",
                verb.infinitive()
            );

            loop {
                let input = get_line().to_lowercase();
                let input = input.trim();
                match correctness(input, &answer) {
                    Correctness::Perfect => {
                        println!("correct!");
                        break;
                    }
                    Correctness::Punctuation => {
                        println!(
                            "correct, but keep punctuation in mind (\"{input}\" vs \"{answer}\")"
                        );
                        break;
                    }
                    Correctness::Wrong => {
                        println!("incorrect, want to try again? (y/n)");
                        if get_line().to_lowercase().trim() == "y" {
                            if regularity == Regularity::Irregular {
                                println!("try again! (hint: it is irregular)");
                            } else {
                                println!("try again!");
                            }
                        } else {
                            let (print, regularity) = if regularity == Regularity::Irregular {
                                let print =
                                    highlight_irregular_letters(verb, &conjugation, &answer);
                                (print, "irregular".red())
                            } else {
                                (answer, "regular".white())
                            };
                            println!("the correct answer is \"{print}\" ({regularity})");
                            break;
                        }
                    }
                }
            }
        }
    }
}
