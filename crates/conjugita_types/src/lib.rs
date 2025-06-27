use multimap::MultiMap;
use rand::distr::{Distribution, StandardUniform};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Ending {
    Ar,
    Er,
    Ir,
}

impl Ending {
    pub fn past_ending_category(&self) -> &'static str {
        match self {
            Ending::Ar => "ar",
            Ending::Er | Ending::Ir => "er/ir",
        }
    }
}

impl fmt::Display for Ending {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ar => write!(f, "ar"),
            Self::Er => write!(f, "er"),
            Self::Ir => write!(f, "ir"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Regularity {
    Regular,
    Irregular,
}

impl Regularity {
    pub fn combine(self, other: Self) -> Self {
        if self == Self::Irregular || other == Self::Irregular {
            Self::Irregular
        } else {
            Self::Regular
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Reflexiveness {
    Reflexive,
    NonReflexive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub enum TranslationKind {
    #[default]
    ToDoSomething,
    ToBeSomething,
    ToBe,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Translation {
    pub kind: TranslationKind,
    pub present: String,
    pub present_third_person: String,
    pub past: String,
    pub past_participle: String,
    pub gerund: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Verb {
    pub stem: String,
    pub ending: Ending,
    pub reflexiveness: Reflexiveness,
    pub irregular_future_stem: Option<String>,
    pub irregular_gerund: Option<String>,
    pub irregular_past_participle: Option<String>,
    pub irregulars: MultiMap<Conjugation, String>,
    pub translation: Translation,
}

impl Verb {
    pub fn reflexive_pronoun(&self, person: Person, number: Number) -> Option<&'static str> {
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

    pub fn infinitive(&self) -> String {
        let reflexive_pronoun = match self.reflexiveness {
            Reflexiveness::Reflexive => "se",
            Reflexiveness::NonReflexive => "",
        };
        format!("{}{reflexive_pronoun}", self.infinitive_without_reflexive())
    }

    pub fn infinitive_without_reflexive(&self) -> String {
        let ending = match self.ending {
            Ending::Ar => "ar",
            Ending::Er => "er",
            Ending::Ir => "ir",
        };
        format!("{}{ending}", self.stem)
    }

    pub fn future_stem(&self) -> (String, Regularity) {
        self.irregular_future_stem
            .clone()
            .map(|stem| (stem, Regularity::Irregular))
            .unwrap_or_else(|| (self.infinitive_without_reflexive(), Regularity::Regular))
    }

    pub fn gerund(&self) -> (String, Regularity) {
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

    pub fn past_participle(&self) -> (String, Regularity) {
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
pub enum Person {
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

impl Distribution<Person> for StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Person {
        match rng.random_range(0..3) {
            0 => Person::First,
            1 => Person::Second,
            _ => Person::Third,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Number {
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

impl Distribution<Number> for StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Number {
        match rng.random_range(0..2) {
            0 => Number::Singular,
            _ => Number::Plural,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IndicativeTense {
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

impl Distribution<IndicativeTense> for StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> IndicativeTense {
        match rng.random_range(0..5) {
            0 => IndicativeTense::Present,
            1 => IndicativeTense::Preterite,
            2 => IndicativeTense::Imperfect,
            3 => IndicativeTense::Conditional,
            _ => IndicativeTense::Future,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SubjunctiveTense {
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

impl Distribution<SubjunctiveTense> for StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> SubjunctiveTense {
        match rng.random_range(0..3) {
            0 => SubjunctiveTense::Present,
            1 => SubjunctiveTense::Imperfect,
            _ => SubjunctiveTense::Future,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ImperativeTense {
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

impl Distribution<ImperativeTense> for StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> ImperativeTense {
        match rng.random_range(0..2) {
            0 => ImperativeTense::Affirmative,
            _ => ImperativeTense::Negative,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Conjugation {
    Indicative(IndicativeTense, Person, Number),
    Subjunctive(SubjunctiveTense, Person, Number),
    Imperative(ImperativeTense, Person, Number),
    Progressive(IndicativeTense, Person, Number),
    Perfect(IndicativeTense, Person, Number),
    // SubjunctiveProgressive
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

impl Distribution<Conjugation> for StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Conjugation {
        match rng.random_range(0..6) {
            0 => Conjugation::Indicative(rng.random(), rng.random(), rng.random()),
            1 => Conjugation::Subjunctive(rng.random(), rng.random(), rng.random()),
            2 => loop {
                let (person, number) = (rng.random(), rng.random());
                if !(person == Person::First && number == Number::Singular) {
                    break Conjugation::Imperative(rng.random(), person, number);
                }
            },
            3 => Conjugation::Progressive(rng.random(), rng.random(), rng.random()),
            4 => Conjugation::Perfect(rng.random(), rng.random(), rng.random()),
            _ => Conjugation::SubjunctivePerfect(rng.random(), rng.random(), rng.random()),
        }
    }
}
