use std::{collections::HashMap, rc::Rc};

use crate::{printer::pr_str, reader::*};

fn new_fn<F: Fn(Vec<MalForm>) -> MalForm + 'static>(f: F) -> MalForm {
    MalForm::FnSpecial(MalFnSpecial::new(Rc::new(f)))
}

fn new_num_fn<F>(f: F) -> MalForm
where
    F: Fn(Vec<i64>) -> i64 + 'static,
{
    new_fn(move |args: Vec<MalForm>| -> MalForm {
        MalForm::Int(f(args
            .iter()
            .map(|x| {
                let MalForm::Int(n) = x else {
                               panic!("not a number");
                           };
                *n
            })
            .collect::<Vec<i64>>()))
    })
}

fn cmp(a: &MalForm, b: &MalForm) -> i64 {
    fn eq<T: PartialEq>(x: T, y: T) -> i64 {
        !(x == y) as i64
    }
    match (a, b) {
        (MalForm::Nil, MalForm::Nil) => 0,
        (MalForm::Bool(x), MalForm::Bool(y)) => eq(x, y),
        (MalForm::Int(x), MalForm::Int(y)) => x - y,
        (MalForm::Symbol(x), MalForm::Symbol(y)) | (MalForm::String(x), MalForm::String(y)) => {
            eq(x, y)
        }

        (MalForm::List(x), MalForm::List(y)) | (MalForm::Vector(x), MalForm::Vector(y)) => {
            !(x.len() == y.len()
                && x.iter()
                    .zip(y.iter())
                    .map(|(x, y)| cmp(x, y))
                    .all(|x| x == 0)) as i64
        }
        (MalForm::Map(x), MalForm::Map(y)) => {
            !(x.len() == y.len()
                && x.keys().all(|k| {
                    if let (Some(xv), Some(yv)) = (x.get(k), y.get(k)) {
                        cmp(xv, yv) == 0
                    } else {
                        false
                    }
                })) as i64
        }

        _ => 1,
    }
}

fn new_cmp_fn<G>(cmp_cond: G) -> MalForm
where
    G: Fn(i64) -> bool + 'static,
{
    new_fn(move |args: Vec<MalForm>| -> MalForm {
        MalForm::Bool(cmp_cond(cmp(&args[0], &args[1])))
    })
}

thread_local! {
#[allow(non_upper_case_globals)]
pub static ns: HashMap<String, MalForm> = HashMap::from([
    // numeric
    (
        "+".to_owned(),
        new_num_fn(|xs| xs.into_iter().fold(0, |a, b| a + b)),
    ),
    (
        "-".to_owned(),
        new_num_fn(|xs| xs.into_iter().reduce(|a, b| a - b).unwrap()),
    ),
    (
        "*".to_owned(),
        new_num_fn(|xs| xs.into_iter().fold(1, |a, b| a * b)),
    ),
    (
        "/".to_owned(),
        new_num_fn(|xs| xs.into_iter().reduce(|a, b| a / b).unwrap()),
    ),
    // cmp
    ("=".to_owned(), new_cmp_fn(|x| x == 0)),
    ("<".to_owned(), new_cmp_fn(|x| x < 0)),
    ("<=".to_owned(), new_cmp_fn(|x| x <= 0)),
    (">".to_owned(), new_cmp_fn(|x| x > 0)),
    (">=".to_owned(), new_cmp_fn(|x| x >= 0)),
    // printing
    (
        "pr-str".to_owned(),
        new_fn(|args| {
            MalForm::String(
                args.iter()
                    .map(|x| pr_str(x, true))
                    .collect::<Vec<String>>()
                    .join(" "),
            )
        }),
    ),
    (
        "str".to_owned(),
        new_fn(|args| {
            MalForm::String(
                args.iter()
                    .map(|x| pr_str(x, false))
                    .collect::<Vec<String>>()
                    .join(""),
            )
        }),
    ),
    (
        "prn".to_owned(),
        new_fn(|args| {
            println!(
                "{}",
                args.iter()
                    .map(|x| pr_str(x, true))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            MalForm::Nil
        }),
    ),
    (
        "println".to_owned(),
        new_fn(|args| {
            println!(
                "{}",
                args.iter()
                    .map(|x| pr_str(x, false))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            MalForm::Nil
        }),
    ),
    // seqs
    (
        "list".to_owned(),
        new_fn(|args| MalForm::List(args.to_vec())),
    ),
    (
        "list?".to_owned(),
        new_fn(|args| {
            MalForm::Bool(if let Some(MalForm::List(_)) = args.first() {
                true
            } else {
                false
            })
        }),
    ),
    (
        "empty?".to_owned(),
        new_fn(|args| {
            MalForm::Bool(
                match args.first() {
                    Some(MalForm::List(v) | MalForm::Vector(v)) => v.is_empty(),
                    Some(MalForm::String(s)) => s.is_empty(),
                    _ => false,
                }
            )
        }),
    ),
    (
        "count".to_owned(),
        new_fn(|args| {
            MalForm::Int(
                match args.first() {
                    Some(MalForm::Nil) => 0,
                    Some(MalForm::List(v) | MalForm::Vector(v)) => v.len() as i64,
                    Some(MalForm::String(s)) => s.chars().count() as i64,
                    _ => -1,
                }
            )
        }),
    ),
]);
}
