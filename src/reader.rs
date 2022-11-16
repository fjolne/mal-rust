use std::{collections::HashMap, rc::Rc};

use crate::env::Env;
use regex::Regex;

struct Reader {
    tokens: Vec<String>,
    cursor: usize,
}

impl Reader {
    fn next(&mut self) -> &str {
        let token = &self.tokens[self.cursor];
        self.cursor += 1;
        token
    }
    fn peek(&self) -> &str {
        &self.tokens[self.cursor]
    }
    fn eof(&self) -> bool {
        self.cursor >= self.tokens.len()
    }
}

fn tokenize(text: String) -> Vec<String> {
    let re = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
        .unwrap();
    re.captures_iter(&text)
        .map(|cap| String::from(cap.get(1).unwrap().as_str()))
        .filter(|token| !token.is_empty())
        .collect::<Vec<String>>()
}

#[derive(PartialEq, Debug)]
pub struct MalErr(String);

impl std::fmt::Display for MalErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for MalErr {
    fn from(s: &str) -> Self {
        MalErr(s.to_owned())
    }
}

impl From<String> for MalErr {
    fn from(s: String) -> Self {
        MalErr(s)
    }
}

impl From<std::io::Error> for MalErr {
    fn from(err: std::io::Error) -> Self {
        MalErr(err.to_string())
    }
}

pub fn read_str(text: String) -> Result<MalForm, MalErr> {
    read_form(&mut Reader {
        tokens: tokenize(text),
        cursor: 0,
    })
}

pub trait Invoke {
    fn invoke(&self, args: Vec<MalForm>) -> Result<MalForm, MalErr>;
}

#[derive(Clone)]
pub struct MalFn {
    pub env: Box<Env>,
    pub params: Vec<String>,
    pub body: Box<MalForm>,
}

impl Invoke for MalFn {
    fn invoke(&self, args: Vec<MalForm>) -> Result<MalForm, MalErr> {
        let mut let_body = vec![MalForm::Symbol("let*".to_owned())];
        let mut bindings = vec![];
        for (i, p) in self.params.iter().enumerate() {
            if p == "&" {
                bindings.push(MalForm::Symbol(self.params[i + 1].clone()));
                bindings.push(MalForm::Vector(args[i..].to_vec()));
                break;
            }
            bindings.push(MalForm::Symbol(p.clone()));
            bindings.push(args[i].clone());
        }
        let_body.push(MalForm::List(bindings));
        let_body.push((*self.body).clone());
        crate::EVAL(MalForm::List(let_body), &mut self.env.clone())
    }
}

type MalFunction = dyn Fn(Vec<MalForm>) -> MalForm;

#[derive(Clone)]
pub struct MalFnSpecial {
    pub f: Rc<MalFunction>,
}

impl MalFnSpecial {
    pub fn new(f: Rc<MalFunction>) -> Self {
        MalFnSpecial { f }
    }
}

impl Invoke for MalFnSpecial {
    fn invoke(&self, args: Vec<MalForm>) -> Result<MalForm, MalErr> {
        Ok((*self.f)(args.to_vec()))
    }
}

#[derive(Clone)]
pub enum MalForm {
    List(Vec<MalForm>),
    Vector(Vec<MalForm>),
    Map(HashMap<String, MalForm>),

    Nil,
    Bool(bool),
    Int(i64),
    Symbol(String),
    String(String),

    Fn(MalFn),
    FnSpecial(MalFnSpecial),
}

impl std::fmt::Debug for MalForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::List(arg0) => f.debug_tuple("List").field(arg0).finish(),
            Self::Vector(arg0) => f.debug_tuple("Vector").field(arg0).finish(),
            Self::Map(arg0) => f.debug_tuple("Map").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Nil => write!(f, "Nil"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Fn(_arg0) => f.debug_tuple("Fn").field(&"f").finish(),
            Self::FnSpecial(_arg0) => f.debug_tuple("FnSpecial").field(&"f").finish(),
        }
    }
}

impl PartialEq for MalForm {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Vector(l0), Self::Vector(r0)) => l0 == r0,
            (Self::Map(l0), Self::Map(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Fn(_l0), Self::Fn(_r0)) => false,
            (Self::FnSpecial(_l0), Self::FnSpecial(_r0)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

fn read_form(r: &mut Reader) -> Result<MalForm, MalErr> {
    if r.eof() {
        Err("Unexpected end of input!".into())
    } else if r.peek() == "(" {
        Ok(MalForm::List(read_list(r, ")")?))
    } else if r.peek() == "[" {
        Ok(MalForm::Vector(read_list(r, "]")?))
    } else if r.peek() == "{" {
        let xs = read_list(r, "}")?;
        if xs.len() % 2 != 0 {
            return Err("odd number of map forms".into());
        }
        let kvs: Result<HashMap<String, MalForm>, MalErr> = xs
            .chunks(2)
            .map(|kv| {
                let [k, v] = kv else { panic!("this should never have happened"); };
                Ok((
                    match k {
                        MalForm::String(s) => s.clone(),
                        _ => return Err("map key should be a string or a keyword".into()),
                    },
                    v.clone(),
                ))
            })
            .collect();
        Ok(MalForm::Map(kvs?))
    } else {
        read_atom(r)
    }
}

fn read_list(r: &mut Reader, end_token: &str) -> Result<Vec<MalForm>, MalErr> {
    let mut v: Vec<MalForm> = vec![];
    r.next();
    while !r.eof() && r.peek() != end_token {
        v.push(read_form(r)?)
    }
    if r.eof() {
        return Err("unbalanced".into());
    }
    r.next();
    Ok(v)
}

fn read_atom(r: &mut Reader) -> Result<MalForm, MalErr> {
    let t = r.next();
    let c = t.chars().nth(0).unwrap();
    if Regex::new(r"^[+-]?\d+$").unwrap().is_match(t) {
        Ok(MalForm::Int(t.parse::<i64>().unwrap()))
    } else if c.is_alphabetic() || "+-*/=&<>".contains(c) {
        Ok(match t {
            "nil" => MalForm::Nil,
            "true" => MalForm::Bool(true),
            "false" => MalForm::Bool(false),
            _ => MalForm::Symbol(String::from(t)),
        })
    } else if c == '"' {
        if t.len() >= 2 && t.ends_with("\"") {
            Ok(MalForm::String(String::from(&t[1..t.len() - 1])))
        } else {
            Err("unbalanced".into())
        }
    } else if c == ':' {
        if t.len() > 1 {
            Ok(MalForm::String(new_keyword(&t[1..])))
        } else {
            Err("keyword cannot be empty".into())
        }
    } else {
        Err(format!("unknown token: {t}").into())
    }
}

const KEYWORD_SPECIFIER: char = '\u{29E}';

fn new_keyword(s: &str) -> String {
    format!("{}{}", KEYWORD_SPECIFIER, s)
}

pub fn is_keyword_str(s: &str) -> bool {
    s.len() > 1 && s.chars().next().unwrap() == KEYWORD_SPECIFIER
}

fn is_keyword(form: &MalForm) -> bool {
    if let MalForm::String(s) = form {
        is_keyword_str(s)
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_test() {
        assert_eq!(
            tokenize(String::from(", ( + 1, 2, ) ,")),
            vec!["(", "+", "1", "2", ")"]
        );
        assert_eq!(
            tokenize(String::from(r#"(str "he(ll)o" "world")"#)),
            vec!["(", "str", r#""he(ll)o""#, r#""world""#, ")"]
        );
        assert_eq!(tokenize(String::from(r#""okay""#)), vec![r#""okay""#]);
        assert_eq!(tokenize(String::from(r#""okay"#)), vec![r#""okay"#]);
        assert_eq!(tokenize(String::from(r#"okay""#)), vec!["okay", "\""]);
        assert_eq!(
            tokenize(String::from(r#""ok"ay""#)),
            vec![r#""ok""#, "ay", "\""]
        );
        assert_eq!(tokenize(String::from("\"")), vec!["\""]);
    }
    #[test]
    fn read_test() {
        assert_eq!(
            read_str(String::from("(+ 1 2)")),
            Ok(MalForm::List(vec![
                MalForm::Symbol(String::from("+")),
                MalForm::Int(1),
                MalForm::Int(2)
            ]))
        );
        assert_eq!(
            read_str(String::from("(+ 17 (* 25 36))")),
            Ok(MalForm::List(vec![
                MalForm::Symbol(String::from("+")),
                MalForm::Int(17),
                MalForm::List(vec![
                    MalForm::Symbol(String::from("*")),
                    MalForm::Int(25),
                    MalForm::Int(36)
                ])
            ])),
        );
    }

    #[test]
    fn keyword_test() {
        let kw = read_str(":okay".to_string());
        assert_eq!(
            kw,
            Ok(MalForm::String(format!("{}okay", KEYWORD_SPECIFIER)))
        );
        assert!(is_keyword(&kw.unwrap()));
        let nkw = read_str(r#""okay""#.to_string());
        assert_eq!(nkw, Ok(MalForm::String("okay".to_string())));
        assert!(!is_keyword(&nkw.unwrap()));
    }

    #[test]
    fn map_test() {
        assert_eq!(read_str("{}".to_string()), Ok(MalForm::Map(HashMap::new())));
        assert_eq!(
            read_str(r#"{ "a" 1 }"#.to_string()),
            Ok(MalForm::Map(HashMap::from([(
                "a".to_string(),
                MalForm::Int(1)
            )])))
        );
        assert_eq!(
            read_str(r#"{ :a 1 }"#.to_string()),
            Ok(MalForm::Map(HashMap::from([(
                new_keyword("a"),
                MalForm::Int(1)
            )])))
        );
        assert_eq!(
            read_str(r#"{ "a" 1 :a 2 }"#.to_string()),
            Ok(MalForm::Map(HashMap::from([
                ("a".to_string(), MalForm::Int(1)),
                (new_keyword("a"), MalForm::Int(2))
            ])))
        );
    }
}
