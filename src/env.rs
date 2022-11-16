use std::{collections::HashMap, rc::Rc};

use crate::reader::MalForm;

#[derive(Clone)]
pub struct Env {
    pub outer: Option<Rc<Env>>,
    data: HashMap<String, MalForm>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            outer: None,
            data: HashMap::new(),
        }
    }

    pub fn with_outer(outer: &Env) -> Self {
        let mut env = Env::new();
        env.outer = Some(Rc::new(outer.to_owned()));
        env
    }

    pub fn set(&mut self, k: &str, v: MalForm) -> &MalForm {
        self.data.insert(k.to_owned(), v);
        self.data.get(k).unwrap()
    }

    fn find(&self, k: &str) -> Option<&Self> {
        if self.data.contains_key(k) {
            Some(self)
        } else if let Some(ref env) = self.outer {
            env.find(k)
        } else {
            None
        }
    }

    pub fn get(&self, k: &str) -> Option<&MalForm> {
        if let Some(env) = self.find(k) {
            Some(env.data.get(k).unwrap())
        } else {
            None
        }
    }
}

impl From<HashMap<String, MalForm>> for Env {
    fn from(data: HashMap<String, MalForm>) -> Self {
        Env { outer: None, data }
    }
}

impl From<&HashMap<String, MalForm>> for Env {
    fn from(data: &HashMap<String, MalForm>) -> Self {
        Env {
            outer: None,
            data: data.to_owned(),
        }
    }
}
