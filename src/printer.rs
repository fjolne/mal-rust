use crate::reader::MalForm;

fn pr_string(s: &str, print_readably: bool) -> String {
    if crate::reader::is_keyword_str(s) {
        format!(":{}", &s[2..])
    } else {
        format!(r#""{s}""#)
    }
}

pub fn pr_str(form: &MalForm, print_readably: bool) -> String {
    fn pr_seq(v: &Vec<MalForm>, print_readably: bool) -> String {
        v.iter()
            .map(|x| pr_str(x, print_readably))
            .collect::<Vec<String>>()
            .join(" ")
    }
    match form {
        MalForm::List(v) => format!("({})", pr_seq(&v, print_readably)),
        MalForm::Vector(v) => format!("[{}]", pr_seq(&v, print_readably)),
        MalForm::Map(m) => format!(
            "{{{}}}",
            m.iter()
                .map(|(k, v)| format!(
                    "{} {}",
                    pr_string(k, print_readably),
                    pr_str(v, print_readably)
                ))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        MalForm::Bool(b) => String::from(if *b == true { "true" } else { "false" }),
        MalForm::Int(n) => n.to_string(),
        MalForm::Nil => String::from("nil"),
        MalForm::Symbol(s) => s.clone(),
        MalForm::String(s) => pr_string(s, print_readably),
        MalForm::Fn(_) => "#<function>".into(),
        MalForm::FnSpecial(_) => "#<builtin function>".into(),
    }
}

#[cfg(test)]
mod tests {
    use crate::reader::read_str;

    use super::*;

    #[test]
    fn pr_str_test() {
        let text = "(+ 17 (* 25 36))";
        assert_eq!(pr_str(&read_str(String::from(text)).unwrap(), true), text);
    }

    #[test]
    fn map_test() {
        assert_eq!(
            pr_str(
                &read_str(String::from(r#"{ "a" { :c 0 } "b" 1 }"#)).unwrap(),
                true
            ),
            r#"{"a" {:c 0}, "b" 1}"#
        )
    }
}
