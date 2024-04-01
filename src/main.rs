use std::{
    fmt::{Display, Formatter, Error}, 
    str::Chars, io, collections::{HashSet, HashMap}, cell::RefCell
};

#[derive(Clone, Debug)]
enum Expr {
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Expr::Var(s) => write!(f, "{}", s),
            Expr::Abs(s, e) => write!(f, "λ{}.{}", s, e),
            Expr::App(u, v) => {
                match u.as_ref() {
                    Expr::Abs(_,_) => write!(f, "({}) ", u),
                    _ => write!(f, "{} ", u)
                }?;
                match v.as_ref() {
                    Expr::Abs(_,_) => write!(f, "({})", v),
                    Expr::App(_,_) => write!(f, "({})", v),
                    _ => write!(f, "{}", v)
                }
            }
        }
    }
}

enum Statement {
    Expr(Expr),
    Assignment(String, Expr)
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => write!(f, "{}", e),
            Statement::Assignment(v, e) => write!(f, "{} = {}", v, e)
        }
    }
}

#[derive(Debug)]
enum Token {
    LParen(usize),
    RParen(usize),
    Lambda(usize),
    Term(usize, String),
    Equals(usize)
}

fn tokenize(input: &mut Chars) -> Vec<Token> {
    let mut res = Vec::new();
    let mut current_term = String::new();
    let mut char_position = 0;
    while let Some(ch) = input.next() {
        char_position += 1;
        let mut next_token = None;
        match ch {
            '\\' | 'λ' => next_token = Some(Token::Lambda(char_position)),
            ch if char::is_whitespace(ch) || ch == '.' => (),
            '(' => next_token = Some(Token::LParen(char_position)),
            ')' => next_token = Some(Token::RParen(char_position)),
            '=' => next_token = Some(Token::Equals(char_position)),
            _ => {
                current_term.push(ch);
                continue;
            }
        }
        if !current_term.is_empty() {
            res.push(Token::Term(
                char_position - current_term.len(),
                current_term.clone(),
            ));
            current_term.clear();
        }
        if let Some(token) = next_token {
            res.push(token);
        }
    }
    res
}

#[derive(Debug)]
enum TreeifyError {
    UnclosedParen(usize),
    UnopenedParen(usize),
    MissingLambdaVar(usize),
    MissingLambdaBody(usize),
    EmptyExprList,
    IllegalAssignment(usize)
}

fn treeify(tokens: &[Token]) -> Result<Expr, TreeifyError> {
    let mut i = 0;
    let mut res = Vec::new();
    while i < tokens.len() {
        match &tokens[i] {
            Token::LParen(paren_idx) => {
                let mut nesting = 0;
                let mut j = i+1;
                let mut pushed_expr = false;
                while j < tokens.len() {
                    match tokens[j] {
                        Token::LParen(_) => {
                            nesting += 1;
                        }
                        Token::RParen(_) => {
                            if nesting == 0 {
                                let inside_expr = treeify(&tokens[i+1..=j-1])?;
                                res.push(inside_expr);
                                pushed_expr = true;
                                break;
                            }
                            nesting -= 1;
                        },
                        _ => ()
                    }
                    j += 1;
                }
                if !pushed_expr {
                    return Err(TreeifyError::UnclosedParen(*paren_idx));
                }
                i = j;
            },
            Token::RParen(paren_idx) => {
                return Err(TreeifyError::UnopenedParen(*paren_idx));
            },
            Token::Lambda(lambda_idx) => {
                if tokens.len() <= i+2 {
                    return Err(TreeifyError::MissingLambdaBody(*lambda_idx));
                }
                if let Some(Token::Term(_, term_str)) = tokens.get(i+1) {
                    let rest = treeify(&tokens[i+2..])?;
                    res.push(Expr::Abs(term_str.to_string(), Box::new(rest)));
                    i = tokens.len();
                }
                else {
                    return Err(TreeifyError::MissingLambdaVar(*lambda_idx));
                }
            },
            Token::Term(_, term_str) => {
                res.push(Expr::Var(term_str.to_string()));
            },
            Token::Equals(equals_idx) => return Err(TreeifyError::IllegalAssignment(*equals_idx))
        }
        i += 1;
    }
    match res.into_iter().reduce(|acc, item| Expr::App(Box::new(acc), Box::new(item))) {
        Some(res) => Ok(res),
        None => Err(TreeifyError::EmptyExprList)
    }
}

fn build_statement(tokens: &[Token]) -> Result<Statement, TreeifyError> {
    if tokens.len() > 2 {
        if let Some(Token::Term(_, s)) = tokens.get(0) {
            if let Some(Token::Equals(_)) = tokens.get(1) {
                let exp = treeify(&tokens[2..])?;
                return Ok(Statement::Assignment(s.clone(), exp));
            }
        }
    }
    
    let exp = treeify(tokens)?;
    return Ok(Statement::Expr(exp));
}

fn free_vars(e: &Expr) -> HashSet<String> {
    let mut free_vars = HashSet::new();
    let mut bound_vars = HashSet::new();
    fn recur(e: &Expr, fv: &mut HashSet<String>, bv: &mut HashSet<String>) {
        match e {
            Expr::Var(v) => {
                if !bv.contains(v) {
                    fv.insert(String::from(v));
                }
            },
            Expr::Abs(v, body) => {
                // insert returns true if thing was inserted, false if it already existed
                // if it was inserted we need to remove it, if it already existed then we don't
                let need_to_remove = bv.insert(String::from(v));
                recur(body, fv, bv);
                if need_to_remove { bv.remove(v); }
            },
            Expr::App(l, r) => {
                recur(l, fv, bv);
                recur(r, fv, bv);
            }
        }
    }
    recur(e, &mut free_vars, &mut bound_vars);
    free_vars
}

thread_local!(static DISAMBIGUATE_CTR: RefCell<u64> = RefCell::new(0));

fn disambiguate(w: &str) -> String {
    DISAMBIGUATE_CTR.with(|c| {
        let mut ctr = c.borrow_mut();
        *ctr += 1;
        format!("{}_{}", w, ctr)
    })
}

fn alpha_convert(v: &str, body: Expr) -> (String, Expr) {
    let new_var = disambiguate(v);
    let new_body = substitute(body, v, &Expr::Var(new_var.clone()));
    (new_var, new_body)
}

fn beta_reduce(abs: Expr, val: Expr) -> Expr {
    match abs {
        Expr::Abs(var, body) => {
            let res = substitute(*body, &var, &val);
            res
        },
        _ => panic!("can't apply to non-abstraction")
    }
}

fn substitute(root: Expr, var: &str, val: &Expr) -> Expr {
    match root {
        Expr::Var(v) => {
            if v == var {
                val.clone()
            } else {
                Expr::Var(v)
            }
        },
        Expr::Abs(v, body) => {
            if v == var {
                Expr::Abs(v, body)
            } else if free_vars(val).contains(&v) {
                let (nv, nb) = alpha_convert(&v, *body);
                Expr::Abs(nv, Box::new(substitute(nb, var, val)))
            } else {
                Expr::Abs(v, Box::new(substitute(*body, var, val)))
            }
        },
        Expr::App(l, r) => {
            Expr::App(
                Box::new(substitute(*l, var, val)), 
                Box::new(substitute(*r, var, val))
            )
        }
    }
}

// fn reduce_aoe(e: Expr, ctx: &HashMap<String, Expr>) -> Expr {
//     match e {
//         Expr::App(l, r) => {
//             let (l, r) = (reduce_aoe(*l, ctx), reduce_aoe(*r, ctx));
//             match l {
//                 Expr::Abs(_, _) => reduce_aoe(beta_reduce(l, r), ctx),
//                 _ => Expr::App(Box::new(l), Box::new(r))
//             }
//         },
//         Expr::Var(v) => {
//             if let Some(exp) = ctx.get(&v) {
//                 exp.clone()
//             }
//             else {
//                 Expr::Var(v)
//             }
//         }
//         _ => e
//     }
// }

fn reduce_lazy(e: Expr, ctx: &HashMap<String, Expr>) -> Expr {
    match e {
        Expr::App(l, r) => {
            let l = reduce_lazy(*l, ctx);
            match l {
                Expr::Abs(_, _) => reduce_lazy(beta_reduce(l, *r), ctx),
                _ => Expr::App(Box::new(l), Box::new(reduce_lazy(*r, ctx)))
            }
        },
        Expr::Var(v) => {
            if let Some(exp) = ctx.get(&v) {
                exp.clone()
            }
            else {
                Expr::Var(v)
            }
        }
        _ => e
    }
}

fn main() {
    let stdin = io::stdin();
    let mut ctx = HashMap::<String, Expr>::new();
    loop {
        let mut buf = String::new();
        stdin.read_line(&mut buf).expect("could not read from stdin");
        if buf.starts_with("exit") {
            println!("bye");
            break;
        }
        let res = tokenize(&mut buf.chars());
        let res = build_statement(&res)
            .expect("could not parse expr");
        match res {
            Statement::Assignment(v, e) => {
                let res = reduce_lazy(e, &ctx);
                println!("-> {} = {}", v, res);
                ctx.insert(v, res);
            },
            Statement::Expr(e) => {
                let res = reduce_lazy(e, &ctx);
                println!("-> {}", res);
            }
        }
    }
}
