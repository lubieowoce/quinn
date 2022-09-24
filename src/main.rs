use std::collections::HashMap;



type Value = i64;
type VarName<'a> = &'a str;
type Env<'a> = HashMap<VarName<'a>, Value>;

#[derive(Debug, Clone)]
enum Expr<'a> {
    Num(i64),
    Add(&'a Expr<'a>, &'a Expr<'a>),
    Mul(&'a Expr<'a>, &'a Expr<'a>),
    Div(&'a Expr<'a>, &'a Expr<'a>),
    Var(VarName<'a>),
    Let(VarName<'a>, &'a Expr<'a>, &'a Expr<'a>),
}

const TEST1: Expr = 
    Expr::Add(
        &Expr::Num(1),
        &Expr::Mul(
            &Expr::Num(2),
            &Expr::Div(&Expr::Num(2), &Expr::Num(1))
        )
    );


fn main() {
    let test2 = 
    &Expr::Let(
        "x",
        &Expr::Num(1),
        &Expr::Add(
            &Expr::Var("x"),
            &Expr::Let("x", &Expr::Num(3), &Expr::Var("x"))
        )
    );
    println!("{:?}", test2);
    println!("{:?}", eval(&test2, &HashMap::new()));
}

fn eval(expr: &Expr, env: &Env) -> Result<i64, String> {
    match expr {
        Expr::Num(n) => Ok(*n),
        Expr::Add(left, right) => Ok(eval(*left, env)? + eval(*right, env)?),
        Expr::Mul(left, right) => Ok(eval(*left, env)? * eval(*right, env)?),
        Expr::Div(left, right) => {
            let r = eval(right, env)?;
            if r == 0 {
                Err("division by zero".to_string())
            } else {
                Ok(eval(left, env)? / r)
            }
        },
        Expr::Let(name, rhs, body) => {
            let rhs_val = eval(rhs, env)?;
            let mut env2 = env.clone();
            env2.insert(name.clone(), rhs_val);
            eval(body, &env2)
        }
        Expr::Var(name) => {
            match env.get(name) {
                Some(val) => Ok(*val),
                None => Err("Undefined variable: ".to_owned() + name)
            }
        }
        _ => Err("Not implemented".to_string())
    }
}
