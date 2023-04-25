//! Interpreter.

use std::collections::HashMap;

use crate::tast::{Block, Expr, Fun, Program, Stmt, Var};
use Expr::*;
use Stmt::*;

/// Execution environment.
struct Env {
    /// Map between vars and their definitions.
    var_env: HashMap<Var, ValueRef>,
}
impl Env {
    /// Creates a new, empty environment.
    fn new() -> Self {
        Self {
            var_env: HashMap::new(),
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&ValueRef> {
        self.var_env.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    fn add_var(&mut self, name: impl Into<Var>, value: impl Into<ValueRef>) {
        self.var_env.insert(name.into(), value.into());
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

/// Interpreter for our language.
pub(crate) struct Interpreter<'a> {
    /// The execution environment stack.
    env: Vec<Env>,
    /// Map between function names and their definition.
    fun_env: &'a HashMap<String, Fun>,
}

/// Runs the interpreter on a given program.
pub fn interpret(p: Program) {
    let mut fun_env = HashMap::new();

    // Add all functions to the context.
    for f in p {
        fun_env.insert(f.name.clone(), f);
    }

    // Create the interpreter and run it.
    let mut i = Interpreter {
        env: vec![Env::default()],
        fun_env: &fun_env,
    };
    i.call("main", vec![]);
}

impl<'a> Interpreter<'a> {
    /// Shortcut to produce the result of a call to an integer binop operation `f`
    /// that takes two integers and returns a value.
    fn int_binop(
        &mut self,
        f: impl Fn(&rug::Integer, &rug::Integer) -> Value,
        args: &Vec<ValueRef>,
    ) -> Option<ValueRef> {
        let lhs = *args.get(0)?;
        let rhs = *args.get(1)?;
        match (self.get_value(lhs), self.get_value(rhs)) {
            (IntV(lhs), IntV(rhs)) if args.len() == 2 => Some(self.add_value(f(lhs, rhs))),
            _ => None,
        }
    }

    /// Checks if we can apply builtin functions to the call to `f_name(..args)`.
    fn check_builtin(&mut self, f_name: &str, args: &Vec<ValueRef>) -> Option<ValueRef> {
        match f_name {
            "+" => self.int_binop(|x, y| IntV((x + y).into()), args),
            "-" => self.int_binop(|x, y| IntV((x - y).into()), args),
            "*" => self.int_binop(|x, y| IntV((x * y).into()), args),
            "/" => self.int_binop(|x, y| IntV((x / y).into()), args),
            "%" => self.int_binop(|x, y| IntV((x % y).into()), args),
            "==" => self.int_binop(|x, y| BoolV(x == y), args),
            "!=" => self.int_binop(|x, y| BoolV(x != y), args),
            ">=" => self.int_binop(|x, y| BoolV(x >= y), args),
            ">" => self.int_binop(|x, y| BoolV(x > y), args),
            "<=" => self.int_binop(|x, y| BoolV(x <= y), args),
            "<" => self.int_binop(|x, y| BoolV(x < y), args),
            _ => None,
        }
    }

    /// Executes a call to a function in scope.
    fn call(&mut self, f_name: impl AsRef<str>, args: Vec<ValueRef>) -> ValueRef {
        let f_name = f_name.as_ref();

        // Override in case of builtin.
        if let Some(res) = self.check_builtin(f_name, &args) {
            res
        } else {
            self.push_scope();
            let f = self.fun_env.get(f_name).unwrap();
            // Introduce arguments in the typing context
            for (arg, value) in f.params.iter().zip(args.iter()) {
                self.add_var(arg.name.clone(), *value);
            }

            let res = self.visit_block(&f.body).to_owned();
            self.pop_scope();

            res
        }
    }

    /// Creates a new scope.
    fn push_scope(&mut self) {
        self.env.push(Env::default());
    }

    /// Pops and removes the current scope.
    fn pop_scope(&mut self) -> Option<()> {
        // Refuse the pop the static environment
        if self.env.len() >= 2 {
            self.env.pop().map(|_| ())
        } else {
            None
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<ValueRef> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env.iter().rev().find_map(|e| e.get_var(v)).copied()
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, name: impl Into<Var>, value: impl Into<ValueRef>) {
        self.env.last_mut().unwrap().add_var(name.into(), value);
    }

    /// Adds a value to the dynamic store/slab.
    fn add_value(&mut self, _value: Value) -> ValueRef {
        todo!()
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value(&self, _value: ValueRef) -> &Value {
        todo!()
    }
}

/// Values in our language.
pub enum Value {
    /// Unit value.
    UnitV,
    /// Integer value.
    IntV(rug::Integer),
    /// Boolean value.
    BoolV(bool),
}

use Value::*;

impl Value {
    /// Truthiness of the value.
    pub fn truth(&self) -> bool {
        if let BoolV(b) = self {
            *b
        } else {
            panic!("Requesting the truth value of something which is not a boolean")
        }
    }
}

/// A reference to a value.
#[derive(Clone, Copy)]
pub struct ValueRef(usize);

impl Interpreter<'_> {
    /// Executes an expression.
    fn visit_expr(&mut self, e: &Expr) -> ValueRef {
        match e {
            UnitE => self.add_value(UnitV),
            IntegerE(i) => self.add_value(IntV(i.clone())),
            VarE(v) => self
                .get_var(v)
                .expect("Runtime error: unknown variable {v}"),
            CallE { name, args } => {
                let args = args.iter().map(|arg| self.visit_expr(arg)).collect();
                self.call(name, args)
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond);
                if self.get_value(cond).truth() {
                    self.visit_block(iftrue)
                } else {
                    self.visit_block(iffalse)
                }
            }
            BlockE(box e) => self.visit_block(e),
            Copy(box b) => self.visit_block(b), // no-op
            Own(box b) => self.visit_block(b).to_owned(),
        }
    }

    /// Executes a block.
    fn visit_block(&mut self, b: &Block) -> ValueRef {
        for stmt in &b.stmts {
            self.visit_stmt(stmt);
        }
        self.visit_expr(&b.ret)
    }

    /// Executes a statement.
    fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Declare(v, e) => {
                let e = self.visit_expr(e);
                self.add_var(v.name.clone(), e);
            }
            Assign(v, e) => {
                let e = self.visit_expr(e);
                self.add_var(v.clone(), e);
            }
            ExprS(e) => {
                self.visit_expr(e);
            }
            While { cond, body } => {
                while {
                    let e = self.visit_expr(cond);
                    self.get_value(e).truth()
                } {
                    self.visit_block(body);
                }
            }
        }
    }
}
