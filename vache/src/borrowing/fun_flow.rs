//! Extending the signatures of functions with flow information to tell the
//! relationships between input and outputs.
//!
//! In other words, what input flows into what output.

use std::collections::HashMap;
use std::default::default;

use itertools::Itertools;

use crate::mir::{Instr, InstrKind, Pointer, Reference};

/// Argument number.
type ArgNb = usize;
/// Argument dependencies.
type ArgDeps = Vec<ArgNb>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
/// Compile-time dependencies between variables.
pub struct FunFlow {
    /// For each argument, tell on which argument the result depends.
    /// The index is the position in the argument list of the function.
    /// Always in the same order as the function arguments.
    args: HashMap<ArgNb, ArgDeps>,
    /// Dependencies of the returned value.
    ret: ArgDeps,
}

impl FunFlow {
    /// Creates a new function flow with specified dependencies for the input
    /// and the output.
    pub fn new(
        arg_deps: impl IntoIterator<Item = (ArgNb, impl IntoIterator<Item = ArgNb>)>,
        ret_deps: impl IntoIterator<Item = ArgNb>,
    ) -> Self {
        Self {
            args: arg_deps
                .into_iter()
                .map(|(nb, arg)| (nb, arg.into_iter().collect::<ArgDeps>()))
                .collect::<HashMap<ArgNb, ArgDeps>>(),
            ret: ret_deps.into_iter().collect(),
        }
    }

    /// Creates a new function flow with dependencies only for returned values.
    pub fn new_only_ret(deps: impl IntoIterator<Item = ArgNb>) -> Self {
        Self {
            args: default(),
            ret: deps.into_iter().collect(),
        }
    }
}

/// Generates the signature for simple binary operations with no output flow.
macro_rules! no_ret_deps_flow {
    ($name:literal : $arg_nb:literal) => {
        ($name, FunFlow::new_only_ret([]))
    };
}

/// Generates the signature for binary operations where the output depends on
/// all the input variables
macro_rules! all_ret_deps_flow {
    ($name:literal : $arg_nb:literal) => {
        ($name, FunFlow::new_only_ret(0..$arg_nb))
    };
}

/// Flows for builtin functions.
pub(super) fn builtin_flows<'ctx>() -> HashMap<&'ctx str, FunFlow> {
    [
        no_ret_deps_flow!("len": 1),
        no_ret_deps_flow!("rand": 2),
        no_ret_deps_flow!("debug": 0),
        no_ret_deps_flow!("assert": 1),
        ("push", FunFlow::new([(0, [1])], [])), // arg 0 will depend on arg 1
        all_ret_deps_flow!("+": 2),
        all_ret_deps_flow!("-": 2),
        all_ret_deps_flow!("*": 2),
        all_ret_deps_flow!("/": 2),
        all_ret_deps_flow!("%": 2),
        no_ret_deps_flow!("==": 2),
        no_ret_deps_flow!(">=": 2),
        no_ret_deps_flow!("<": 2),
        no_ret_deps_flow!(">": 2),
        no_ret_deps_flow!("<=": 2),
        no_ret_deps_flow!("!=": 2),
        no_ret_deps_flow!("||": 2),
        no_ret_deps_flow!("&&": 2),
        no_ret_deps_flow!("!": 1),
    ]
    .into_iter()
    .collect()
}

impl<'mir, 'ctx> Instr<'mir, 'ctx> {
    /// Applies a function flow to the algorithm, returning the maps of pointers
    /// to references these pointers will borrow from in that instruction.
    pub fn flow<'a>(
        &'a self,
        fun_flow: &'a HashMap<&'ctx str, FunFlow>,
    ) -> HashMap<Pointer<'ctx>, Vec<&'a Reference<'mir, 'ctx>>> {
        match &self.kind {
            InstrKind::Noop => HashMap::new(),
            InstrKind::Assign(lhs, rhs) => [(lhs.as_ptr(), rhs.references().collect_vec())]
                .into_iter()
                .collect(),
            InstrKind::Call {
                name,
                args,
                destination,
            } => {
                let flow = &fun_flow.get(name.name).unwrap_or_else(|| {
                    panic!(
                        "internal compiler error: {} has no registered flow",
                        name.name
                    )
                });
                let mut res = HashMap::new();
                // Compute the flow for each argument
                for (i, arg) in args.iter().enumerate() {
                    // Only consider the by-reference arguments.
                    if let Some(mutated) = arg.mutated_ptr() {
                        // Get its dependency map, and get the references for the arguments we
                        // depend on.
                        let boxed: Vec<&Reference> = flow
                            .args
                            .get(&i)
                            .map(|deps| deps.iter().map(|&i| args[i].reference()).collect_vec())
                            .unwrap_or_default();

                        res.insert(mutated, boxed);
                    }
                }

                // And for the result, of course
                if let Some(destination) = destination {
                    res.insert(
                        destination.as_ptr(),
                        flow.ret.iter().map(|&i| args[i].reference()).collect_vec(),
                    );
                }

                res
            }
            InstrKind::Branch(_) | InstrKind::PhantomUse(_) => HashMap::new(),
            InstrKind::SwapS(_, _) => unreachable!(),
        }
    }
}
