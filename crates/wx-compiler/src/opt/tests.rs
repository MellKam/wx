//! Tests for the sea-of-nodes builder.
//!
//! Each test compiles a small wx snippet → MIR, then runs `Builder::build` and
//! asserts structural properties of the resulting `Function` (node counts, node
//! kinds, use-lists, phi presence, etc.).
//!
//! The stdlib is intentionally excluded so tests don't depend on stdlib span
//! offsets. Only language features available without stdlib are used.

use std::collections::HashMap;

use indoc::indoc;

use crate::mir::{self, MIR};
use crate::opt::builder::Builder;
use crate::opt::scheduler::{Instruction, Scheduler};
use crate::opt::{ControlNode, DataNodeKind, ScalarType, StackResult};
use crate::{tir, vfs};

/// Minimal stdlib definitions required for memory / pointer tests.
const STD: &str = indoc! {"
    trait PointerSize {}
    impl PointerSize for u32 {}
    trait Memory {
        type Size: PointerSize;
        const MEMORY_INDEX: u32;
        fn grow(self, delta: Self::Size) -> Self::Size;
        fn size(self) -> Self::Size;
    }
"};


// ── Test harness
// ──────────────────────────────────────────────────────────────

struct TestCase {
    mir: MIR,
}

impl TestCase {
    fn schedule(&self) -> Vec<Instruction> {
        let func_mir = self.get_first_func();
        let opt = Builder::build(&self.mir, func_mir);
        Scheduler::schedule(&opt, &self.mir).body
    }

    fn schedule_full(&self) -> crate::opt::scheduler::ScheduledFunction {
        let func_mir = self.get_first_func();
        let opt = Builder::build(&self.mir, func_mir);
        Scheduler::schedule(&opt, &self.mir)
    }
}

impl TestCase {
    fn new(source: &str) -> Self {
        let mut builder = vfs::CompilationGraphBuilder::new();
        let stdlib_id = builder
            .load_crate(
                "std.wx".to_string(),
                &vfs::VirtualFileSource::new(HashMap::from([(
                    "std.wx".to_string(),
                    STD.to_string(),
                )])),
            )
            .unwrap();
        let root_id = builder
            .load_crate(
                "main.wx".to_string(),
                &vfs::VirtualFileSource::new(HashMap::from([(
                    "main.wx".to_string(),
                    source.to_string(),
                )])),
            )
            .unwrap();
        let mut graph = builder.build(root_id, stdlib_id);
        let tir = tir::TIR::build(&mut graph);
        let mir = MIR::build(&tir, &graph.interner, graph.id_generator);
        TestCase { mir }
    }

    /// Return the first function in the MIR output.
    /// Each test compiles a single function (no stdlib), so this is always the
    /// right one.
    fn get_first_func(&self) -> &mir::Function {
        self.mir.functions.first().expect("no functions in MIR")
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

/// `fn add(a: i32, b: i32) -> i32 { a + b }` should produce exactly:
///   - 2 Param nodes (indices 0 and 1)
///   - 1 Add node referencing them
///   - root block has 1 statement (Return)
#[test]
fn test_simple_add() {
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        export { add }
    "});
    let func = case.get_first_func();
    let opt = Builder::build(&case.mir, func);

    // Exactly 3 data nodes: Param(0), Param(1), Add.
    assert_eq!(
        opt.data_nodes.len(),
        3,
        "expected Param(0), Param(1), Add — got {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    assert!(matches!(
        opt.data_nodes[0].kind,
        DataNodeKind::Param {
            index: 0,
            ty: ScalarType::I32
        }
    ));
    assert!(matches!(
        opt.data_nodes[1].kind,
        DataNodeKind::Param {
            index: 1,
            ty: ScalarType::I32
        }
    ));
    assert!(matches!(
        opt.data_nodes[2].kind,
        DataNodeKind::Add {
            left: 0,
            right: 1,
            ..
        }
    ));

    // The Add node is used by nothing (the Return consumes it via StackResult, not
    // a use-edge). Params are used by the Add.
    assert_eq!(
        opt.data_nodes[0].uses,
        vec![2],
        "Param(0) should be used by Add"
    );
    assert_eq!(
        opt.data_nodes[1].uses,
        vec![2],
        "Param(1) should be used by Add"
    );

    // Root block: 1 Return statement.
    let root = opt.blocks[0].as_ref().unwrap();
    assert_eq!(root.statements.len(), 1);
    assert!(matches!(
        root.statements[0],
        crate::opt::ControlNode::Return {
            value: StackResult::Value(2)
        }
    ));
}

/// `fn const_add() -> i32 { 3 + 4 }` — constant folding should eliminate the
/// Add node and produce a single `Int { value: 7 }` node.
#[test]
fn test_constant_folding() {
    let case = TestCase::new(indoc! {"
        fn const_add() -> i32 { 3 + 4 }
        export { const_add }
    "});
    let func = case.get_first_func();
    let opt = Builder::build(&case.mir, func);

    // No Add node should exist — folded away at construction time.
    let add_exists = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::Add { .. }));
    assert!(
        !add_exists,
        "Add node should have been folded; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // Exactly one Int node with value 7.
    let int_nodes: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| {
            matches!(
                n.kind,
                DataNodeKind::Int {
                    value: 7,
                    ty: ScalarType::I32
                }
            )
        })
        .collect();
    assert_eq!(int_nodes.len(), 1, "expected one Int{{7}} node");
}

/// Deep constant folding: `(2 + 3) * (1 + 4)` should fold to `Int { value: 25
/// }`.
#[test]
fn test_constant_folding_nested() {
    let case = TestCase::new(indoc! {"
        fn nested() -> i32 { (2 + 3) * (1 + 4) }
        export { nested }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let any_arith = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::Add { .. } | DataNodeKind::Mul { .. }));
    assert!(!any_arith, "all arithmetic should be folded");

    let result_node = opt
        .data_nodes
        .iter()
        .find(|n| matches!(n.kind, DataNodeKind::Int { value: 25, .. }));
    assert!(result_node.is_some(), "expected Int{{25}}");
}

/// `fn cse(x: i32) -> i32 { local a: i32 = x + 1; local b: i32 = x + 1; a + b
/// }`
///
/// `x + 1` should be computed once (CSE deduplication). There should be exactly
/// two Add nodes in total: one for `x + 1` and one for `(x+1) + (x+1)`.
#[test]
fn test_cse_deduplication() {
    let case = TestCase::new(indoc! {"
        fn cse(x: i32) -> i32 {
            local a: i32 = x + 1;
            local b: i32 = x + 1;
            a + b
        }
        export { cse }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let add_nodes: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::Add { .. }))
        .collect();

    // x+1 (once, CSE'd), then (x+1)+(x+1)
    assert_eq!(
        add_nodes.len(),
        2,
        "expected 2 Add nodes (x+1 CSE'd, then (x+1)+(x+1)); got {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // The x+1 node should have exactly 2 uses (left and right of the outer Add).
    let xplus1 = add_nodes[0];
    assert_eq!(
        xplus1.uses.len(),
        2,
        "x+1 should be used twice by the outer Add"
    );
}

/// `fn max(a: i32, b: i32) -> i32 { if a > b { a } else { b } }`
///
/// The if-else should introduce exactly one Phi node merging the two params.
#[test]
fn test_if_else_phi() {
    let case = TestCase::new(indoc! {"
        fn max(a: i32, b: i32) -> i32 {
            if a > b { a } else { b }
        }
        export { max }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let phi_nodes: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::Phi { .. }))
        .collect();

    assert_eq!(
        phi_nodes.len(),
        1,
        "expected exactly one Phi node; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // The Phi should merge Param(0) and Param(1).
    let phi = &phi_nodes[0];
    assert!(
        matches!(
            &phi.kind,
            DataNodeKind::Phi {
                left: 0,
                right: 1,
                ty: ScalarType::I32
            }
        ) || matches!(
            &phi.kind,
            DataNodeKind::Phi {
                left: 1,
                right: 0,
                ty: ScalarType::I32
            }
        ),
        "Phi should merge the two params; got {:?}",
        phi.kind
    );
}

/// `fn one_sided(x: i32) -> i32 { if x > 0 { x + 1 } else { x } }` — when only
/// the then-branch modifies a binding, the phi should still be created
/// correctly.
#[test]
fn test_if_no_else_phi() {
    let case = TestCase::new(indoc! {"
        fn one_sided(x: i32) -> i32 {
            if x > 0 { x + 1 } else { x }
        }
        export { one_sided }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    // There must be a Phi merging (x+1) and x.
    let phi = opt
        .data_nodes
        .iter()
        .find(|n| matches!(n.kind, DataNodeKind::Phi { .. }));
    assert!(phi.is_some(), "expected a Phi node for the if-else result");
}

/// `fn loop_count() -> i32 { local mut i: i32 = 0; loop { if i >= 10 { break i
/// } i = i + 1; } }`
///
/// The loop should produce a LoopParam node for `i` that has `before = Int{0}`
/// and `after` pointing to the add result.
#[test]
fn test_loop_param() {
    let case = TestCase::new(indoc! {"
        fn loop_count() -> i32 {
            local mut i: i32 = 0;
            loop {
                if i >= 10 { break i }
                i = i + 1;
            }
        }
        export { loop_count }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let loop_params: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::LoopParam { .. }))
        .collect();

    assert!(
        !loop_params.is_empty(),
        "expected at least one LoopParam node; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // The LoopParam for `i` should have `before` pointing to Int{0}.
    let i_param = loop_params.iter().find(|n| {
        if let DataNodeKind::LoopParam { before, .. } = n.kind {
            matches!(
                opt.data_nodes[before as usize].kind,
                DataNodeKind::Int { value: 0, .. }
            )
        } else {
            false
        }
    });
    assert!(
        i_param.is_some(),
        "expected LoopParam with before=Int{{0}} for counter `i`"
    );

    // Its `before` and `after` must differ (it IS modified in the loop).
    if let DataNodeKind::LoopParam { before, after, .. } = i_param.unwrap().kind {
        assert_ne!(
            before, after,
            "LoopParam.before and .after must differ for a mutated counter"
        );
    }
}

/// Aggregate field access on a freshly-created struct should fold through
/// immediately: `AggregateGet(Aggregate([param0, param1]), 0)` → `param0`.
/// No `AggregateGet` node should appear in the output.
#[test]
fn test_aggregate_field_fold() {
    let case = TestCase::new(indoc! {"
        struct Point { x: i32, y: i32 }
        fn get_x(px: i32, py: i32) -> i32 {
            local p: Point = Point::{ x: px, y: py };
            p.x
        }
        export { get_x }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    // AggregateGet should have been folded away — returning Param(0) directly.
    let agg_get_exists = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::AggregateGet { .. }));
    assert!(
        !agg_get_exists,
        "AggregateGet should fold through Aggregate at construction; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // The function's return value should be Param(0) directly.
    let root = opt.blocks[0].as_ref().unwrap();
    let return_val = root.statements.iter().find_map(|s| {
        if let crate::opt::ControlNode::Return {
            value: StackResult::Value(n),
        } = s
        {
            Some(*n)
        } else {
            None
        }
    });
    assert!(
        matches!(return_val, Some(n) if matches!(opt.data_nodes[n as usize].kind,
            DataNodeKind::Param { index: 0, .. })),
        "return value should be Param(0) after fold-through"
    );
}

/// Dead nodes (zero uses, not a return value) should not pollute the graph.
/// `fn dead(x: i32) -> i32 { local _unused: i32 = x * 2; x }` — `x * 2` has
/// zero uses so the Mul node should have an empty use list.
#[test]
fn test_dead_node_has_zero_uses() {
    let case = TestCase::new(indoc! {"
        fn dead(x: i32) -> i32 {
            local _unused: i32 = x * 2;
            x
        }
        export { dead }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let mul = opt
        .data_nodes
        .iter()
        .find(|n| matches!(n.kind, DataNodeKind::Mul { .. }));
    assert!(mul.is_some(), "Mul node should still exist in the graph");
    assert_eq!(
        mul.unwrap().uses.len(),
        0,
        "Mul node should have zero uses (dead)"
    );
}

// ── Scheduler tests
// ───────────────────────────────────────────────────────────

/// `fn add(a: i32, b: i32) -> i32 { a + b }` — both params are inlined (never
/// spilled), the Add has a single use, so it is also inlined. Expected: 4
/// instructions.
#[test]
fn test_sched_simple_add() {
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        export { add }
    "});
    let body = case.schedule();

    assert_eq!(
        body.len(),
        3,
        "expected [LocalGet(0), LocalGet(1), I32Add]; got {:#?}",
        body
    );
    assert!(matches!(body[0], Instruction::LocalGet(0)));
    assert!(matches!(body[1], Instruction::LocalGet(1)));
    assert!(matches!(body[2], Instruction::I32Add));
}

/// `fn const_add() -> i32 { 3 + 4 }` — constant folded to `Int{7}`, inlined.
/// Expected: 2 instructions.
#[test]
fn test_sched_constant_folding() {
    let case = TestCase::new(indoc! {"
        fn const_add() -> i32 { 3 + 4 }
        export { const_add }
    "});
    let body = case.schedule();

    assert_eq!(body.len(), 1, "expected [I32Const(7)]; got {:#?}", body);
    assert!(matches!(body[0], Instruction::I32Const(7)));
}

/// `fn cse(x: i32) -> i32 { local a: i32 = x + 1; local b: i32 = x + 1; a + b
/// }` — `x + 1` has 2 uses so it is spilled to a WASM local (index 1, after the
/// single param). Expected: LocalGet(0), I32Const(1), I32Add, LocalSet(1),
/// LocalGet(1), LocalGet(1), I32Add, Return.
#[test]
fn test_sched_cse_spill() {
    let case = TestCase::new(indoc! {"
        fn cse(x: i32) -> i32 {
            local a: i32 = x + 1;
            local b: i32 = x + 1;
            a + b
        }
        export { cse }
    "});
    let body = case.schedule();

    assert_eq!(
        body.len(),
        7,
        "expected 7 instructions (spill + 2 reads); got {:#?}",
        body
    );
    // Compute x+1 and spill to local 1.
    assert!(matches!(body[0], Instruction::LocalGet(0))); // x
    assert!(matches!(body[1], Instruction::I32Const(1)));
    assert!(matches!(body[2], Instruction::I32Add));
    assert!(matches!(body[3], Instruction::LocalSet(1))); // spill x+1
    // Use spilled x+1 twice for the outer add.
    assert!(matches!(body[4], Instruction::LocalGet(1)));
    assert!(matches!(body[5], Instruction::LocalGet(1)));
    assert!(matches!(body[6], Instruction::I32Add));
}

/// `fn max(a: i32, b: i32) -> i32 { if a > b { a } else { b } }` — the if-else
/// uses a phi-through-local pattern:
///   - condition inline (3 instrs)
///   - `if` with Empty block type
///   - then: store param(0) to phi local
///   - else: store param(1) to phi local
///   - `end`
///   - read phi local
///   - return
#[test]
fn test_sched_if_else() {
    let case = TestCase::new(indoc! {"
        fn max(a: i32, b: i32) -> i32 {
            if a > b { a } else { b }
        }
        export { max }
    "});
    let body = case.schedule();

    // Check structural landmarks: If, Else, End are present in that order.
    let if_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::If { .. }));
    let else_pos = body.iter().position(|i| matches!(i, Instruction::Else));
    let end_pos = body.iter().position(|i| matches!(i, Instruction::End));

    assert!(if_pos.is_some(), "expected If instruction; got {:#?}", body);
    assert!(
        else_pos.is_some(),
        "expected Else instruction; got {:#?}",
        body
    );
    assert!(
        end_pos.is_some(),
        "expected End instruction; got {:#?}",
        body
    );

    let (ip, ep, np) = (if_pos.unwrap(), else_pos.unwrap(), end_pos.unwrap());
    assert!(
        ip < ep && ep < np,
        "If must precede Else which must precede End"
    );

    // The If block type must be Empty (not Value) because a phi local is used.
    assert!(
        matches!(
            body[ip],
            Instruction::If {
                ty: crate::opt::scheduler::BlockType::Empty
            }
        ),
        "If block type should be Empty when phi outputs exist; got {:?}",
        body[ip]
    );

    // Both branches must write to the phi local (LocalSet) and the result is read
    // via LocalGet after End.
    let local_sets: Vec<_> = body[ip..np]
        .iter()
        .filter(|i| matches!(i, Instruction::LocalSet(_)))
        .collect();
    assert_eq!(
        local_sets.len(),
        2,
        "expected one LocalSet per branch; got {:#?}",
        local_sets
    );

    // After End: LocalGet (phi) is the last instruction.
    assert!(
        matches!(body[np + 1], Instruction::LocalGet(_)),
        "expected LocalGet after End"
    );
    assert_eq!(
        body.len(),
        np + 2,
        "LocalGet after End must be the last instruction"
    );
}

/// `fn loop_count() -> i32 { local mut i: i32 = 0; loop { if i >= 10 { break i
/// } i = i + 1; } }`
///
/// Structural check: Block, Loop, ... Br(0), End, End, Return.
/// The loop variable `i` must be initialised before the block and written back
/// at the end.
#[test]
fn test_sched_loop() {
    let case = TestCase::new(indoc! {"
        fn loop_count() -> i32 {
            local mut i: i32 = 0;
            loop {
                if i >= 10 { break i }
                i = i + 1;
            }
        }
        export { loop_count }
    "});
    let body = case.schedule();

    // Block and Loop instructions must appear in that order.
    let block_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::Block { .. }));
    let loop_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::Loop { .. }));
    assert!(
        block_pos.is_some(),
        "expected Block instruction; got {:#?}",
        body
    );
    assert!(
        loop_pos.is_some(),
        "expected Loop instruction; got {:#?}",
        body
    );
    assert!(
        block_pos.unwrap() < loop_pos.unwrap(),
        "Block must precede Loop"
    );

    // A Br(0) must exist for the implicit continue before End End.
    let br0_pos = body.iter().rposition(|i| matches!(i, Instruction::Br(0)));
    assert!(
        br0_pos.is_some(),
        "expected Br(0) for implicit continue; got {:#?}",
        body
    );

    // Two End instructions close the Loop and Block.
    let ends: Vec<_> = body
        .iter()
        .enumerate()
        .filter(|(_, i)| matches!(i, Instruction::End))
        .collect();
    assert!(
        ends.len() >= 2,
        "expected at least 2 End instructions; got {:#?}",
        body
    );

    // No trailing Return — the break value (LocalGet) is the last instruction.
    assert!(
        !matches!(body.last().unwrap(), Instruction::Return),
        "trailing Return must be stripped; got {:#?}",
        body.last()
    );
}

// ── Edge case tests
// ───────────────────────────────────────────────────────────

/// `fn same(x: i32) -> i32 { if x > 0 { x } else { x } }` — both branches
/// return the same node, so `Phi(x, x)` folds to `x`. No phi node should exist.
#[test]
fn test_phi_identity_fold() {
    let case = TestCase::new(indoc! {"
        fn same(x: i32) -> i32 { if x > 0 { x } else { x } }
        export { same }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let phi_nodes: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::Phi { .. }))
        .collect();
    assert!(
        phi_nodes.is_empty(),
        "Phi(x, x) should fold away; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );
}

/// `fn f(a: i32, b: i32, c: i32) -> i32 { if a > 0 { if b > 0 { a } else { b }
/// } else { c } }`
///
/// The inner if-else creates `Phi(a, b)`. The outer if-else creates
/// `Phi(Phi(a,b), c)`. Exactly 2 phi nodes must be present.
#[test]
fn test_nested_if_else_phi_chain() {
    let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> i32 {
            if a > 0 {
                if b > 0 { a } else { b }
            } else {
                c
            }
        }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let phi_nodes: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::Phi { .. }))
        .collect();
    assert_eq!(
        phi_nodes.len(),
        2,
        "expected 2 phi nodes for nested if-else; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // The inner phi must merge two params (a and b).
    let inner_phi_exists = phi_nodes.iter().any(|n| {
        matches!(&n.kind, DataNodeKind::Phi { left, right, .. }
            if matches!(opt.data_nodes[*left as usize].kind, DataNodeKind::Param { .. })
            && matches!(opt.data_nodes[*right as usize].kind, DataNodeKind::Param { .. }))
    });
    assert!(
        inner_phi_exists,
        "inner phi should merge two params (a and b)"
    );

    // The outer phi must have one operand that is itself a phi.
    let outer_phi_exists = phi_nodes.iter().any(|n| {
        matches!(&n.kind, DataNodeKind::Phi { left, right, .. }
            if matches!(opt.data_nodes[*left as usize].kind, DataNodeKind::Phi { .. })
            || matches!(opt.data_nodes[*right as usize].kind, DataNodeKind::Phi { .. }))
    });
    assert!(
        outer_phi_exists,
        "outer phi should have a phi as one of its operands"
    );
}

/// A binding that is read inside a loop but never written should produce a
/// `LoopParam` with `before == after` (zero uses). Only the mutated counter
/// `i` should appear in the Loop's outputs.
#[test]
fn test_loop_immutable_binding() {
    let case = TestCase::new(indoc! {"
        fn f(limit: i32) -> i32 {
            local mut i: i32 = 0;
            loop {
                if i >= limit { break i }
                i = i + 1;
            }
        }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    // Any LoopParam with before == after was left unpatched (binding never
    // modified). It may still have uses from reads inside the loop body —
    // that's correct. The important invariant is that before == after (not a
    // self-referential node).
    for node in &opt.data_nodes {
        if let DataNodeKind::LoopParam { before, after, .. } = node.kind {
            if before == after {
                // before and after point to the same pre-loop node — not self-referential.
                // (before == the_loop_param_itself would be the old bug.)
                assert_ne!(
                    before as usize,
                    opt.data_nodes
                        .iter()
                        .position(|n| { std::ptr::eq(n as *const _, node as *const _) })
                        .unwrap_or(usize::MAX),
                    "before must not point to the LoopParam itself; got {:?}",
                    node.kind
                );
            }
        }
    }

    // Exactly one LoopParam must be active (before != after): the counter `i`.
    let active: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(
            |n| matches!(&n.kind, DataNodeKind::LoopParam { before, after, .. } if before != after),
        )
        .collect();
    assert_eq!(
        active.len(),
        1,
        "only `i` should be an active LoopParam; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );
}

/// When two bindings are mutated inside a loop both must be present in the
/// Loop control node's outputs (i.e. both LoopParams are patched).
#[test]
fn test_loop_two_mutated_bindings() {
    let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            local mut i: i32 = 0;
            local mut sum: i32 = 0;
            loop {
                if i >= 5 { break sum }
                sum = sum + i;
                i = i + 1;
            }
        }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let active: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(
            |n| matches!(&n.kind, DataNodeKind::LoopParam { before, after, .. } if before != after),
        )
        .collect();
    assert_eq!(
        active.len(),
        2,
        "expected active LoopParams for both `i` and `sum`; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // Verify that each active LoopParam has at least 1 use from inside the loop
    // body.
    for lp in &active {
        assert!(
            lp.uses.len() >= 1,
            "active LoopParam should have ≥1 use; got {:?}",
            lp.kind
        );
    }
}

/// Division by zero must NOT be constant-folded: `1 / 0` should keep the
/// `Div` node in the graph.
#[test]
fn test_no_fold_div_by_zero() {
    let case = TestCase::new(indoc! {"
        fn f() -> i32 { 1 / 0 }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let div_exists = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::Div { .. }));
    assert!(
        div_exists,
        "Div(1, 0) must not be folded; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );
}

/// Bitwise-XOR of two integer constants must be constant-folded.
/// `5 ^ 3 == 6` so the graph should contain `Int { value: 6 }` and no `BitXor`.
#[test]
fn test_constant_fold_bitwise() {
    let case = TestCase::new(indoc! {"
        fn f() -> i32 { 5 ^ 3 }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let xor_exists = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::BitXor { .. }));
    assert!(
        !xor_exists,
        "BitXor(5,3) should be folded away; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    let folded = opt.data_nodes.iter().find(|n| {
        matches!(
            n.kind,
            DataNodeKind::Int {
                value: 6,
                ty: ScalarType::I32
            }
        )
    });
    assert!(folded.is_some(), "expected Int{{6}} after BitXor fold");
}

/// An explicit mid-function `return` produces a `Return` statement in the
/// then-block and a second implicit `Return` at the end of the root block.
/// Across all blocks, exactly 2 `Return` statements must be present.
#[test]
fn test_explicit_mid_return() {
    let case = TestCase::new(indoc! {"
        fn f(x: i32) -> i32 {
            if x > 0 { return x } else { 0 }
        }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    let total_returns = opt
        .blocks
        .iter()
        .filter_map(|b| b.as_ref())
        .flat_map(|b| b.statements.iter())
        .filter(|s| matches!(s, ControlNode::Return { .. }))
        .count();
    assert_eq!(
        total_returns, 2,
        "expected 2 Return statements (explicit in then-block + implicit tail in root); got {}",
        total_returns
    );
}

/// A struct binding that differs between two branches of an if-else must be
/// decomposed field-by-field: one `Phi` per struct field must appear, and
/// `AggregateGet` must fold away (since the merged aggregate node is concrete).
#[test]
fn test_aggregate_phi_decomposition() {
    let case = TestCase::new(indoc! {"
        struct Pair { x: i32, y: i32 }
        fn f(a: i32, b: i32, cond: i32) -> i32 {
            local mut p: Pair = Pair::{ x: a, y: b };
            if cond > 0 { p = Pair::{ x: b, y: a } } else { p = Pair::{ x: a, y: b } }
            p.x
        }
        export { f }
    "});
    let opt = Builder::build(&case.mir, case.get_first_func());

    // Two phi nodes — one for field x, one for field y.
    let phi_nodes: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::Phi { .. }))
        .collect();
    assert_eq!(
        phi_nodes.len(),
        2,
        "expected one Phi per struct field; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    // AggregateGet must fold through the freshly-built merged Aggregate node.
    let agg_get_exists = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::AggregateGet { .. }));
    assert!(
        !agg_get_exists,
        "AggregateGet should fold through the merged Aggregate; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );
}

/// Loading a struct through a pointer expands to one `PointerLoadResult` node
/// per field assembled into a regular `Aggregate` node. There should be one
/// `PointerLoad` control statement per field.
#[test]
fn test_struct_pointer_load_expands_to_per_field_loads() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory<Size = u32>;
        struct Point { x: i32, y: i32 }
        fn load_point(ptr: heap::*Point) -> Point { ptr.* }
        export { load_point, heap }
    "}
    );
    let case = TestCase::new(&src);
    let opt = Builder::build(&case.mir, case.get_first_func());

    let plr_count = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::PointerLoadResult { .. }))
        .count();
    assert_eq!(
        plr_count,
        2,
        "expected one PointerLoadResult per field; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    let agg_count = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::Aggregate { .. }))
        .count();
    assert_eq!(
        agg_count,
        1,
        "expected one Aggregate node wrapping the field results; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    let root = opt.blocks[0].as_ref().unwrap();
    let pointer_load_count = root
        .statements
        .iter()
        .filter(|s| matches!(s, ControlNode::PointerLoad { .. }))
        .count();
    assert_eq!(
        pointer_load_count, 2,
        "expected one PointerLoad control node per field"
    );
}

/// `local p = *ptr; p.x` — `AggregateGet` folds through the `Aggregate` built
/// from per-field `PointerLoadResult` nodes, so the return value is a
/// `PointerLoadResult` directly. No `AggregateGet` node should appear in the
/// graph.
#[test]
fn test_struct_pointer_load_field_access_folds() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory<Size = u32>;
        struct Point { x: i32, y: i32 }
        fn get_x(ptr: heap::*Point) -> i32 {
            local p: Point = ptr.*;
            p.x
        }
        export { get_x, heap }
    "}
    );
    let case = TestCase::new(&src);
    let opt = Builder::build(&case.mir, case.get_first_func());

    let agg_get_exists = opt
        .data_nodes
        .iter()
        .any(|n| matches!(n.kind, DataNodeKind::AggregateGet { .. }));
    assert!(
        !agg_get_exists,
        "AggregateGet should fold through Aggregate built from PointerLoadResults; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    let root = opt.blocks[0].as_ref().unwrap();
    let return_val = root.statements.iter().find_map(|s| {
        if let ControlNode::Return {
            value: StackResult::Value(n),
        } = s
        {
            Some(*n)
        } else {
            None
        }
    });
    assert!(
        matches!(
            return_val,
            Some(n) if matches!(
                opt.data_nodes[n as usize].kind,
                DataNodeKind::PointerLoadResult { .. }
            )
        ),
        "return value should be a PointerLoadResult after AggregateGet fold"
    );
}

/// Storing a struct through a pointer expands to one scalar `PointerStore`
/// control node per field. No aggregate-store variant is needed.
#[test]
fn test_struct_pointer_store_expands_to_per_field_stores() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory<Size = u32>;
        struct Point { x: i32, y: i32 }
        fn store_point(ptr: heap::*mut Point, x: i32, y: i32) {
            ptr.* = Point::{ x: x, y: y }
        }
        export { store_point, heap }
    "}
    );
    let case = TestCase::new(&src);
    let opt = Builder::build(&case.mir, case.get_first_func());

    let root = opt.blocks[0].as_ref().unwrap();
    let store_count = root
        .statements
        .iter()
        .filter(|s| matches!(s, ControlNode::PointerStore { .. }))
        .count();
    assert_eq!(
        store_count, 2,
        "expected one PointerStore control node per struct field"
    );
}

// ── Additional scheduler edge cases
// ───────────────────────────────────────────

/// When both branches of an if-else return the same node (phi identity),
/// No inter-branch phi local needed: the only LocalSet is the one-shot capture
/// of the if-block's value after `End` (needed to clear the stack before the
/// final implicit return).  No second LocalSet for a phi join.
#[test]
fn test_sched_phi_identity_no_local_set() {
    let case = TestCase::new(indoc! {"
        fn same(x: i32) -> i32 { if x > 0 { x } else { x } }
        export { same }
    "});
    let body = case.schedule();

    // Exactly one LocalSet (the post-End capture) — no phi-join sets inside
    // branches.
    let local_sets: Vec<_> = body
        .iter()
        .filter(|i| matches!(i, Instruction::LocalSet(_)))
        .collect();
    assert_eq!(
        local_sets.len(),
        1,
        "expected exactly one LocalSet (post-End capture); got {:#?}",
        body
    );

    // Basic structural sanity: If … Else … End present, no trailing Return.
    assert!(
        body.iter().any(|i| matches!(i, Instruction::If { .. })),
        "expected If instruction; got {:#?}",
        body
    );
    assert!(
        body.iter().any(|i| matches!(i, Instruction::Else)),
        "expected Else instruction; got {:#?}",
        body
    );
    assert!(
        body.iter().any(|i| matches!(i, Instruction::End)),
        "expected End instruction; got {:#?}",
        body
    );
    assert!(
        !matches!(body.last().unwrap(), Instruction::Return),
        "trailing Return must be stripped"
    );
}

/// A loop with two mutated variables must emit write-backs for both before the
/// `Br(0)` that closes the iteration. Exactly 2 `LocalSet` instructions must
/// appear between the `Loop` opcode and the final `Br(0)`.
#[test]
fn test_sched_loop_two_vars_writebacks() {
    let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            local mut i: i32 = 0;
            local mut sum: i32 = 0;
            loop {
                if i >= 5 { break sum }
                sum = sum + i;
                i = i + 1;
            }
        }
        export { f }
    "});
    let body = case.schedule();

    let loop_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::Loop { .. }))
        .expect("expected Loop instruction; got {:#?}");
    let br0_pos = body
        .iter()
        .rposition(|i| matches!(i, Instruction::Br(0)))
        .expect("expected Br(0) for loop-continue");

    // Count LocalSets in the range [Loop, Br(0)) — these are the write-backs.
    let write_backs: Vec<_> = body[loop_pos..br0_pos]
        .iter()
        .filter(|i| matches!(i, Instruction::LocalSet(_)))
        .collect();
    assert_eq!(
        write_backs.len(),
        2,
        "expected 2 write-back LocalSets (for `i` and `sum`) before Br(0); got {:#?}",
        body
    );

    assert!(
        !matches!(body.last().unwrap(), Instruction::Return),
        "trailing Return must be stripped"
    );
}

/// `fn no_else(x: i32) -> i32 { local mut y: i32 = 0; if x > 0 { y = x + 1 }
/// else { y = 0 }; y }`
///
/// Because the then-branch and else-branch produce different values for `y`
/// (Param+1 vs Int{0}), the scheduler must use an Empty If block (phi via
/// LocalSet) and emit exactly 2 `LocalSet`s — one per branch.
#[test]
fn test_sched_if_else_phi_stores() {
    let case = TestCase::new(indoc! {"
        fn no_else(x: i32) -> i32 {
            local mut y: i32 = 0;
            if x > 0 { y = x + 1 } else { y = 0 }
            y
        }
        export { no_else }
    "});
    let body = case.schedule();

    let if_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::If { .. }))
        .expect("expected If");
    let end_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::End))
        .expect("expected End");

    // Block type must be Empty (phi stored via LocalSet, not returned from
    // branches).
    assert!(
        matches!(
            body[if_pos],
            Instruction::If {
                ty: crate::opt::scheduler::BlockType::Empty
            }
        ),
        "If block type should be Empty when phi stores are used; got {:?}",
        body[if_pos]
    );

    // One LocalSet in each branch — 2 total between If and End.
    let sets_in_if: Vec<_> = body[if_pos..=end_pos]
        .iter()
        .filter(|i| matches!(i, Instruction::LocalSet(_)))
        .collect();
    assert_eq!(
        sets_in_if.len(),
        2,
        "expected one LocalSet per branch; got {:#?}",
        body
    );

    // After End: LocalGet (phi) is the last instruction.
    assert!(
        matches!(body[end_pos + 1], Instruction::LocalGet(_)),
        "expected LocalGet after End; got {:#?}",
        &body[end_pos + 1]
    );
    assert_eq!(
        body.len(),
        end_pos + 2,
        "LocalGet after End must be the last instruction"
    );
}

/// Storing a struct through a pointer emits one store instruction per field.
/// For `Point { x: i32, y: i32 }` the x field uses `i32.store offset=0` and
/// the y field uses `i32.store offset=4` — field offsets are baked into the
/// memarg immediate, no address arithmetic needed.
#[test]
fn test_sched_struct_pointer_store() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory<Size = u32>;
        struct Point { x: i32, y: i32 }
        fn store_point(ptr: heap::*mut Point, x: i32, y: i32) {
            ptr.* = Point::{ x: x, y: y }
        }
        export { store_point, heap }
    "}
    );
    let case = TestCase::new(&src);
    let body = case.schedule();

    let store_count = body
        .iter()
        .filter(|i| matches!(i, Instruction::I32Store(_)))
        .count();
    assert_eq!(
        store_count, 2,
        "expected one I32Store per field; got {:#?}",
        body
    );

    // Field offsets are in the instruction immediates — no address arithmetic
    // needed.
    let add_count = body
        .iter()
        .filter(|i| matches!(i, Instruction::I32Add))
        .count();
    assert_eq!(
        add_count, 0,
        "field offsets should be in memarg immediates, not computed via I32Add; got {:#?}",
        body
    );
}

/// Loading a struct and accessing one field via `p.x` emits only the live
/// field load, and the final value on the stack is the result of the field-x
/// load (spilled to a local, then read back).
#[test]
fn test_sched_struct_pointer_load_field_access() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory<Size = u32>;
        struct Point { x: i32, y: i32 }
        fn get_x(ptr: heap::*Point) -> i32 {
            local p: Point = ptr.*;
            p.x
        }
        export { get_x, heap }
    "}
    );
    let case = TestCase::new(&src);
    let body = case.schedule();

    // Only the x-field load is emitted. The y-field load is dead after the
    // AggregateGet fold and is skipped by scheduler liveness.
    let load_count = body
        .iter()
        .filter(|i| matches!(i, Instruction::I32Load(_)))
        .count();
    assert_eq!(
        load_count, 1,
        "expected only the live field load; got {:#?}",
        body
    );

    // The return value is a LocalGet — the spilled PointerLoadResult for x.
    assert!(
        matches!(body.last(), Some(Instruction::LocalGet(_))),
        "last instruction should be LocalGet (field x spilled to local); got {:#?}",
        body.last()
    );
}

/// Returning the full loaded aggregate keeps all field loads live.
#[test]
fn test_sched_struct_pointer_load_full_aggregate_keeps_all_loads() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory<Size = u32>;
        struct Point { x: i32, y: i32 }
        fn load_point(ptr: heap::*Point) -> Point { ptr.* }
        export { load_point, heap }
    "}
    );
    let case = TestCase::new(&src);
    let body = case.schedule();

    let load_count = body
        .iter()
        .filter(|i| matches!(i, Instruction::I32Load(_)))
        .count();
    assert_eq!(
        load_count, 2,
        "expected both field loads when returning the full aggregate; got {:#?}",
        body
    );
}

// ── Snapshot test
// ─────────────────────────────────────────────────────────────

/// Snapshot the full scheduled instruction sequence for the CSE example.
///
/// `fn cse(x: i32) -> i32 { local a: i32 = x + 1; local b: i32 = x + 1; a + b
/// }`
///
/// Expected shape: compute `x+1`, spill it to local 1, read it twice, add,
/// return. The snapshot pins the exact opcode sequence so regressions in spill
/// decisions or instruction ordering are immediately visible.
#[test]
fn test_snapshot_sched_cse() {
    let case = TestCase::new(indoc! {"
        fn cse(x: i32) -> i32 {
            local a: i32 = x + 1;
            local b: i32 = x + 1;
            a + b
        }
        export { cse }
    "});
    insta::assert_yaml_snapshot!(case.schedule_full());
}

// ── Function call tests
// ───────────────────────────────────────────────────────

/// A direct function call produces exactly one `CallResult` data node (the SSA
/// value for the return) and exactly one `Call` control statement in the root
/// block.
#[test]
fn test_call_creates_callresult_node() {
    let case = TestCase::new(indoc! {"
        fn caller(x: i32) -> i32 { callee(x) }
        fn callee(x: i32) -> i32 { x + 1 }
        export { caller }
    "});
    let opt = Builder::build(&case.mir, &case.mir.functions[0]);

    let call_results: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::CallResult { .. }))
        .collect();
    assert_eq!(
        call_results.len(),
        1,
        "expected exactly one CallResult node; nodes: {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );

    let root = opt.blocks[0].as_ref().unwrap();
    let call_stmts = root
        .statements
        .iter()
        .filter(|s| matches!(s, ControlNode::Call { .. }))
        .count();
    assert_eq!(
        call_stmts, 1,
        "expected one Call control statement in the root block"
    );
}

/// Two calls to the same function with the same argument must produce two
/// distinct `CallResult` nodes — calls are excluded from CSE because they
/// may have observable side effects.
#[test]
fn test_call_no_cse() {
    let case = TestCase::new(indoc! {"
        fn caller(x: i32) -> i32 {
            local a: i32 = callee(x);
            local b: i32 = callee(x);
            a + b
        }
        fn callee(x: i32) -> i32 { x + 1 }
        export { caller }
    "});
    let opt = Builder::build(&case.mir, &case.mir.functions[0]);

    let call_results: Vec<_> = opt
        .data_nodes
        .iter()
        .filter(|n| matches!(n.kind, DataNodeKind::CallResult { .. }))
        .collect();
    assert_eq!(
        call_results.len(),
        2,
        "identical calls must not be CSE'd; expected 2 CallResult nodes, got {:#?}",
        opt.data_nodes.iter().map(|n| &n.kind).collect::<Vec<_>>()
    );
}

/// The argument node's use list must include the `CallResult` that consumes it,
/// confirming the data-flow edge `arg → CallResult` is properly registered.
#[test]
fn test_call_arg_use_edges() {
    let case = TestCase::new(indoc! {"
        fn caller(x: i32) -> i32 { callee(x) }
        fn callee(x: i32) -> i32 { x + 1 }
        export { caller }
    "});
    let opt = Builder::build(&case.mir, &case.mir.functions[0]);

    let call_result_idx = opt
        .data_nodes
        .iter()
        .position(|n| matches!(n.kind, DataNodeKind::CallResult { .. }))
        .expect("CallResult node must exist") as u32;

    // The argument Param{0} must list the CallResult as a user.
    let param = opt
        .data_nodes
        .iter()
        .find(|n| matches!(n.kind, DataNodeKind::Param { index: 0, .. }))
        .expect("Param{0} must exist");
    assert!(
        param.uses.contains(&call_result_idx),
        "Param(x) should be registered as an input to the CallResult; uses: {:?}",
        param.uses
    );

    // The FunctionRef for the callee must also list the CallResult as a user.
    let func_ref = opt
        .data_nodes
        .iter()
        .find(|n| matches!(n.kind, DataNodeKind::FunctionRef { .. }))
        .expect("FunctionRef node must exist");
    assert!(
        func_ref.uses.contains(&call_result_idx),
        "FunctionRef should be registered as an input to the CallResult; uses: {:?}",
        func_ref.uses
    );
}

/// The scheduler must push all arguments onto the stack *before* emitting the
/// `Call` opcode. For `caller(a, b)`, the sequence must be:
/// `LocalGet(0)`, `LocalGet(1)`, `Call(n)`.
#[test]
fn test_sched_call_args_precede_opcode() {
    let case = TestCase::new(indoc! {"
        fn caller(a: i32, b: i32) -> i32 { add(a, b) }
        fn add(x: i32, y: i32) -> i32 { x + y }
        export { caller }
    "});
    let body = case.schedule();

    let call_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::Call(_)))
        .expect("expected a Call instruction");

    // Both args must appear before the Call.
    assert!(
        call_pos >= 2,
        "need at least 2 instructions before Call for the args"
    );
    assert!(
        matches!(body[call_pos - 2], Instruction::LocalGet(0)),
        "first arg (a = local 0) must be pushed two slots before Call; got {:?}",
        body[call_pos - 2]
    );
    assert!(
        matches!(body[call_pos - 1], Instruction::LocalGet(1)),
        "second arg (b = local 1) must be pushed immediately before Call; got {:?}",
        body[call_pos - 1]
    );
}

/// A call result is always spilled to a WASM local (never dropped inline), even
/// when it is used only once. A `LocalSet` must immediately follow the `Call`
/// opcode, and the corresponding `LocalGet` must appear before the `Return`.
#[test]
fn test_sched_call_result_spilled() {
    let case = TestCase::new(indoc! {"
        fn caller(x: i32) -> i32 { callee(x) }
        fn callee(x: i32) -> i32 { x + 1 }
        export { caller }
    "});
    let body = case.schedule();

    let call_pos = body
        .iter()
        .position(|i| matches!(i, Instruction::Call(_)))
        .expect("expected a Call instruction");

    // Immediately after Call: LocalSet (spill the return value).
    assert!(
        matches!(body[call_pos + 1], Instruction::LocalSet(_)),
        "LocalSet must immediately follow Call to spill the result; got {:?}",
        body[call_pos + 1]
    );

    // The spill local must be read back before Return.
    let spill_local = match body[call_pos + 1] {
        Instruction::LocalSet(l) => l,
        _ => unreachable!(),
    };
    let has_get = body
        .iter()
        .any(|i| matches!(i, Instruction::LocalGet(l) if *l == spill_local));
    assert!(
        has_get,
        "spilled call-result local {} must be read back before Return",
        spill_local
    );

    assert!(
        !matches!(body.last().unwrap(), Instruction::Return),
        "trailing Return must be stripped"
    );
}

// ── Narrow load/store instruction selection ───────────────────────────────────

/// Sub-word loads must emit the appropriate narrow opcode, not a full-width
/// `i32.load`. This is the per-instruction regression for the bug where
/// `ScalarType` erased the width distinction before the scheduler ran.
#[test]
fn test_sched_narrow_loads_emit_correct_opcodes() {
    let cases: &[(&str, &str, fn(&Instruction) -> bool)] = &[
        ("*u8",  "u8",  |i| matches!(i, Instruction::I32Load8U(_))),
        ("*i8",  "i8",  |i| matches!(i, Instruction::I32Load8S(_))),
        ("*u16", "u16", |i| matches!(i, Instruction::I32Load16U(_))),
        ("*i16", "i16", |i| matches!(i, Instruction::I32Load16S(_))),
    ];

    for (ptr_ty, ret_ty, is_expected) in cases {
        let src = format!(
            "{STD}
memory heap: Memory<Size = u32>;
fn read(ptr: heap::{ptr_ty}) -> {ret_ty} {{ ptr.* }}
export {{ read, heap }}"
        );
        let case = TestCase::new(&src);
        let body = case.schedule();

        assert!(
            body.iter().any(|i| is_expected(i)),
            "expected narrow load opcode for {ptr_ty}; got {:#?}",
            body
        );
        assert!(
            !body.iter().any(|i| matches!(i, Instruction::I32Load(_))),
            "must not emit full-width I32Load for {ptr_ty}; got {:#?}",
            body
        );
    }
}

/// Sub-word stores must emit `i32.store8` or `i32.store16`, not `i32.store`.
/// Sign is irrelevant for stores — they always truncate to the target width —
/// so only one variant per byte-width needs covering.
#[test]
fn test_sched_narrow_stores_emit_correct_opcodes() {
    let cases: &[(&str, &str, fn(&Instruction) -> bool)] = &[
        ("*mut u8",  "u8",  |i| matches!(i, Instruction::I32Store8(_))),
        ("*mut u16", "u16", |i| matches!(i, Instruction::I32Store16(_))),
    ];

    for (ptr_ty, val_ty, is_expected) in cases {
        let src = format!(
            "{STD}
memory heap: Memory<Size = u32>;
fn write(ptr: heap::{ptr_ty}, val: {val_ty}) {{ ptr.* = val }}
export {{ write, heap }}"
        );
        let case = TestCase::new(&src);
        let body = case.schedule();

        assert!(
            body.iter().any(|i| is_expected(i)),
            "expected narrow store opcode for {ptr_ty}; got {:#?}",
            body
        );
        assert!(
            !body.iter().any(|i| matches!(i, Instruction::I32Store(_))),
            "must not emit full-width I32Store for {ptr_ty}; got {:#?}",
            body
        );
    }
}

/// Snapshot the full instruction sequence for a two-argument direct call.
/// Pins the spill-then-read pattern: args → Call → LocalSet → LocalGet →
/// Return.
#[test]
fn test_snapshot_sched_call_two_args() {
    let case = TestCase::new(indoc! {"
        fn caller(a: i32, b: i32) -> i32 { add(a, b) }
        fn add(x: i32, y: i32) -> i32 { x + y }
        export { caller }
    "});
    insta::assert_yaml_snapshot!(case.schedule_full());
}
