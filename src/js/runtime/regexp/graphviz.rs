use std::{collections::HashSet, fs, ops::Range};

use crate::{
    common::graphviz::{DotGraphBuilder, DotTextAlign, DOTFILE_EXTENSION},
    runtime::{
        regexp::{
            compiled_regexp::CompiledRegExpObject,
            instruction::{
                BranchInstruction, Instruction, InstructionIterator, JumpInstruction,
                LookaroundInstruction, LoopInstruction, OpCode,
            },
        },
        Context, HeapPtr,
    },
};

/// If the option is set, generate a Graphviz DOT file for the a compiled RegExp and write to disk.
pub fn save_regexp_dotfile_if_needed(
    context: Context,
    compiled_regexp: HeapPtr<CompiledRegExpObject>,
) {
    let output_directory = match context.options.regexp_dotfile_directory.clone() {
        Some(directory) => directory,
        None => return,
    };

    let pattern = compiled_regexp
        .escaped_pattern_source()
        .format()
        .unwrap_or_default();

    let output_path = context.debug_file_name_reserver.reserve_unique_path(
        &output_directory,
        &pattern,
        DOTFILE_EXTENSION,
    );

    let dot_graph = compiled_regexp.to_dot_graph();
    let dot_file = dot_graph.to_string();

    if let Err(error) =
        fs::create_dir_all(&output_directory).and_then(|()| fs::write(&output_path, dot_file))
    {
        eprintln!("Failed to write RegExp dotfile {}: {error}", output_path.display());
    }
}

/// Render a Graphviz DOT representation of the compiled RegExp bytecode.
pub fn compiled_regexp_to_dot_graph(regexp: HeapPtr<CompiledRegExpObject>) -> DotGraphBuilder {
    let mut graph = DotGraphBuilder::new("regexp");

    add_graph_title(&mut graph, regexp);
    add_default_attributes(&mut graph);
    add_start_node(&mut graph);
    add_nodes_and_edges(&mut graph, regexp);

    graph
}

/// Each basic block's starting offset can be used as its unique id.
fn block_to_node_id(block_start_offset: usize) -> String {
    block_start_offset.to_string()
}

fn add_graph_title(graph: &mut DotGraphBuilder, regexp: HeapPtr<CompiledRegExpObject>) {
    let pattern = regexp.escaped_pattern_source().format().unwrap_or_default();
    let graph_title = format!("/{pattern}/{}", regexp.flags);

    graph.set_graph_attribute("label", &graph_title);
}

fn add_start_node(graph: &mut DotGraphBuilder) {
    // Start node points to the first block
    graph.add_node("start").attribute("label", "start");
    graph.add_edge("start", &block_to_node_id(0));
}

fn add_default_attributes(graph: &mut DotGraphBuilder) {
    graph.set_graph_attribute("labelloc", "t");
    graph.set_node_default_attribute("shape", "box");
    graph.set_node_default_attribute("fontname", "monospace");
    graph.set_edge_default_attribute("fontname", "monospace");
}

/// Determine boundaries of all basic blocks in the bytecode.
///
/// Must reconstruct these boundaries by detecting terminator instructions and their branches.
/// Return a vec containing the offsets of the [start, end) of every basic block in the bytecode.
fn build_basic_block_bounds(bytecode: &[u32]) -> Vec<usize> {
    let mut block_bounds = HashSet::new();
    block_bounds.insert(0);

    let mut offset = 0;
    for instr in InstructionIterator::new(bytecode) {
        offset += instr.size();

        let is_basic_block_end = match instr.opcode() {
            OpCode::Fail | OpCode::Accept => true,
            OpCode::Jump => {
                block_bounds.insert(instr.cast::<JumpInstruction>().target() as usize);
                true
            }
            OpCode::Branch => {
                let instr = instr.cast::<BranchInstruction>();
                block_bounds.insert(instr.first_branch() as usize);
                block_bounds.insert(instr.second_branch() as usize);
                true
            }
            OpCode::Loop => {
                block_bounds.insert(instr.cast::<LoopInstruction>().end_branch() as usize);
                true
            }
            OpCode::Lookaround => {
                block_bounds.insert(instr.cast::<LookaroundInstruction>().body_branch() as usize);
                true
            }
            _ => false,
        };

        if is_basic_block_end {
            block_bounds.insert(offset);
        }
    }

    // Sort deduped block boundaries to construct the final list
    let mut sorted_block_bounds = block_bounds.into_iter().collect::<Vec<_>>();
    sorted_block_bounds.sort();

    sorted_block_bounds
}

/// Iterate over all basic blocks in the compiled instructions.
///
/// Call `f` for each basic block, passing the block's offset range and an iterator over the
/// instructions in the block.
fn iter_basic_blocks(bytecode: &[u32], mut f: impl FnMut(Range<usize>, InstructionIterator)) {
    let block_bounds = build_basic_block_bounds(bytecode);

    for bounds in block_bounds.windows(2) {
        let bounds = bounds[0]..bounds[1];
        let basic_block_bytecode = &bytecode[bounds.clone()];
        let iter = InstructionIterator::new(basic_block_bytecode);

        f(bounds, iter);
    }
}

fn add_nodes_and_edges(graph: &mut DotGraphBuilder, regexp: HeapPtr<CompiledRegExpObject>) {
    iter_basic_blocks(regexp.instructions(), |block_bounds, mut instructions| {
        let mut node_label = String::new();

        let node_id = block_to_node_id(block_bounds.start);
        graph.add_node(&node_id);

        let mut offset = block_bounds.start;

        while let Some(instr) = instructions.next() {
            let next_offset = offset + instr.size();

            // Accumulate text for all instructions in the block for the node label
            node_label.push_str(&format!("{offset}: {}\n", instr.debug_print()));

            // Last instruction determines all control flow edges out of the block
            if instructions.is_end() {
                let next_node_id = block_to_node_id(next_offset);

                add_node_color(graph, instr, &node_id);
                add_node_edges(graph, instr, &node_id, &next_node_id);
            }

            offset = next_offset;
        }

        graph
            .get_node(&node_id)
            .attribute("label", &node_label)
            .text_align(DotTextAlign::Left);
    });
}

fn add_node_color(graph: &mut DotGraphBuilder, block_last_instr: &Instruction, node_id: &str) {
    const ACCEPT_COLOR: &str = "#d6f5d6";
    const FAIL_COLOR: &str = "#f5d6d6";

    let node = graph.get_node(node_id);

    match block_last_instr.opcode() {
        OpCode::Accept => {
            node.attribute("style", "filled")
                .attribute("fillcolor", ACCEPT_COLOR);
        }
        OpCode::Fail => {
            node.attribute("style", "filled")
                .attribute("fillcolor", FAIL_COLOR);
        }
        _ => {}
    }
}

fn add_node_edges(
    graph: &mut DotGraphBuilder,
    terminator_instr: &Instruction,
    node_id: &str,
    fallthrough_node_id: &str,
) {
    match terminator_instr.opcode() {
        OpCode::Jump => {
            let instr = terminator_instr.cast::<JumpInstruction>();
            let target_node_id = block_to_node_id(instr.target() as usize);

            graph.add_edge(node_id, &target_node_id);
        }
        OpCode::Branch => {
            let instr = terminator_instr.cast::<BranchInstruction>();
            let first_node_id = block_to_node_id(instr.first_branch() as usize);
            let second_node_id = block_to_node_id(instr.second_branch() as usize);

            graph.add_edge(node_id, &first_node_id);
            graph
                .add_edge(node_id, &second_node_id)
                .attribute("color", "gray50")
                .attribute("style", "dashed");
        }
        // Loop has implicit continue edge which falls through to the next block
        OpCode::Loop => {
            let instr = terminator_instr.cast::<LoopInstruction>();
            let end_node_id = block_to_node_id(instr.end_branch() as usize);

            graph
                .add_edge(node_id, &end_node_id)
                .attribute("label", "end");
            graph
                .add_edge(node_id, fallthrough_node_id)
                .attribute("label", "continue");
        }
        // Lookaround has implicit success edge which falls through to the next block
        OpCode::Lookaround => {
            let instr = terminator_instr.cast::<LookaroundInstruction>();
            let body_node_id = block_to_node_id(instr.body_branch() as usize);

            graph
                .add_edge(node_id, &body_node_id)
                .attribute("color", "blue")
                .attribute("style", "dotted")
                .attribute("label", "lookaround");
            graph.add_edge(node_id, fallthrough_node_id);
        }
        OpCode::Accept | OpCode::Fail => {}
        // No explicit control flow instruction means fall through to the next block.
        _ => {
            graph.add_edge(node_id, fallthrough_node_id);
        }
    }
}
