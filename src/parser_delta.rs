use crate::parser::{NodeId, NodeType};

#[derive(Debug)]
pub struct ParserDelta {
    pub node_id_offset: usize,
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    pub node_types: Vec<NodeType>,
}

impl ParserDelta {
    pub fn new(node_id_offset: usize) -> Self {
        Self {
            node_id_offset,
            span_start: vec![],
            span_end: vec![],
            node_types: vec![],
        }
    }

    pub fn print(&self) {
        if self.node_types.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.node_types.len() - 1), 0)
        }
    }

    fn print_helper(&self, node_id: &NodeId, indent: usize) {
        for _ in 0..indent {
            print!(" ")
        }

        match &self.node_types[node_id.0] {
            NodeType::Let {
                variable_name,
                initializer,
            } => {
                println!(
                    "Let ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::LetMut {
                variable_name,
                initializer,
            } => {
                println!(
                    "Mut ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::Param { name, ty } => {
                println!(
                    "Param ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
            }
            NodeType::Closure { params, block } => {
                println!(
                    "Closure ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(params, indent + 2);
                self.print_helper(block, indent + 2);
            }
            NodeType::Block(nodes) => {
                println!(
                    "Block ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            NodeType::Params(nodes) => {
                print!(
                    "Params ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                if nodes.is_empty() {
                    println!(" <empty>");
                } else {
                    println!();
                }

                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            NodeType::Call { head, args } => {
                println!(
                    "Call ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            NodeType::BinaryOp { lhs, op, rhs } => {
                println!(
                    "BinaryOp ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::Range { lhs, rhs } => {
                println!(
                    "Range ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::If {
                condition,
                then_block,
                else_expression,
            } => {
                println!(
                    "If ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            x => {
                println!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0],
                )
            }
        }
    }
}
