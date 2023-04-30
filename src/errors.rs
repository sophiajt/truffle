use crate::parser::NodeId;

#[derive(Debug)]
pub struct ScriptError {
    pub message: String,
    pub node_id: NodeId,
}

impl ScriptError {
    // pub fn new(message: impl Into<String>, node_id: NodeId) -> ScriptError {
    //     ScriptError {
    //         message: message.into(),
    //         node_id,
    //     }
    // }
}
