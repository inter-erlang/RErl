//! Process representation and message handling

use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use crate::types::{SwErlState, SwErlMessage, SwErlResponse, RegistrationType};
use crate::pid::Pid;
use crate::errors::SwErlError;

/// Handler types for different process behaviors
pub enum ProcessHandler {
    /// Asynchronous stateless process handler
    AsyncStateless(Box<dyn Fn(Pid, SwErlMessage) + Send + Sync>),
    /// Synchronous stateless process handler
    SyncStateless(Box<dyn Fn(Pid, SwErlMessage) -> SwErlResponse + Send + Sync>),
    /// Asynchronous stateful process handler
    AsyncStateful(Box<dyn Fn(Pid, SwErlMessage, Arc<Mutex<SwErlState>>) -> SwErlState + Send + Sync>),
    /// Synchronous stateful process handler
    SyncStateful(Box<dyn Fn(Pid, SwErlMessage, Arc<Mutex<SwErlState>>) -> (SwErlResponse, SwErlState) + Send + Sync>),
}

/// Structure representing an Erlang-like process in Rust
/// State is embedded directly in the process struct per design decision
pub struct SwErlProcess {
    /// The registered Pid of the process
    pub pid: Pid,
    /// Embedded state (Decision 4: Process Struct)
    pub state: Option<Arc<Mutex<SwErlState>>>,
    /// Process handler
    pub handler: ProcessHandler,
    /// Registration type (local or global)
    pub registration_type: RegistrationType,
    /// Optional name for the process
    pub name: Option<String>,
    /// Message receiver for async processes
    pub receiver: Option<mpsc::UnboundedReceiver<SwErlMessage>>,
    /// Message sender for async processes
    pub sender: Option<mpsc::UnboundedSender<SwErlMessage>>,
}

impl SwErlProcess {
    /// Creates a new process with the given parameters
    pub fn new(
        pid: Pid,
        handler: ProcessHandler,
        registration_type: RegistrationType,
        name: Option<String>,
        initial_state: Option<SwErlState>,
    ) -> Self {
        let state = initial_state.map(|s| Arc::new(Mutex::new(s)));
        let (sender, receiver) = mpsc::unbounded_channel();
        
        Self {
            pid,
            state,
            handler,
            registration_type,
            name,
            receiver: Some(receiver),
            sender: Some(sender),
        }
    }

    /// Gets the state of the process
    pub fn get_state(&self) -> Option<Arc<Mutex<SwErlState>>> {
        self.state.clone()
    }

    /// Sets the state of the process
    pub fn set_state(&mut self, state: SwErlState) {
        self.state = Some(Arc::new(Mutex::new(state)));
    }

    /// Sends a message to this process
    pub fn send_message(&self, message: SwErlMessage) -> Result<(), SwErlError> {
        if let Some(sender) = &self.sender {
            sender.send(message).map_err(|_| SwErlError::InvalidMessage)?;
            Ok(())
        } else {
            Err(SwErlError::InvalidMessage)
        }
    }

    /// Gets the message sender for this process
    pub fn get_sender(&self) -> Option<mpsc::UnboundedSender<SwErlMessage>> {
        self.sender.clone()
    }
}
