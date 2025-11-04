//! EventManager behavior implementation

use crate::types::SwErlMessage;
use crate::errors::SwErlError;
use crate::pid::Pid;
use crate::registrar::Registrar;
use crate::message::SendMessage;
use std::sync::Arc;

/// Type alias for event handler functions
pub type SwErlStatelessHandler = Arc<dyn Fn(Pid, SwErlMessage) + Send + Sync>;

/// Provides the API for creating, messaging, and managing EventManager instances
pub enum EventManager {}

impl EventManager {
    /// Registers and prepares a specified event manager with a given name
    pub fn link(
        _name: &str,
        _initial_handlers: Vec<SwErlStatelessHandler>,
    ) -> Result<Pid, SwErlError> {
        let registrar = Registrar::local();
        let pid = registrar.generate_pid();

        // Create process with EventManager handler
        // Note: This is a simplified implementation
        // Full implementation would need to integrate with ProcessHandler
        
        Ok(pid)
    }

    /// Unlinks the information of an occurrence of an event manager
    pub fn unlink(name: &str) -> Result<(), SwErlError> {
        let registrar = Registrar::local();
        
        if let Some(pid) = registrar.get_pid(name) {
            if registrar.get_process(pid).is_some() {
                registrar.unlink_by_name(name);
                return Ok(());
            }
        }
        Err(SwErlError::NotRegisteredByName)
    }

    /// Sends a concurrent, non-blocking message to a registered occurrence of an event manager
    pub fn notify(name: &str, message: SwErlMessage) {
        // Notify all handlers asynchronously
        let _ = name.send(message);
    }

    /// Sends a concurrent, non-blocking message to a registered occurrence of an event manager (by PID)
    pub fn notify_pid(pid: Pid, message: SwErlMessage) {
        // Notify all handlers asynchronously
        let _ = pid.send(message);
    }
}
