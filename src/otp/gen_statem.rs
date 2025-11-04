//! GenStateM (Generic State Machine) behavior implementation

use crate::otp::base::OTPActorBehavior;
use crate::types::{SwErlState, SwErlMessage, SwErlResponse};
use crate::errors::SwErlError;
use crate::pid::Pid;
use crate::registrar::Registrar;
use crate::message::SendMessage;
use std::any::Any;

/// Trait defining the behavior for a generic state machine
pub trait GenStatemBehavior: OTPActorBehavior {
    /// This hook function is called if the send operator is used to send a message to a statem_behavior
    fn notify(&self, message: &dyn Any, state: &dyn Any);

    /// This hook function is used to create the first state of this state machine subtype's occurrence
    fn initialize(&self, initial_data: &dyn Any) -> Result<SwErlState, SwErlError>;

    /// This hook function is used to react to a request to unlink request
    fn unlinked(&self, message: &dyn Any, current_state: &dyn Any);

    /// This hook function is used to deal with the results of using the gen_statem.cast function
    fn handle_cast(&self, message: &dyn Any, current_state: &dyn Any) -> Result<SwErlState, SwErlError>;

    /// This hook function is used to deal with the results of using the gen_statem.call function
    fn handle_call(&self, message: &dyn Any, current_state: &dyn Any) -> Result<(SwErlResponse, SwErlState), SwErlError>;
}

/// Provides the API for creating, messaging, and managing GenStateM instances
pub enum GenStateM {}

impl GenStateM {
    /// Registers, by name, and prepares an occurrence of a specified sub-type of a generic state machine
    pub fn start_link<T: GenStatemBehavior + 'static>(
        _name: &str,
        statem: T,
        initial_data: &dyn Any,
    ) -> Result<Pid, SwErlError> {
        let registrar = Registrar::local();
        let pid = registrar.generate_pid();

        // Initialize state
        let _state = statem.initialize(initial_data)?;

        // Create process with GenStateM handler
        // Note: This is a simplified implementation
        // Full implementation would need to integrate with ProcessHandler
        
        Ok(pid)
    }

    /// Registers, by name, and prepares an occurrence of a specified sub-type of a generic state machine (global)
    pub fn start_link_globally<T: GenStatemBehavior + 'static>(
        name: &str,
        statem: T,
        initial_data: &dyn Any,
    ) -> Result<Pid, SwErlError> {
        // Similar to start_link but uses global registrar
        Self::start_link(name, statem, initial_data)
    }

    /// Unlinks the information of an occurrence of a generic state machine's sub-type
    pub fn unlink(name: &str, _message: SwErlMessage) -> SwErlResponse {
        let registrar = Registrar::local();
        
        if let Some(pid) = registrar.get_pid(name) {
            if let Some(process) = registrar.get_process(pid) {
                if process.get_state().is_some() {
                    // Call unlinked hook if it's a GenStateM
                    registrar.unlink_by_name(name);
                    return Ok(None);
                }
            }
        }
        Err(SwErlError::NotRegisteredByName)
    }

    /// Sends a concurrent message to a registered occurrence of a generic state machine sub-type
    pub fn cast(name: &str, message: SwErlMessage) {
        // Asynchronous cast - no return value expected
        let _ = name.send(message);
    }

    /// Sends a concurrent message to a registered occurrence of a generic state machine sub-type (by PID)
    pub fn cast_pid(pid: Pid, message: SwErlMessage) {
        // Asynchronous cast - no return value expected
        let _ = pid.send(message);
    }

    /// Sends a concurrent, non-blocking message to a registered occurrence of a generic state machine sub-type
    pub fn notify(name: &str, message: SwErlMessage) {
        // Non-blocking notification
        let _ = name.send(message);
    }

    /// Sends a concurrent, non-blocking message to a registered occurrence of a generic state machine sub-type (by PID)
    pub fn notify_pid(pid: Pid, message: SwErlMessage) {
        // Non-blocking notification
        let _ = pid.send(message);
    }

    /// Sends a message to a registered occurrence of a generic state machine sub-type and waits for a response
    pub fn call(name: &str, message: SwErlMessage) -> SwErlResponse {
        name.send(message)
    }

    /// Sends a message to a registered occurrence of a generic state machine sub-type and waits for a response (by PID)
    pub fn call_pid(pid: Pid, message: SwErlMessage) -> SwErlResponse {
        pid.send(message)
    }
}
