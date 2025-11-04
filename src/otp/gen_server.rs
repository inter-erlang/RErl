//! GenServer behavior implementation

use crate::otp::base::OTPActorBehavior;
use crate::types::{SwErlState, SwErlMessage, SwErlResponse};
use crate::errors::SwErlError;
use crate::pid::Pid;
use crate::registrar::Registrar;
use crate::message::SendMessage;
use std::any::Any;

/// Trait defining the behavior for a generic server (GenServer)
pub trait GenServerBehavior: OTPActorBehavior {
    /// Called during server startup, allowing for the initial state modification or validation
    fn initialize_data(&self, data: Option<&dyn Any>) -> Result<SwErlState, SwErlError>;

    /// Executed before a server is removed from the registry, typically for cleanup
    fn terminate_cleanup(&self, reason: &str, data: Option<&dyn Any>);

    /// Handles asynchronous cast messages, where no reply is expected by the sender
    fn handle_cast(&self, request: &dyn Any, data: &dyn Any) -> Result<SwErlState, SwErlError>;

    /// Handles synchronous call messages, where the sender awaits a reply
    fn handle_call(&self, request: &dyn Any, data: &dyn Any) -> Result<(Box<dyn Any + Send + Sync>, SwErlState), SwErlError>;
}

/// Provides the API for creating, messaging, and managing GenServer instances
pub enum GenServer {}

impl GenServer {
    /// Initializes and registers a GenServerBehavior instance, uniquely identified by a name
    pub fn start_link<T: GenServerBehavior + 'static>(
        name: &str,
        behavior: T,
        initial_state: Option<SwErlState>,
    ) -> Result<String, SwErlError> {
        let registrar = Registrar::local();
        let _pid = registrar.generate_pid();

        // Initialize data
        let _state = behavior.initialize_data(initial_state.as_ref().map(|s| s.as_ref() as &dyn Any))
            .map_err(|_| SwErlError::InvalidState)?;

        // Create process with GenServer handler
        // Note: This is a simplified implementation
        // Full implementation would need to integrate with ProcessHandler
        
        Ok(name.to_string())
    }

    /// Sends a message to a GenServerBehavior identified by its registered name
    pub fn notify(name: &str, message: SwErlMessage) -> Result<(), SwErlError> {
        name.send(message)?;
        Ok(())
    }

    /// Sends a message to a GenServerBehavior instance identified by its PID
    pub fn notify_pid(pid: Pid, message: SwErlMessage) -> Result<(), SwErlError> {
        pid.send(message)?;
        Ok(())
    }

    /// Terminates a GenServerBehavior identified by its registered name
    pub fn unlink(name: &str, _reason: &str) -> Result<(), SwErlError> {
        let registrar = Registrar::local();
        
        if let Some(pid) = registrar.get_pid(name) {
            if registrar.get_process(pid).is_some() {
                // Call terminate_cleanup if it's a GenServer
                // Simplified - would need proper type checking
                registrar.unlink_by_name(name);
                return Ok(());
            }
        }
        Err(SwErlError::NotRegisteredByName)
    }

    /// Terminates a GenServerBehavior identified by its PID
    pub fn unlink_pid(pid: Pid, _reason: &str) -> Result<(), SwErlError> {
        let registrar = Registrar::local();
        
        if registrar.get_process(pid).is_some() {
            // Call terminate_cleanup if it's a GenServer
            registrar.unlink(pid);
            return Ok(());
        }
        Err(SwErlError::NotRegisteredByPid)
    }

    /// Asynchronously sends a cast message to a GenServerBehavior instance identified by its name
    pub fn cast(name: &str, message: SwErlMessage) -> Result<(), SwErlError> {
        name.send(message)?;
        Ok(())
    }

    /// Asynchronously sends a cast message to a GenServerBehavior instance identified by its PID
    pub fn cast_pid(pid: Pid, message: SwErlMessage) -> Result<(), SwErlError> {
        pid.send(message)?;
        Ok(())
    }

    /// Synchronously sends a call message to a GenServerBehavior instance and waits for a reply
    pub fn call(name: &str, message: SwErlMessage) -> SwErlResponse {
        name.send(message)
    }

    /// Synchronously sends a call message to a GenServerBehavior identified by its PID and waits for a reply
    pub fn call_pid(pid: Pid, message: SwErlMessage) -> SwErlResponse {
        pid.send(message)
    }
}
