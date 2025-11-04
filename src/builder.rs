//! Process builder for spawning processes
//! Uses unified builder pattern (Decision 5)

use crate::pid::Pid;
use crate::process::{SwErlProcess, ProcessHandler};
use crate::types::{SwErlState, SwErlMessage, SwErlResponse, RegistrationType};
use crate::errors::SwErlError;
use crate::registrar::Registrar;
use std::sync::Arc;

/// Builder for creating new processes
/// Provides fluent API for configuring process behavior
pub struct ProcessBuilder {
    is_async: bool,
    is_stateless: bool,
    name: Option<String>,
    registration_type: RegistrationType,
    initial_state: Option<SwErlState>,
}

impl ProcessBuilder {
    /// Creates a new ProcessBuilder with default settings
    pub fn new() -> Self {
        Self {
            is_async: true,
            is_stateless: true,
            name: None,
            registration_type: RegistrationType::Local,
            initial_state: None,
        }
    }

    /// Sets the process to be asynchronous
    pub fn r#async(mut self) -> Self {
        self.is_async = true;
        self
    }

    /// Sets the process to be synchronous
    pub fn sync(mut self) -> Self {
        self.is_async = false;
        self
    }

    /// Sets the process to be stateless
    pub fn stateless(mut self) -> Self {
        self.is_stateless = true;
        self
    }

    /// Sets the process to be stateful
    pub fn stateful(mut self) -> Self {
        self.is_stateless = false;
        self
    }

    /// Sets the name for the process
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Sets the registration type
    pub fn registration_type(mut self, rt: RegistrationType) -> Self {
        self.registration_type = rt;
        self
    }

    /// Sets the initial state for stateful processes
    pub fn initial_state(mut self, state: SwErlState) -> Self {
        self.initial_state = Some(state);
        self
    }

    /// Spawns an asynchronous stateless process
    pub fn spawn_async_stateless<F>(self, handler: F) -> Result<Pid, SwErlError>
    where
        F: Fn(Pid, SwErlMessage) + Send + Sync + 'static,
    {
        let registrar = match self.registration_type {
            RegistrationType::Local => Registrar::local(),
            RegistrationType::Global => Registrar::global(),
        };

        let pid = registrar.generate_pid();
        let handler = ProcessHandler::AsyncStateless(Box::new(handler));
        let process = Arc::new(SwErlProcess::new(
            pid,
            handler,
            self.registration_type,
            self.name.clone(),
            None,
        ));

        registrar.link(process, self.registration_type, self.name)?;
        Ok(pid)
    }

    /// Spawns a synchronous stateless process
    pub fn spawn_sync_stateless<F>(self, handler: F) -> Result<Pid, SwErlError>
    where
        F: Fn(Pid, SwErlMessage) -> SwErlResponse + Send + Sync + 'static,
    {
        let registrar = match self.registration_type {
            RegistrationType::Local => Registrar::local(),
            RegistrationType::Global => Registrar::global(),
        };

        let pid = registrar.generate_pid();
        let handler = ProcessHandler::SyncStateless(Box::new(handler));
        let process = Arc::new(SwErlProcess::new(
            pid,
            handler,
            self.registration_type,
            self.name.clone(),
            None,
        ));

        registrar.link(process, self.registration_type, self.name)?;
        Ok(pid)
    }

    /// Spawns an asynchronous stateful process
    pub fn spawn_async_stateful<F>(self, handler: F) -> Result<Pid, SwErlError>
    where
        F: Fn(Pid, SwErlMessage, std::sync::Arc<std::sync::Mutex<SwErlState>>) -> SwErlState + Send + Sync + 'static,
    {
        let registrar = match self.registration_type {
            RegistrationType::Local => Registrar::local(),
            RegistrationType::Global => Registrar::global(),
        };

        let pid = registrar.generate_pid();
        let initial_state = self.initial_state.ok_or(SwErlError::InvalidState)?;
        let handler = ProcessHandler::AsyncStateful(Box::new(handler));
        let process = Arc::new(SwErlProcess::new(
            pid,
            handler,
            self.registration_type,
            self.name.clone(),
            Some(initial_state),
        ));

        registrar.link(process, self.registration_type, self.name)?;
        Ok(pid)
    }

    /// Spawns a synchronous stateful process
    pub fn spawn_sync_stateful<F>(self, handler: F) -> Result<Pid, SwErlError>
    where
        F: Fn(Pid, SwErlMessage, std::sync::Arc<std::sync::Mutex<SwErlState>>) -> (SwErlResponse, SwErlState) + Send + Sync + 'static,
    {
        let registrar = match self.registration_type {
            RegistrationType::Local => Registrar::local(),
            RegistrationType::Global => Registrar::global(),
        };

        let pid = registrar.generate_pid();
        let initial_state = self.initial_state.ok_or(SwErlError::InvalidState)?;
        let handler = ProcessHandler::SyncStateful(Box::new(handler));
        let process = Arc::new(SwErlProcess::new(
            pid,
            handler,
            self.registration_type,
            self.name.clone(),
            Some(initial_state),
        ));

        registrar.link(process, self.registration_type, self.name)?;
        Ok(pid)
    }

    // Note: Generic spawn method removed - use specific spawn methods
    // spawn_async_stateless, spawn_sync_stateless, spawn_async_stateful, spawn_sync_stateful
}

impl Default for ProcessBuilder {
    fn default() -> Self {
        Self::new()
    }
}
