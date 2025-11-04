//! Message sending traits and implementations

use crate::pid::{Pid, SwErlAtom};
use crate::types::SwErlMessage;
use crate::errors::SwErlError;
use crate::registrar::Registrar;
use crate::types::SwErlResponse;

/// Trait for sending messages to processes
/// Replaces Swift's ! operator with send() method (Decision 2)
pub trait SendMessage {
    fn send(&self, message: SwErlMessage) -> SwErlResponse;
}

impl SendMessage for Pid {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        let registrar = Registrar::local();
        if let Some(process) = registrar.get_process(*self) {
            process.send_message(message)
                .map(|_| None)
                .map_err(|e| e)
        } else {
            Err(SwErlError::NotRegisteredByPid)
        }
    }
}

impl SendMessage for str {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        let registrar = Registrar::local();
        if let Some(pid) = registrar.get_pid(self) {
            pid.send(message)
        } else {
            Err(SwErlError::NotRegisteredByName)
        }
    }
}

impl SendMessage for String {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        self.as_str().send(message)
    }
}

impl SendMessage for SwErlAtom {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        if let Some(name) = self.string() {
            name.send(message)
        } else {
            Err(SwErlError::BadAtom)
        }
    }
}
