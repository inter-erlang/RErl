//! Error types for RuErl

use std::fmt;

/// Enumeration representing SwErl related errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SwErlError {
    /// Indicates that there is a process currently registered with the specified name
    ProcessAlreadyLinked,
    /// Indicates that no process is registered with the specified name
    NotRegisteredByName,
    /// Indicates that no process is registered with the specified process identifier (Pid)
    NotRegisteredByPid,
    /// Indicates that a behavior other than the expected GenServer was encountered
    NotGenServerBehavior,
    /// Indicates that a behavior other than the expected Statem was encountered
    NotStatemBehavior,
    /// Indicates that the Statem behavior was encountered without a valid state
    StatemBehaviorWithoutState,
    /// Indicates that an unknown command was received
    InvalidCommand,
    /// Indicates that the state provided is invalid for the given operation
    InvalidState,
    /// Indicates that a 'Data' is not a valid Erlang external interchange format
    InvalidExternalType,
    /// Indicates that a provided value is invalid for the given context
    InvalidValue,
    /// Indicates that a required closure is missing for the specified operation
    MissingClosure,
    /// Indicates an invalid port encountered during interaction
    InvalidPort,
    /// Indicates that the IP address associated with a given port was not found
    IpNotFound,
    /// Indicates an attempt to start a process that has already been started
    AlreadyStarted,
    /// Indicates an invalid message
    InvalidMessage,
    /// Indicates a bad atom
    BadAtom,
}

impl fmt::Display for SwErlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SwErlError::ProcessAlreadyLinked => write!(f, "Process already linked"),
            SwErlError::NotRegisteredByName => write!(f, "Not registered by name"),
            SwErlError::NotRegisteredByPid => write!(f, "Not registered by Pid"),
            SwErlError::NotGenServerBehavior => write!(f, "Not GenServer behavior"),
            SwErlError::NotStatemBehavior => write!(f, "Not Statem behavior"),
            SwErlError::StatemBehaviorWithoutState => write!(f, "Statem behavior without state"),
            SwErlError::InvalidCommand => write!(f, "Invalid command"),
            SwErlError::InvalidState => write!(f, "Invalid state"),
            SwErlError::InvalidExternalType => write!(f, "Invalid external type"),
            SwErlError::InvalidValue => write!(f, "Invalid value"),
            SwErlError::MissingClosure => write!(f, "Missing closure"),
            SwErlError::InvalidPort => write!(f, "Invalid port"),
            SwErlError::IpNotFound => write!(f, "IP not found"),
            SwErlError::AlreadyStarted => write!(f, "Already started"),
            SwErlError::InvalidMessage => write!(f, "Invalid message"),
            SwErlError::BadAtom => write!(f, "Bad atom"),
        }
    }
}

impl std::error::Error for SwErlError {}

/// Enum used in the values of some SwErl functions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwErlPassed {
    Ok,
    Fail,
}
