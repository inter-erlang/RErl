//! Type aliases and shared types for RuErl

use crate::errors::SwErlError;

/// Type alias for SwErl state values
/// Must be Send + Sync for thread safety
pub type SwErlState = Box<dyn std::any::Any + Send + Sync>;

/// Type alias for SwErl message values
/// Must be Send + Sync for thread safety
pub type SwErlMessage = Box<dyn std::any::Any + Send + Sync>;

/// Type alias for SwErl response values
/// Using Result pattern instead of tuple
pub type SwErlResponse = Result<Option<Box<dyn std::any::Any + Send + Sync>>, SwErlError>;

/// Enum representing different types of registration for SwErl processes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegistrationType {
    /// Local process registration within a node
    Local,
    /// Global process registration across nodes
    Global,
}
