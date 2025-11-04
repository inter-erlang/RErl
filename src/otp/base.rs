//! Base trait for all OTP actors

/// Base trait for all non-process RuErl actors
/// Since it is the base, all types of RuErl actors can be accumulated
/// in shared data structures without resorting to using Rust's Any type
pub trait OTPActorBehavior: Send + Sync {
    // Base trait with no required methods
    // Sub-traits will define specific behavior requirements
}
