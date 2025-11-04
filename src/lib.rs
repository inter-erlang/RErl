//! # RuErl
//!
//! RuErl is a Rust reengineering of SwErl, an Erlang/OTP-inspired concurrency library.
//! It provides lightweight processes, message-passing concurrency, and OTP-style behaviors.
//!
//! ## Features
//!
//! - Lightweight processes with message passing
//! - Thread-safe process registry
//! - OTP behaviors: GenServer, GenStateM, EventManager
//! - Safe data structures
//! - Async and sync process support via Tokio
//!
//! ## Example
//!
//! ```no_run
//! use rerl::*;
//! use rerl::SendMessage;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), SwErlError> {
//!     let pid = ProcessBuilder::new()
//!         .r#async()
//!         .stateless()
//!         .name("my_process")
//!         .spawn_async_stateless(|pid, message| {
//!             println!("Received: {:?}", message);
//!         })?;
//!
//!     "my_process".send(Box::new("Hello, RuErl!".to_string()))?;
//!     Ok(())
//! }
//! ```

pub mod errors;
pub mod types;
pub mod pid;
pub mod process;
pub mod registrar;
pub mod message;
pub mod builder;
pub mod safe_data;
pub mod otp;

// Re-export public API
pub use errors::{SwErlError, SwErlPassed};
pub use types::{SwErlState, SwErlMessage, SwErlResponse, RegistrationType};
pub use pid::{Pid, SwErlAtom, SwErlRef, SwErlNewerRef};
pub use process::SwErlProcess;
pub use registrar::Registrar;
pub use message::SendMessage;
pub use builder::ProcessBuilder;
pub use safe_data::{SafeDictCommand, build_safe};
pub use otp::{OTPActorBehavior, GenServer, GenServerBehavior, GenStateM, GenStatemBehavior, EventManager};
