//! OTP (Open Telecom Platform) behaviors module
//! Contains GenServer, GenStateM, and EventManager implementations

pub mod base;
pub mod gen_server;
pub mod gen_statem;
pub mod event_manager;

pub use base::OTPActorBehavior;
pub use gen_server::{GenServer, GenServerBehavior};
pub use gen_statem::{GenStateM, GenStatemBehavior};
pub use event_manager::EventManager;
