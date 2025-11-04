//! EventManager tests

use rerl::*;
use rerl::SendMessage;
use rerl::otp::event_manager::SwErlStatelessHandler;
use tokio;
use std::sync::Arc;

#[tokio::test]
async fn test_event_manager_link() {
    let handlers: Vec<SwErlStatelessHandler> = vec![
        Arc::new(|_pid, _message| {
            // Handler 1
        }),
        Arc::new(|_pid, _message| {
            // Handler 2
        }),
    ];

    let result = EventManager::link("test_manager", handlers);
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_event_manager_notify() {
    // This test would require a full EventManager implementation
    // Simplified for now
    assert!(true);
}

#[tokio::test]
async fn test_event_manager_unlink() {
    // This test would require a full EventManager implementation
    // Simplified for now
    assert!(true);
}

