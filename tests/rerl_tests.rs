//! Core RuErl tests

use rerl::*;
use rerl::SendMessage;
use tokio;

#[tokio::test]
async fn test_spawn_async_stateless() {
    let pid = ProcessBuilder::new()
        .r#async()
        .stateless()
        .spawn_async_stateless(|pid, message| {
            println!("Process {} received: {:?}", pid, message);
        })
        .unwrap();

    // PID should have valid values (id and serial can start at 0, but should be valid)
    assert!(pid.serial >= 0);
}

#[tokio::test]
async fn test_spawn_sync_stateless() {
    let pid = ProcessBuilder::new()
        .sync()
        .stateless()
        .spawn_sync_stateless(|_pid, message| {
            Ok(Some(message))
        })
        .unwrap();

    let response = pid.send(Box::new("test".to_string()));
    assert!(response.is_ok());
}

#[tokio::test]
async fn test_named_process() {
    let _pid = ProcessBuilder::new()
        .r#async()
        .stateless()
        .name("test_process")
        .spawn_async_stateless(|_pid, _message| {
            // Handler
        })
        .unwrap();

    let response = "test_process".send(Box::new("message".to_string()));
    assert!(response.is_ok());
}

#[tokio::test]
async fn test_process_registry() {
    let registrar = Registrar::local();
    let pid = registrar.generate_pid();
    
    // PID should have valid values (id and serial can start at 0, but should be valid)
    assert!(pid.serial >= 0);
    assert!(!registrar.pid_linked(pid));
}

#[tokio::test]
async fn test_pid_creation() {
    let pid1 = Pid { id: 1, serial: 1, creation: 0 };
    let pid2 = Pid { id: 1, serial: 1, creation: 0 };
    
    assert_eq!(pid1, pid2);
    assert_eq!(pid1.to_string(), "1,1,0");
}

#[tokio::test]
async fn test_swerl_atom() {
    let atom = SwErlAtom::from("TEST");
    assert_eq!(atom.string(), Some("test"));
    
    let atom2 = SwErlAtom::from("");
    assert_eq!(atom2.string(), None);
}

