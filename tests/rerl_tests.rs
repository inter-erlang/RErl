// Copyright (c) 2025 Lee Barney, Sam Velasquez
//
// MIT License
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

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

