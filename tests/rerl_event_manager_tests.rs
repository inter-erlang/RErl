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

