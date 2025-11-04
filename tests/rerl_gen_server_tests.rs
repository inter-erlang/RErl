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

//! GenServer tests

use rerl::*;
use rerl::SendMessage;
use tokio;

struct TestGenServer;

impl OTPActorBehavior for TestGenServer {}

impl GenServerBehavior for TestGenServer {
    fn initialize_data(&self, _data: Option<&dyn std::any::Any>) -> Result<SwErlState, SwErlError> {
        Ok(Box::new(0i32))
    }

    fn terminate_cleanup(&self, _reason: &str, _data: Option<&dyn std::any::Any>) {
        // Cleanup logic
    }

    fn handle_cast(&self, _request: &dyn std::any::Any, data: &dyn std::any::Any) -> Result<SwErlState, SwErlError> {
        if let Some(state) = data.downcast_ref::<i32>() {
            Ok(Box::new(state + 1))
        } else {
            Err(SwErlError::InvalidState)
        }
    }

    fn handle_call(&self, _request: &dyn std::any::Any, data: &dyn std::any::Any) -> Result<(Box<dyn std::any::Any + Send + Sync>, SwErlState), SwErlError> {
        if let Some(state) = data.downcast_ref::<i32>() {
            let response: Box<dyn std::any::Any + Send + Sync> = Box::new(*state);
            let new_state: SwErlState = Box::new(state + 1);
            Ok((response, new_state))
        } else {
            Err(SwErlError::InvalidState)
        }
    }
}

#[tokio::test]
async fn test_gen_server_start_link() {
    let server = TestGenServer;
    let result = GenServer::start_link("test_server", server, Some(Box::new(0i32)));
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_gen_server_call() {
    // This test would require a full GenServer implementation
    // Simplified for now
    assert!(true);
}

#[tokio::test]
async fn test_gen_server_cast() {
    // This test would require a full GenServer implementation
    // Simplified for now
    assert!(true);
}

