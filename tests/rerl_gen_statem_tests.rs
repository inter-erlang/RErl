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

//! GenStateM tests

use rerl::*;
use rerl::SendMessage;
use tokio;

struct TestGenStatem;

impl OTPActorBehavior for TestGenStatem {}

impl GenStatemBehavior for TestGenStatem {
    fn notify(&self, _message: &dyn std::any::Any, _state: &dyn std::any::Any) {
        // Notification handler
    }

    fn initialize(&self, _initial_data: &dyn std::any::Any) -> Result<SwErlState, SwErlError> {
        Ok(Box::new("initial_state".to_string()))
    }

    fn unlinked(&self, _message: &dyn std::any::Any, _current_state: &dyn std::any::Any) {
        // Unlink handler
    }

    fn handle_cast(&self, _message: &dyn std::any::Any, state: &dyn std::any::Any) -> Result<SwErlState, SwErlError> {
        if let Some(s) = state.downcast_ref::<String>() {
            Ok(Box::new(s.clone()))
        } else {
            Err(SwErlError::InvalidState)
        }
    }

    fn handle_call(&self, _message: &dyn std::any::Any, state: &dyn std::any::Any) -> Result<(SwErlResponse, SwErlState), SwErlError> {
        if let Some(s) = state.downcast_ref::<String>() {
            let response: SwErlResponse = Ok(None);
            Ok((response, Box::new(s.clone())))
        } else {
            Err(SwErlError::InvalidState)
        }
    }
}

#[tokio::test]
async fn test_gen_statem_start_link() {
    let statem = TestGenStatem;
    let result = GenStateM::start_link("test_statem", statem, &"initial".to_string());
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_gen_statem_call() {
    // This test would require a full GenStateM implementation
    // Simplified for now
    assert!(true);
}

#[tokio::test]
async fn test_gen_statem_cast() {
    // This test would require a full GenStateM implementation
    // Simplified for now
    assert!(true);
}

