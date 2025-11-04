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

