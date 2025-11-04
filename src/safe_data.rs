//! Safe data structures

use std::collections::HashMap;
use crate::types::SwErlState;
use crate::errors::SwErlError;
use crate::builder::ProcessBuilder;

/// Defines commands for interacting with a thread-safe dictionary
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SafeDictCommand {
    /// Adds a new key-value pair to the dictionary
    Add,
    /// Removes the key-value pair associated with the specified key
    Remove,
    /// Retrieves the value associated with the specified key
    Get,
    /// Retrieves all keys from the dictionary
    GetKeys,
    /// Retrieves all values from the dictionary
    GetValues,
    /// Retrieves an unsafe copy of the raw dictionary object
    GetRaw,
}

/// Constructs a thread-safe dictionary with specified initial state
pub fn build_safe<K, V>(dictionary: HashMap<K, V>, name: &str) -> Result<(), SwErlError>
where
    K: Send + Sync + 'static + Clone + std::hash::Hash + Eq,
    V: Send + Sync + 'static + Clone,
{
    let initial_state: SwErlState = Box::new(dictionary);

    ProcessBuilder::new()
        .sync()
        .stateful()
        .name(name)
        .initial_state(initial_state)
        .spawn_sync_stateful(move |_pid, _message, state| {
            let mut state_guard = state.lock().unwrap();
            
            // Try to extract the dictionary from state
            let dict = state_guard.downcast_mut::<HashMap<K, V>>();
            
            match dict {
                Some(d) => {
                    // Parse message
                    // This is a simplified implementation
                    // Full implementation would need proper message parsing
                    
                    let response = Ok(None);
                    let new_state: SwErlState = Box::new(d.clone());
                    (response, new_state)
                }
                None => {
                    let response = Err(SwErlError::InvalidState);
                    let new_state: SwErlState = Box::new(HashMap::<K, V>::new());
                    (response, new_state)
                }
            }
        })?;

    Ok(())
}
