//! Process identifiers and related types

use std::hash::Hash;
use std::fmt;

/// Structure representing a Process Identifier (Pid) in RuErl
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Pid {
    /// When `serial` would overflow UInt32, `id` is incremented and `serial` is reset to 1
    pub id: u32,
    /// The serial number of the process
    pub serial: u32,
    /// A unique identifier of the originator of the process.
    /// This is zero when the process is local, non-zero when the process is a remote node.
    pub creation: u32,
}

impl Pid {
    /// Converts the Pid into a comma-separated string representation
    pub fn to_string(&self) -> String {
        format!("{},{},{}", self.id, self.serial, self.creation)
    }
}

impl fmt::Display for Pid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

/// Structure representing an Identifier of a process
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct SwErlRef {
    /// The atom indicating the node where the process exists
    pub node: SwErlAtom,
    /// The unique identifier of the originator of the process
    pub id: u32,
    /// The creation count associated with the process
    pub creation: u32,
}

/// Structure representing an Erlang Newer Identifier for a process
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct SwErlNewerRef {
    /// The atom indicating the node where the process exists
    pub node: SwErlAtom,
    /// The creation count associated with the process
    pub creation: u32,
    /// The unique identifier of the originator of the process (Data of at most 5 bytes)
    pub id: Vec<u8>,
}

/// Structure representing an Erlang-like atom in RuErl
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct SwErlAtom {
    /// The string value of the atom, stored in a lowercase form
    value: Option<String>,
}

impl SwErlAtom {
    /// Initializes an atom with the provided string value
    pub fn new(value: String) -> Self {
        let value = if value.is_empty() {
            None
        } else {
            Some(value.to_lowercase())
        };
        Self { value }
    }

    /// Returns the optional string value of the atom
    pub fn string(&self) -> Option<&str> {
        self.value.as_deref()
    }

    /// Returns the optional string value as bytes in UTF8
    pub fn utf8(&self) -> Option<Vec<u8>> {
        self.value.as_ref().map(|s| s.as_bytes().to_vec())
    }
}

impl From<&str> for SwErlAtom {
    fn from(s: &str) -> Self {
        SwErlAtom::new(s.to_string())
    }
}

impl From<String> for SwErlAtom {
    fn from(s: String) -> Self {
        SwErlAtom::new(s)
    }
}
