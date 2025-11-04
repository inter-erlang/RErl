//! Process registry for local and global process management

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicU64, Ordering};
use crate::pid::Pid;
use crate::process::SwErlProcess;
use crate::errors::SwErlError;
use crate::types::RegistrationType;

/// Thread-safe process ID counter
struct ProcessIDCounter {
    counter: AtomicU64,
}

impl ProcessIDCounter {
    fn new() -> Self {
        Self {
            counter: AtomicU64::new(0),
        }
    }

    fn next(&self) -> (u32, u32) {
        let value = self.counter.fetch_add(1, Ordering::SeqCst);
        let id = (value >> 32) as u32;
        let serial = (value & 0xFFFFFFFF) as u32;
        (id, serial)
    }
}

/// The Registrar manages local and global mappings of names and Pids to RuErl processes
pub struct Registrar {
    /// Dictionary mapping Pids to SwErl processes
    processes: Arc<RwLock<HashMap<Pid, Arc<SwErlProcess>>>>,
    /// Dictionary mapping unique names to Pids
    name_map: Arc<RwLock<HashMap<String, Pid>>>,
    /// Thread-safe process counter
    pid_counter: Arc<ProcessIDCounter>,
}

// Global instances for local and global registrars
static LOCAL: std::sync::OnceLock<Registrar> = std::sync::OnceLock::new();
static GLOBAL: std::sync::OnceLock<Registrar> = std::sync::OnceLock::new();

impl Registrar {
    /// Returns the local registrar instance
    pub fn local() -> &'static Registrar {
        LOCAL.get_or_init(|| Registrar::new())
    }

    /// Returns the global registrar instance
    pub fn global() -> &'static Registrar {
        GLOBAL.get_or_init(|| Registrar::new())
    }

    fn new() -> Self {
        Self {
            processes: Arc::new(RwLock::new(HashMap::new())),
            name_map: Arc::new(RwLock::new(HashMap::new())),
            pid_counter: Arc::new(ProcessIDCounter::new()),
        }
    }

    /// Generates a new Pid using a thread-safe shared counter
    pub fn generate_pid(&self) -> Pid {
        let (id, serial) = self.pid_counter.next();
        Pid {
            id,
            serial,
            creation: 0,
        }
    }

    /// Links a SwErl process to a Pid or name
    pub fn link(
        &self,
        process: Arc<SwErlProcess>,
        _registration_type: RegistrationType,
        name: Option<String>,
    ) -> Result<(), SwErlError> {
        let mut processes = self.processes.write().unwrap();
        let mut name_map = self.name_map.write().unwrap();

        // Check if PID is already linked
        if processes.contains_key(&process.pid) {
            return Err(SwErlError::ProcessAlreadyLinked);
        }

        // Check if name is already linked
        if let Some(ref n) = name {
            if name_map.contains_key(n) {
                return Err(SwErlError::ProcessAlreadyLinked);
            }
            name_map.insert(n.clone(), process.pid);
        }

        processes.insert(process.pid, process);
        Ok(())
    }

    /// Unlinks a process by Pid
    pub fn unlink(&self, pid: Pid) {
        let mut processes = self.processes.write().unwrap();
        let mut name_map = self.name_map.write().unwrap();

        if let Some(process) = processes.remove(&pid) {
            if let Some(ref name) = process.name {
                name_map.remove(name);
            }
        }
    }

    /// Unlinks a process by name
    pub fn unlink_by_name(&self, name: &str) {
        let mut processes = self.processes.write().unwrap();
        let mut name_map = self.name_map.write().unwrap();

        if let Some(pid) = name_map.remove(name) {
            processes.remove(&pid);
        }
    }

    /// Gets a process by Pid
    pub fn get_process(&self, pid: Pid) -> Option<Arc<SwErlProcess>> {
        let processes = self.processes.read().unwrap();
        processes.get(&pid).cloned()
    }

    /// Gets a Pid by name
    pub fn get_pid(&self, name: &str) -> Option<Pid> {
        let name_map = self.name_map.read().unwrap();
        name_map.get(name).copied()
    }

    /// Gets all Pids
    pub fn get_all_pids(&self) -> Vec<Pid> {
        let processes = self.processes.read().unwrap();
        processes.keys().copied().collect()
    }

    /// Gets all names
    pub fn get_all_names(&self) -> Vec<String> {
        let name_map = self.name_map.read().unwrap();
        name_map.keys().cloned().collect()
    }

    /// Checks if a Pid is linked
    pub fn pid_linked(&self, pid: Pid) -> bool {
        let processes = self.processes.read().unwrap();
        processes.contains_key(&pid)
    }

    /// Checks if a name is linked
    pub fn name_linked(&self, name: &str) -> bool {
        let name_map = self.name_map.read().unwrap();
        name_map.contains_key(name)
    }
}
