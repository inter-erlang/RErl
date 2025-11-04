<!--
Copyright (c) 2025 Lee Barney

MIT License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->

# RuErl Design Decisions

This document captures all approved reengineering decisions for converting SwErl (Swift) to RuErl (Rust).

## Overview

RuErl is a Rust reengineering of SwErl, preserving behavioral equivalence while adapting to Rust idioms. SwErl is an Erlang/OTP-inspired concurrency library for Swift that provides lightweight processes, message-passing concurrency, and OTP-style behaviors.

## Approved Architecture Decisions

### 1. Concurrency Model: Tokio

**Decision:** Use Tokio runtime for async/await concurrency.

**Rationale:**
- Tokio provides excellent async/await support and runtime
- Allows preserving both async and sync process behaviors
- Industry-standard async runtime for Rust
- Excellent performance and ecosystem

**Implementation:**
- Processes run as Tokio tasks
- Message channels: `tokio::sync::mpsc` for async messaging
- Response channels: `tokio::sync::oneshot` for synchronous responses
- Both async and sync behaviors supported via Tokio runtime

### 2. Message Sending API: `send()` Method

**Decision:** Replace Swift's `!` infix operator with `send()` method.

**Rationale:**
- Rust doesn't support custom infix operators like Swift
- Method-based API is more idiomatic Rust
- Clear and explicit intent

**Implementation:**
- `pid.send(message)` for Pid-based sending
- `"name".send(message)` via trait extension for String
- `atom.send(message)` via trait extension for SwErlAtom
- Preserves same message-passing semantics as SwErl

### 3. Error Handling: `Result<T, E>`

**Decision:** Use Rust's `Result<T, E>` type instead of Swift's tuple-based responses.

**Rationale:**
- Idiomatic Rust error handling
- Compiler-enforced error handling
- Better integration with Rust ecosystem

**Implementation:**
- Convert `SwErlResponse` tuples `(SwErlPassed, Any?)` to `Result<Option<Value>, SwErlError>`
- `Ok(Some(value))` for success with value
- `Ok(None)` for success without value
- `Err(SwErlError)` for failures
- `SwErlPassed` enum becomes part of Result pattern

### 4. State Storage: Process Struct (Option B)

**Decision:** Store state directly in `SwErlProcess` struct rather than external registry.

**Rationale:**
- More idiomatic Rust (clear ownership)
- Better encapsulation (state with process)
- No extra lookup needed
- Compiler helps prevent leaks
- Aligns with Rust ownership model

**Trade-offs:**
- Process struct becomes larger (acceptable)
- Less direct match to SwErl design (but preserves behavior)
- State tied to process lifetime (appropriate)

**Implementation:**
- State stored as `Option<Arc<Mutex<SwErlState>>>` in `SwErlProcess`
- State lifetime tied to process lifetime
- Clear ownership and encapsulation

### 5. Process Spawning: Builder Pattern (Option B)

**Decision:** Use unified builder pattern instead of separate spawn functions.

**Rationale:**
- Single entry point reduces duplication
- Flexible and extensible API
- Idiomatic Rust builder pattern
- Better for future extensions

**Trade-offs:**
- Less direct match to SwErl API (but preserves behavior)
- More verbose usage (acceptable trade-off)
- Additional abstraction layer (beneficial)

**Implementation:**
- `ProcessBuilder` with fluent API
- Methods: `.async()`, `.sync()`, `.stateless()`, `.stateful()`, `.name()`, `.queue()`
- Example: `ProcessBuilder::new().async().stateless().name("my_process").spawn(handler)?`

## Module Structure

```
ruerl/
├── Cargo.toml
├── src/
│   ├── lib.rs              # Main entry, re-exports
│   ├── pid.rs              # Pid, SwErlAtom, SwErlRef types
│   ├── process.rs          # SwErlProcess with embedded state
│   ├── registrar.rs        # Process registry (local/global)
│   ├── errors.rs           # SwErlError enum
│   ├── message.rs          # Message sending traits and impls
│   ├── builder.rs          # ProcessBuilder for spawning
│   ├── types.rs            # Type aliases (SwErlState, SwErlMessage, etc.)
│   ├── safe_data.rs        # Safe data structures
│   └── otp/
│       ├── mod.rs          # OTP module entry
│       ├── base.rs         # OTPActorBehavior trait
│       ├── gen_server.rs   # GenServer implementation
│       ├── gen_statem.rs   # GenStateM implementation
│       └── event_manager.rs # EventManager implementation
├── tests/                  # Test modules
└── examples/              # Example code
```

## Type Mappings

### Core Type Mappings

| Swift Type | Rust Type | Notes |
|-----------|-----------|-------|
| `typealias SwErlState = Any` | `type SwErlState = Box<dyn Any + Send + Sync>` | Must be Send + Sync for thread safety |
| `typealias SwErlMessage = Any` | `type SwErlMessage = Box<dyn Any + Send + Sync>` | Must be Send + Sync for thread safety |
| `typealias SwErlResponse = (SwErlPassed, Any?)` | `type SwErlResponse = Result<Option<Box<dyn Any + Send + Sync>>, SwErlError>` | Using Result pattern |
| `DispatchQueue` | `tokio::runtime::Handle` or `tokio::task::JoinHandle` | Tokio equivalent |
| `@Sendable` closures | `Box<dyn Fn(...) + Send + Sync>` | Rust trait object |
| `Dictionary<K, V>` | `HashMap<K, V>` | Standard Rust collection |
| `String` (process names) | `String` | Same |

### Process ID Types

| Swift | Rust |
|------|------|
| `struct Pid` | `#[derive(Copy, Clone, Hash, Eq, PartialEq)] struct Pid` | Copy type for small integers |
| `struct SwErlAtom` | `#[derive(Clone, Hash, Eq, PartialEq)] struct SwErlAtom` | Contains String |
| `struct SwErlRef` | `#[derive(Clone, Hash, Eq, PartialEq)] struct SwErlRef` | Reference type |

### Error Types

| Swift | Rust |
|------|------|
| `enum SwErlError: Error` | `#[derive(Debug, Clone, PartialEq, Eq)] pub enum SwErlError` | Implements `std::error::Error` |
| `enum SwErlPassed` | Part of `Result<T, E>` pattern | No separate enum needed |

## Protocol to Trait Mappings

| Swift Protocol | Rust Trait |
|---------------|------------|
| `OTPActor_behavior` | `trait OTPActorBehavior` |
| `GenServerBehavior` | `trait GenServerBehavior` |
| `GenStatemBehavior` | `trait GenStatemBehavior` |

All traits use `&self` or static methods where appropriate.

## Process Structure

```rust
pub struct SwErlProcess {
    pub pid: Pid,
    pub state: Option<Arc<Mutex<SwErlState>>>, // Embedded state (Decision 4)
    pub handler: ProcessHandler,
    pub registration_type: RegistrationType,
    pub name: Option<String>,
    // ... other fields
}

pub enum ProcessHandler {
    AsyncStateless(Box<dyn Fn(Pid, SwErlMessage) + Send + Sync>),
    SyncStateless(Box<dyn Fn(Pid, SwErlMessage) -> SwErlResponse + Send + Sync>),
    AsyncStateful(Box<dyn Fn(Pid, SwErlMessage, Arc<Mutex<SwErlState>>) + Send + Sync>),
    SyncStateful(Box<dyn Fn(Pid, SwErlMessage, Arc<Mutex<SwErlState>>) -> (SwErlResponse, SwErlState) + Send + Sync>),
    GenServer(Arc<dyn GenServerBehavior + Send + Sync>),
    GenStatem(Arc<dyn GenStatemBehavior + Send + Sync>),
    EventManager(Vec<Box<dyn Fn(Pid, SwErlMessage) + Send + Sync>>),
}
```

## Registrar Structure

```rust
pub struct Registrar {
    processes: Arc<RwLock<HashMap<Pid, Arc<SwErlProcess>>>>,
    name_map: Arc<RwLock<HashMap<String, Pid>>>,
    pid_counter: Arc<AtomicU64>, // Thread-safe counter
}

impl Registrar {
    pub fn local() -> &'static Registrar { /* ... */ }
    pub fn global() -> &'static Registrar { /* ... */ }
}
```

## Process Spawning API

### Builder Pattern (Decision 5)

```rust
pub struct ProcessBuilder {
    async: bool,
    stateless: bool,
    name: Option<String>,
    registration_type: RegistrationType,
    // ... other fields
}

impl ProcessBuilder {
    pub fn new() -> Self { /* ... */ }
    pub fn async(mut self) -> Self { /* ... */ }
    pub fn sync(mut self) -> Self { /* ... */ }
    pub fn stateless(mut self) -> Self { /* ... */ }
    pub fn stateful(mut self) -> Self { /* ... */ }
    pub fn name(mut self, name: impl Into<String>) -> Self { /* ... */ }
    pub fn registration_type(mut self, rt: RegistrationType) -> Self { /* ... */ }
    pub fn spawn<F>(self, handler: F) -> Result<Pid, SwErlError> { /* ... */ }
}
```

## Message Sending API

### Method-based (Decision 2)

```rust
pub trait SendMessage {
    fn send(&self, message: SwErlMessage) -> SwErlResponse;
}

impl SendMessage for Pid {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        // Implementation
    }
}

impl SendMessage for str {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        // Implementation
    }
}

impl SendMessage for SwErlAtom {
    fn send(&self, message: SwErlMessage) -> SwErlResponse {
        // Implementation
    }
}
```

## OTP Behaviors

### GenServer

```rust
pub trait GenServerBehavior: OTPActorBehavior {
    fn initialize_data(data: Option<&dyn Any>) -> Result<Box<dyn Any + Send + Sync>, SwErlError>;
    fn handle_call(&self, request: &dyn Any, data: &dyn Any) -> Result<(Box<dyn Any + Send + Sync>, Box<dyn Any + Send + Sync>), SwErlError>;
    fn handle_cast(&self, request: &dyn Any, data: &dyn Any) -> Result<Box<dyn Any + Send + Sync>, SwErlError>;
    fn terminate_cleanup(&self, reason: &str, data: Option<&dyn Any>);
}

pub enum GenServer {
    // Static methods for API
}

impl GenServer {
    pub fn start_link<T: GenServerBehavior>(name: &str, behavior: T, initial_state: Option<Box<dyn Any + Send + Sync>>) -> Result<String, SwErlError>;
    pub fn call(name: &str, message: SwErlMessage) -> Result<SwErlResponse, SwErlError>;
    pub fn cast(name: &str, message: SwErlMessage) -> Result<(), SwErlError>;
    pub fn unlink(name: &str, reason: &str) -> Result<(), SwErlError>;
}
```

### GenStateM

```rust
pub trait GenStatemBehavior: OTPActorBehavior {
    fn initialize(&self, initial_data: &dyn Any) -> Result<Box<dyn Any + Send + Sync>, SwErlError>;
    fn handle_call(&self, message: &dyn Any, current_state: &dyn Any) -> Result<(SwErlResponse, Box<dyn Any + Send + Sync>), SwErlError>;
    fn handle_cast(&self, message: &dyn Any, current_state: &dyn Any) -> Result<Box<dyn Any + Send + Sync>, SwErlError>;
    fn notify(&self, message: &dyn Any, state: &dyn Any);
    fn unlinked(&self, message: &dyn Any, current_state: &dyn Any);
}

pub enum GenStateM {
    // Static methods for API
}
```

### EventManager

```rust
pub enum EventManager {
    // Static methods for API
}

impl EventManager {
    pub fn link(name: &str, handlers: Vec<Box<dyn Fn(Pid, SwErlMessage) + Send + Sync>>) -> Result<Pid, SwErlError>;
    pub fn unlink(name: &str) -> Result<(), SwErlError>;
    pub fn notify(name: &str, message: SwErlMessage);
}
```

## Safe Data Structures

```rust
pub enum SafeDictCommand {
    Add,
    Remove,
    Get,
    GetKeys,
    GetValues,
    GetRaw,
}

pub fn build_safe<K, V>(dictionary: HashMap<K, V>, name: &str) -> Result<(), SwErlError>
where
    K: Send + Sync + 'static,
    V: Send + Sync + 'static,
{
    // Implementation using ProcessBuilder
}
```

## Dependencies

### Cargo.toml Dependencies

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
# Add other dependencies as needed

[dev-dependencies]
# Test dependencies
```

## Behavioral Equivalence Requirements

1. **API Surface:** Same public interface (Rust-style naming)
2. **Concurrency Semantics:** Preserved via Tokio
3. **Error Handling:** Result-based (Rust idiom)
4. **Process Lifecycle:** spawn → register → message handling → unlink
5. **Message Passing:** Same semantics (method-based API)
6. **State Management:** Embedded in process (Rust idiom)
7. **OTP Behaviors:** Same functionality (GenServer, GenStateM, EventManager)

## Implementation Notes

- All types must implement `Send + Sync` for thread safety
- Use `Arc<Mutex<T>>` for shared mutable state
- Use `Arc<RwLock<T>>` for concurrent read access
- Use `AtomicU64` for thread-safe counters
- Process IDs are `Copy` types (small integers)
- All closures must be `Send + Sync` for Tokio compatibility

## Testing Strategy

- Generate tests from behavioral equivalence requirements
- Test all four process types (async/sync × stateless/stateful)
- Test all OTP behaviors (GenServer, GenStateM, EventManager)
- Test safe data structures
- Test error conditions
- Test concurrency scenarios

