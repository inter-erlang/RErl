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

# RuErl

RuErl is a Rust reengineering of SwErl, an Erlang/OTP-inspired concurrency library. It provides lightweight processes, message-passing concurrency, and OTP-style behaviors.

## Features

- **Lightweight Processes**: Spawn processes with minimal overhead
- **Message Passing**: Send messages between processes using the `send()` method
- **Thread-Safe Registry**: Local and global process registration
- **OTP Behaviors**: GenServer, GenStateM, and EventManager implementations
- **Safe Data Structures**: Thread-safe dictionary and other data structures
- **Async & Sync Support**: Both asynchronous and synchronous process behaviors via Tokio

## Installation

### Prerequisites: Installing Rust

Before using rerl, you need to install Rust and Cargo. The recommended way is through [rustup](https://rustup.rs/), the official Rust installer.

#### macOS

1. Open Terminal and run:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. Follow the on-screen instructions. The installer will configure your PATH automatically.

3. Restart your terminal or run:
   ```bash
   source $HOME/.cargo/env
   ```

4. Verify installation:
   ```bash
   rustc --version
   cargo --version
   ```

#### Linux

1. Open your terminal and run:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. Follow the on-screen instructions. You may need to install additional dependencies:
   ```bash
   # Ubuntu/Debian
   sudo apt-get install build-essential
   
   # Fedora
   sudo dnf install gcc
   
   # Arch Linux
   sudo pacman -S base-devel
   ```

3. Restart your terminal or run:
   ```bash
   source $HOME/.cargo/env
   ```

4. Verify installation:
   ```bash
   rustc --version
   cargo --version
   ```

#### Windows

1. Download and run [rustup-init.exe](https://rustup.rs/) from the official website.

2. Follow the installer prompts. You'll need:
   - Microsoft C++ Build Tools (installed automatically if needed)
   - Visual Studio 2013 or later (with C++ support)

3. Open a new Command Prompt or PowerShell window.

4. Verify installation:
   ```cmd
   rustc --version
   cargo --version
   ```

### Installing rerl

Add rerl to your `Cargo.toml`:

```toml
[dependencies]
rerl = "0.1.0"
tokio = { version = "1.0", features = ["full"] }
```

Or install from source:

```bash
git clone https://github.com/inter-erlang/RuErl.git
cd RuErl
```

## Quick Start

### Spawning Processes

```rust
use rerl::*;

#[tokio::main]
async fn main() -> Result<(), SwErlError> {
    // Spawn an asynchronous stateless process
    let pid = ProcessBuilder::new()
        .r#async()
        .stateless()
        .name("my_process")
        .spawn_async_stateless(|pid, message| {
            println!("Process {} received: {:?}", pid, message);
        })?;

    // Send a message to the process by name
    "my_process".send(Box::new("Hello, rerl!".to_string()))?;
    
    Ok(())
}
```

### Synchronous Processes

```rust
use rerl::*;

#[tokio::main]
async fn main() -> Result<(), SwErlError> {
    // Spawn a synchronous stateless process
    let pid = ProcessBuilder::new()
        .sync()
        .stateless()
        .spawn_sync_stateless(|_pid, message| {
            // Return a response
            Ok(Some(message))
        })?;

    // Send a message and wait for response
    let response = pid.send(Box::new("test".to_string()))?;
    println!("Response: {:?}", response);
    
    Ok(())
}
```

### Stateful Processes

```rust
use rerl::*;

#[tokio::main]
async fn main() -> Result<(), SwErlError> {
    // Spawn a synchronous stateful process
    let pid = ProcessBuilder::new()
        .sync()
        .stateful()
        .name("counter")
        .initial_state(Box::new(0i32))
        .spawn_sync_stateful(|_pid, message, state| {
            let mut state_guard = state.lock().unwrap();
            if let Some(count) = state_guard.downcast_mut::<i32>() {
                *count += 1;
                let response: Box<dyn std::any::Any + Send + Sync> = Box::new(*count);
                let new_state: SwErlState = Box::new(*count);
                (Ok(Some(response)), new_state)
            } else {
                (Err(SwErlError::InvalidState), Box::new(0i32))
            }
        })?;

    // Send messages to increment counter
    let response = "counter".send(Box::new("increment".to_string()))?;
    println!("Counter: {:?}", response);
    
    Ok(())
}
```

### GenServer

```rust
use rerl::*;

struct MyGenServer;

impl GenServerBehavior for MyGenServer {
    fn initialize_data(&self, data: Option<&dyn std::any::Any>) -> Result<SwErlState, SwErlError> {
        Ok(data.map(|d| d.clone()).unwrap_or_else(|| Box::new(0i32)))
    }

    fn terminate_cleanup(&self, _reason: &str, _data: Option<&dyn std::any::Any>) {
        // Cleanup logic
    }

    fn handle_cast(&self, _request: &dyn std::any::Any, data: &dyn std::any::Any) -> Result<SwErlState, SwErlError> {
        // Handle async message
        Ok(data.clone())
    }

    fn handle_call(&self, request: &dyn std::any::Any, data: &dyn std::any::Any) -> Result<(Box<dyn std::any::Any + Send + Sync>, SwErlState), SwErlError> {
        // Handle sync message and return response + new state
        let response: Box<dyn std::any::Any + Send + Sync> = Box::new("response");
        Ok((response, data.clone()))
    }
}

#[tokio::main]
async fn main() -> Result<(), SwErlError> {
    let server = MyGenServer;
    GenServer::start_link("my_server", server, Some(Box::new(0i32)))?;
    
    // Send async message
    GenServer::cast("my_server", Box::new("message".to_string()))?;
    
    // Send sync message and wait for response
    let response = GenServer::call("my_server", Box::new("request".to_string()))?;
    
    Ok(())
}
```

## Building and Testing

### Compiling the Library

#### macOS and Linux

```bash
cd RErl
cargo build
```

For release builds (optimized):

```bash
cargo build --release
```

#### Windows

```cmd
cd RErl
cargo build
```

For release builds (optimized):

```cmd
cargo build --release
```

### Running Tests

#### macOS and Linux

Run all tests:

```bash
cargo test
```

Run tests with output:

```bash
cargo test -- --nocapture
```

Run specific test suites:

```bash
# Core tests
cargo test --test rerl_tests

# GenServer tests
cargo test --test rerl_gen_server_tests

# GenStateM tests
cargo test --test rerl_gen_statem_tests

# EventManager tests
cargo test --test rerl_event_manager_tests
```

Run documentation tests:

```bash
cargo test --doc
```

#### Windows

Run all tests:

```cmd
cargo test
```

Run tests with output:

```cmd
cargo test -- --nocapture
```

Run specific test suites:

```cmd
REM Core tests
cargo test --test rerl_tests

REM GenServer tests
cargo test --test rerl_gen_server_tests

REM GenStateM tests
cargo test --test rerl_gen_statem_tests

REM EventManager tests
cargo test --test rerl_event_manager_tests
```

### Running Examples

Examples are located in the `examples/` directory. Run them with:

```bash
# macOS/Linux
cargo run --example <example_name>

# Windows
cargo run --example <example_name>
```

### Troubleshooting

#### Common Issues

**Issue: "linker 'cc' not found" (Linux)**
- Solution: Install build-essential: `sudo apt-get install build-essential`

**Issue: "could not compile 'proc-macro2'"**
- Solution: Update Rust: `rustup update`

**Issue: "error: failed to run custom build command for 'ring'"**
- Solution: Ensure you have the latest Rust toolchain: `rustup update stable`

**Issue: Tests fail with "spawn" errors**
- Solution: Ensure you're running on a supported platform with proper async runtime support

## Architecture

RuErl is designed to preserve SwErl's behavioral equivalence while using Rust idioms:

- **Concurrency**: Tokio runtime for async/await
- **Message Sending**: `send()` method instead of `!` operator
- **Error Handling**: `Result<T, E>` instead of tuples
- **State Storage**: Embedded in process struct
- **Process Spawning**: Builder pattern for flexible configuration

## API Reference

See the [API documentation](https://docs.rs/rerl) for complete API reference.

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please see CONTRIBUTING.md for guidelines.

## Acknowledgments

Development of this tool was enhanced by using a custom [AALang and gab](https://aalang.org) tool.

## Related Projects

- [SwErl](https://github.com/inter-erlang/SwErl) - The original Swift implementation
- [Erlang/OTP](https://www.erlang.org/) - The inspiration for this library
