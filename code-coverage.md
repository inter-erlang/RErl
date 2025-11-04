# Code Coverage Analysis

**Date:** November 4, 2025  
**Tool:** cargo-tarpaulin v0.34.1  
**Overall Coverage:** 38.41% (116/302 lines covered)

## Summary

This code coverage analysis was performed using `cargo-tarpaulin`, a code coverage tool for Rust projects. The analysis covers all source files in the `src/` directory and tests the execution paths through all test suites.

### Overall Metrics

- **Total Lines:** 302
- **Covered Lines:** 116
- **Uncovered Lines:** 186
- **Coverage Percentage:** 38.41%

## File-by-File Coverage

### Core Modules

| File | Covered | Total | Coverage % | Status |
|------|---------|-------|------------|--------|
| `src/builder.rs` | 39 | 82 | 47.56% | ⚠️ Needs improvement |
| `src/errors.rs` | 0 | 18 | 0.00% | ❌ Critical - No coverage |
| `src/message.rs` | 10 | 18 | 55.56% | ⚠️ Needs improvement |
| `src/pid.rs` | 10 | 16 | 62.50% | ✅ Good |
| `src/process.rs` | 9 | 16 | 56.25% | ⚠️ Needs improvement |
| `src/registrar.rs` | 33 | 57 | 57.89% | ⚠️ Needs improvement |
| `src/safe_data.rs` | 0 | 18 | 0.00% | ❌ Critical - No coverage |
| `src/types.rs` | N/A | N/A | N/A | Type definitions only |

### OTP Behavior Modules

| File | Covered | Total | Coverage % | Status |
|------|---------|-------|------------|--------|
| `src/otp/base.rs` | N/A | N/A | N/A | Trait definitions only |
| `src/otp/event_manager.rs` | 4 | 15 | 26.67% | ❌ Critical - Low coverage |
| `src/otp/gen_server.rs` | 6 | 35 | 17.14% | ❌ Critical - Low coverage |
| `src/otp/gen_statem.rs` | 5 | 27 | 18.52% | ❌ Critical - Low coverage |
| `src/otp/mod.rs` | N/A | N/A | N/A | Module re-exports only |

## Detailed Uncovered Lines

### `src/builder.rs` (43 uncovered lines)
- Lines 52-54: Error handling paths
- Lines 64-66: Error handling paths
- Lines 70-72: Error handling paths
- Line 82: Error handling
- Line 106: Error handling
- Lines 124, 128-130: Stateful async spawn variants
- Lines 133-141: Stateful async spawn variants
- Lines 144-145: Stateful async spawn variants
- Line 149: Stateful async spawn variants
- Lines 153-155: Stateful async spawn variants
- Lines 158-166: Stateful async spawn variants
- Lines 169-170: Stateful async spawn variants
- Lines 178-179: Stateful async spawn variants

### `src/errors.rs` (18 uncovered lines - 100% uncovered)
- Lines 43-60: Error display implementations (`Display` trait)
- **Note:** Error types are typically not directly tested, but error paths should be exercised

### `src/message.rs` (8 uncovered lines)
- Line 23: Error handling path
- Line 34: Error handling path
- Lines 40-41: Error handling paths
- Lines 46-48: Error handling paths
- Line 50: Error handling path

### `src/otp/event_manager.rs` (11 uncovered lines)
- Lines 33-34: Error handling
- Lines 36-39: Error handling paths
- Line 42: Error handling
- Line 46: Error handling
- Line 48: Error handling
- Line 52: Error handling
- Line 54: Error handling

### `src/otp/gen_server.rs` (29 uncovered lines)
- Lines 51-53: Error handling paths
- Lines 57-59: Error handling paths
- Lines 63-64: Error handling paths
- Lines 66-67: Error handling paths
- Lines 70-71: Error handling paths
- Line 74: Error handling
- Lines 78-79: Error handling paths
- Line 81: Error handling
- Lines 83-84: Error handling paths
- Line 86: Error handling
- Lines 90-92: Error handling paths
- Lines 96-98: Error handling paths
- Lines 102-103: Error handling paths
- Lines 107-108: Error handling paths

### `src/otp/gen_statem.rs` (22 uncovered lines)
- Line 53: Error handling
- Line 59: Error handling
- Lines 63-64: Error handling paths
- Lines 66-68: Error handling paths
- Lines 70-71: Error handling paths
- Line 75: Error handling
- Line 79: Error handling
- Line 81: Error handling
- Line 85: Error handling
- Line 87: Error handling
- Line 91: Error handling
- Line 93: Error handling
- Line 97: Error handling
- Line 99: Error handling
- Lines 103-104: Error handling paths
- Lines 108-109: Error handling paths

### `src/pid.rs` (6 uncovered lines)
- Lines 26-27: Error handling or edge cases
- Lines 77-78: Error handling or edge cases
- Lines 89-90: Error handling or edge cases

### `src/process.rs` (7 uncovered lines)
- Lines 64-65: Error handling paths
- Lines 69-70: Error handling paths
- Line 79: Error handling
- Lines 84-85: Error handling paths

### `src/registrar.rs` (24 uncovered lines)
- Lines 52-53: Error handling paths
- Line 86: Error handling
- Line 92: Error handling
- Lines 102-104: Error handling paths
- Lines 106-108: Error handling paths
- Lines 114-116: Error handling paths
- Lines 118-119: Error handling paths
- Lines 136-138: Error handling paths
- Lines 142-144: Error handling paths
- Lines 154-156: Error handling paths

### `src/safe_data.rs` (18 uncovered lines - 100% uncovered)
- Line 26: Function implementation
- Line 31: Function implementation
- Line 33: Function implementation
- Lines 36-39: Function implementations
- Line 42: Function implementation
- Lines 44-45: Function implementations
- Lines 50-52: Function implementations
- Lines 54-57: Function implementations
- Line 62: Function implementation
- **Note:** This module appears to have no test coverage at all

## Critical Areas Requiring Attention

### Priority 1: Zero Coverage Modules
1. **`src/errors.rs`** (0% coverage)
   - Error types and their `Display` implementations need test coverage
   - Error propagation paths should be tested

2. **`src/safe_data.rs`** (0% coverage)
   - Thread-safe data structures are completely untested
   - This is a critical component for concurrent safety

### Priority 2: Low Coverage OTP Behaviors
1. **`src/otp/gen_server.rs`** (17.14% coverage)
   - Most error handling paths are untested
   - Critical client-server pattern needs comprehensive testing

2. **`src/otp/gen_statem.rs`** (18.52% coverage)
   - State machine transitions and error paths need testing
   - Critical for state management correctness

3. **`src/otp/event_manager.rs`** (26.67% coverage)
   - Event notification and error handling need more tests

### Priority 3: Medium Coverage Modules
1. **`src/builder.rs`** (47.56% coverage)
   - Missing coverage for stateful async spawn variants
   - Error handling paths need testing

2. **`src/message.rs`** (55.56% coverage)
   - Error handling paths for message sending need coverage

3. **`src/process.rs`** (56.25% coverage)
   - Error handling paths need more coverage

4. **`src/registrar.rs`** (57.89% coverage)
   - Error handling paths for registration need testing

## Recommendations

### Immediate Actions
1. **Add tests for `src/safe_data.rs`**
   - Test thread-safe dictionary operations
   - Test concurrent access patterns
   - Test error cases

2. **Add tests for `src/errors.rs`**
   - Test error creation and display
   - Test error propagation through call chains

3. **Expand OTP behavior tests**
   - Test error handling in GenServer, GenStateM, and EventManager
   - Test edge cases and error recovery

4. **Add stateful async spawn tests**
   - Test all builder variants for stateful processes
   - Test error cases in process spawning

### Testing Strategy
1. **Unit Tests**: Focus on individual function coverage
2. **Integration Tests**: Test error propagation across modules
3. **Error Path Testing**: Systematically test all error handling branches
4. **Concurrency Tests**: Test thread-safe operations under load

### Target Coverage Goals
- **Overall Target:** 80% (as specified in `testing-target.jsonld`)
- **Critical Paths:** 100% (as specified in `testing-target.jsonld`)
  - Process spawning
  - Message sending
  - Error handling
  - Process registration
  - OTP behavior initialization
  - State management
  - Process lifecycle

## Test Execution Summary

The following test suites were executed during coverage analysis:

- ✅ `rerl_tests` (6 tests passed)
- ✅ `rerl_gen_server_tests` (3 tests passed)
- ✅ `rerl_gen_statem_tests` (3 tests passed)
- ✅ `rerl_event_manager_tests` (3 tests passed)
- ✅ Documentation tests (1 test passed)

**Total:** 16 tests passed, 0 failed

## How to Request Another Coverage Analysis

To request another code coverage analysis, simply ask:

- "do a code coverage Analysis" or
- "run code coverage again" or
- "generate a new coverage report"

The analysis will:
1. Install `cargo-tarpaulin` if not already installed
2. Run all tests with coverage instrumentation
3. Generate a detailed report showing:
   - Overall coverage percentage
   - File-by-file breakdown
   - Uncovered line numbers
   - Recommendations for improvement

You can also run the analysis manually using:

```bash
cd rerl
cargo install cargo-tarpaulin --locked  # First time only
cargo tarpaulin --out stdout --timeout 120
```

For more detailed output, including line-by-line coverage:

```bash
cargo tarpaulin --out stdout --timeout 120 --verbose
```

To generate an HTML report:

```bash
cargo tarpaulin --out Html --output-dir ./coverage
```

## Notes

- Coverage percentages are calculated based on line coverage, not branch coverage
- Some modules (like `types.rs`, `base.rs`, `mod.rs`) contain only type definitions and trait declarations, which don't require runtime coverage
- Error handling paths are a significant portion of uncovered code
- The current coverage (38.41%) is below the target of 80%, indicating significant work is needed to improve test coverage

---

*Last updated: November 4, 2025*

