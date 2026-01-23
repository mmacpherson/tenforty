# Windows Build Research

This document captures the research and attempts to get Windows builds working for tenforty. As of January 2026, Windows builds are not supported due to runtime crashes that occur when the library is loaded or used.

## Background

The tenforty library wraps [Open Tax Solver (OTS)](https://opentaxsolver.sourceforge.net/) C code via Cython bindings. OTS documentation recommends using MinGW for compilation on Windows. However, Python on Windows is typically built with MSVC, and mixing MinGW-compiled C extensions with MSVC-built Python can cause interoperability issues. While builds work correctly on Linux and macOS, Windows builds consistently crash at runtime.

## Approaches Tried

### 1. MSVC with C++17 Inline Variables (Original Approach)

- **Configuration**: Default MSVC compiler with `inline` variable declarations
- **Result**: Runtime crashes due to MSVC's handling of inline variables across translation units
- **Error**: Heap corruption (`0xc0000374`) or access violation during `evaluate_form()` calls

### 2. MinGW-w64 with UCRT (20+ CI Attempts)

Multiple iterations trying MinGW as an alternative compiler:

- Set up MinGW via `egor-tensin/setup-mingw@v2`
- Created `pydistutils.cfg` with `compiler = mingw32`
- Set `USE_MINGW=1` environment variable
- Added `-DMS_WIN64` flag for 64-bit Windows compatibility
- Added static linking flags: `-static-libgcc -static-libstdc++`

**Result**: Builds succeed but identical runtime crashes with access violations occur.

### 3. Static Instead of Inline for Global Variables

- Modified `generate_otslib.py` to use `static` linkage for variables while keeping `inline` for functions
- Regenerated all OTS headers with the new approach

**Result**: Still crashes at runtime with MinGW. The issue is not inline variable handling.

### 4. Static MinGW Libraries

- Attempted `static: 1` option in the setup-mingw GitHub Action
- **Result**: Action fails - tries to delete files that don't exist in newer MinGW distributions

### 5. MSVC with Static Variables and LTCG Disabled

- Removed MinGW setup, reverted to MSVC
- Added `/LTCG:OFF` to disable Link-Time Code Generation
- Added `/bigobj` for large object files
- Fixed `use_mingw()` function to not auto-detect gcc in PATH

**Result**: MSVC Internal Compiler Error (C1001) during LTCG, then runtime crash after working around the ICE.

## Error Types Observed

| Error Code | Description | Context |
|------------|-------------|---------|
| `0xc0000374` | Heap corruption | Windows heap validator detecting memory overwrite |
| Access violation | Segmentation fault equivalent | During `evaluate_form` → OTS `main()` execution |
| C1001 | MSVC Internal Compiler Error | During Link-Time Code Generation |

## Possible Causes (Hypotheses)

These are theories that have not been definitively confirmed:

1. **MinGW/MSVC ABI incompatibility**: MinGW-compiled extensions may not be fully compatible with MSVC-built Python's runtime
2. **Memory safety issues in OTS code**: The OTS C code may contain buffer overflows (e.g., in `sprintf`/`strcpy` calls) that Windows detects but Unix-like systems tolerate
3. **MSVC-specific code generation**: The compiler may optimize or generate code differently in ways that expose latent bugs
4. **Memory layout differences**: Windows may have different struct alignment, padding, or heap management behavior
5. **OTS code assumptions**: The OTS code may assume Unix-like behavior in ways that don't translate to Windows

## Recommendations for Future Work

1. **Debug on Windows locally**: Use Visual Studio debugger or WinDbg to step through the crash and identify the exact buffer overflow location

2. **Address Sanitizer**: Try building with MSVC's Address Sanitizer (`/fsanitize=address`) to identify the memory corruption source

3. **Upstream fixes**: Once specific bugs are identified, contribute fixes to the OTS project

4. **Code audit**: Review OTS C code for common buffer overflow patterns:
   - `sprintf` without bounds checking (should use `snprintf`)
   - `strcpy` without length validation (should use `strncpy` or `strlcpy`)
   - Array access without bounds checking

## Current Status

Windows support is deferred until the root cause can be identified and resolved. The library builds and runs correctly on:

- Linux (x86_64)
- macOS (x86_64 and arm64)

## Related Issues

- GitHub Issue #19: Windows support tracking issue
