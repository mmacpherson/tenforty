# Error Handling

`tenforty` provides configurable error handling for dealing with OTS processing
issues. This document covers the `on_error` parameter and the exception types
available.

## The `on_error` Parameter

Both `evaluate_return` and `evaluate_returns` accept an `on_error` parameter
that controls how errors from the underlying OTS engine are handled.

### Available Modes

| Mode | Behavior |
|------|----------|
| `"raise"` | Raise an `OTSError` exception (default) |
| `"warn"` | Emit a `RuntimeWarning` and continue |
| `"ignore"` | Silently continue |

### Examples

```python
from tenforty import evaluate_return

# Default: raise exceptions on OTS errors
result = evaluate_return(w2_income=50000)

# Emit warnings instead of raising
result = evaluate_return(w2_income=50000, on_error="warn")

# Silently ignore OTS errors
result = evaluate_return(w2_income=50000, on_error="ignore")
```

### When to Use Each Mode

- **`"raise"` (default)**: Use for interactive exploration and debugging. You'll
  immediately see when something goes wrong.

- **`"warn"`**: Use when processing many returns in batch where you want to know
  about issues but don't want to stop processing.

- **`"ignore"`**: Use when you've already validated inputs and want maximum
  performance, or when you're intentionally testing edge cases.

## Exception Types

### `OTSError`

Raised when the OTS engine returns a non-zero exit code. This typically
indicates a problem with the input data that OTS couldn't process.

```python
from tenforty import OTSError

try:
    result = evaluate_return(w2_income=50000)
except OTSError as e:
    print(f"Exit code: {e.exit_code}")
    print(f"Year: {e.year}")
    print(f"Form: {e.form}")
```

Attributes:
- `exit_code`: The non-zero exit code returned by OTS
- `year`: The tax year being processed
- `form`: The OTS form ID (e.g., `"US_1040"`)

### `OTSParseError`

Raised when OTS output cannot be parsed. This usually indicates an unexpected
output format from OTS.

```python
from tenforty import OTSParseError

try:
    result = evaluate_return(w2_income=50000)
except OTSParseError as e:
    print(f"Message: {e}")
    if e.raw_output:
        print(f"Raw output: {e.raw_output}")
```

Attributes:
- `raw_output`: The raw OTS output that couldn't be parsed (if available)

The error message is accessible via `str(e)` (inherited from `Exception`).

### `OTSErrorPolicy` Enum

For type-safe code, you can use the `OTSErrorPolicy` enum instead of string
literals:

```python
from tenforty import evaluate_return, OTSErrorPolicy

result = evaluate_return(w2_income=50000, on_error=OTSErrorPolicy.WARN)
```

Values: `OTSErrorPolicy.RAISE`, `OTSErrorPolicy.WARN`, `OTSErrorPolicy.IGNORE`

## Handling Invalid Inputs

Invalid year/form combinations raise `ValueError` before OTS is invoked:

```python
from tenforty import otslib

# Raises ValueError: Unknown year/form combination: 1999/US_1040
otslib._evaluate_form(1999, "US_1040", "Title: Test")
```

Input validation errors (invalid state, filing status, etc.) are handled by
pydantic and raise `ValidationError`:

```python
from tenforty import evaluate_return

# Raises ValidationError with helpful message about valid states
evaluate_return(state="ZZ", w2_income=50000)
```
