"""Build configuration for tenforty with platform-specific compiler flags.

This setup.py handles platform-specific build configuration, particularly
for compiling the OTS C++ sources with proper flags for each platform.
"""

import os
import pathlib
import platform

from Cython.Build import cythonize
from setuptools import Extension, setup

# Get absolute path to the otslib directory for header includes
OTSLIB_DIR = str(pathlib.Path(__file__).parent / "src" / "tenforty" / "otslib")


def use_mingw() -> bool:
    """Check if we should use MinGW on Windows.

    Returns True if USE_MINGW env var is set, if pydistutils.cfg exists
    with compiler=mingw32, or if gcc is available in PATH.
    """
    import shutil

    # Debug output
    print(f"DEBUG: USE_MINGW env var = {os.environ.get('USE_MINGW')!r}")
    pydistutils_cfg = pathlib.Path.home() / "pydistutils.cfg"
    print(f"DEBUG: pydistutils.cfg path = {pydistutils_cfg}")
    print(f"DEBUG: pydistutils.cfg exists = {pydistutils_cfg.exists()}")
    print(f"DEBUG: gcc in PATH = {shutil.which('gcc')}")

    if os.environ.get("USE_MINGW"):
        print("DEBUG: Using MinGW (env var)")
        return True
    # Check for pydistutils.cfg that sets mingw32 compiler
    if pydistutils_cfg.exists():
        content = pydistutils_cfg.read_text()
        if "mingw32" in content:
            print("DEBUG: Using MinGW (pydistutils.cfg)")
            return True
    # Fallback: check if gcc is in PATH
    if shutil.which("gcc"):
        print("DEBUG: Using MinGW (gcc in PATH)")
        return True
    print("DEBUG: Not using MinGW")
    return False


extra_compile_args = []
extra_link_args = []

if platform.system() == "Windows":
    # Check if using MinGW or MSVC
    mingw = use_mingw()
    print(f"Windows build: use_mingw()={mingw}")  # Debug output

    if mingw:
        # MinGW/GCC - matches OTS recommended toolchain for Windows.
        # OTS was designed for GCC and has runtime issues with MSVC.
        # -DMS_WIN64: Tell Python headers we're on 64-bit Windows (required for MinGW-64)
        # See https://github.com/cython/cython/issues/3405
        extra_compile_args = ["-DMS_WIN64", "-O2", "-std=c++17"]
        # Static link GCC runtime to avoid DLL dependencies and potential
        # ABI conflicts with MSVC-built Python.
        extra_link_args = ["-static-libgcc", "-static-libstdc++"]
    else:
        # MSVC fallback - may have runtime issues due to inline variable handling
        # Define SIZEOF_VOID_P for 64-bit builds (we skip 32-bit in CI)
        extra_compile_args = ["/Od", "/EHsc", "/GL-", "/std:c++17", "/DSIZEOF_VOID_P=8"]
elif platform.system() == "Darwin":
    extra_compile_args = ["-O2", "-std=c++17"]
else:
    extra_compile_args = ["-O2", "-std=c++17"]

extensions = [
    Extension(
        "tenforty.otslib",
        sources=["src/tenforty/otslib/ots.pyx"],
        include_dirs=[OTSLIB_DIR],
        language="c++",
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
    ),
]

setup(
    ext_modules=cythonize(extensions, language_level="3"),
)
