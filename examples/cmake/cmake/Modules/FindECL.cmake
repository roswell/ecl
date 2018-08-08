#=============================================================================
# Copyright (C) 2018 marcinkolenda419@gmail.com
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
# AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
# OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# FindECL
# --------
#
# Find ECL library and binary executables.
#
# Find the Embedded Common Lisp library headers and libraries.
#
# This module provides the following variables:
#
#   ECL_INCLUDE_DIRS    - where to find ecl/ecl.h, etc.
#   ECL_LIBRARIES       - List of libraries to link when using ecl.
#   ECL_FOUND           - True if ecl found.
#   ECL_VERSION_STRING  - the version of ecl found (since CMake 2.8.8)
#
#   ECL_BIN_DIR         - directory that contains `ecl` and `ecl-config` executables
#   ECL_BIN_PATH        - path to `ecl` binary executable
#   ECL_CONFIG_BIN_PATH - path to `ecl-config` binary executable
#   ECL_VERSION         - full version string e.g. "16.1.3"
#   ECL_VERSION_MAJOR   - major version string e.g. "16"
#   ECL_VERSION_MINOR   - minor version string e.g. "1"
#   ECL_VERSION_PATCH   - patch version string e.g. "3"


find_path(ECL_INCLUDE_DIR NAMES ecl/ecl.h)
mark_as_advanced(ECL_INCLUDE_DIR)

find_library(ECL_LIBRARY NAMES
    ecl
    # TODO check how it's on windows
)
mark_as_advanced(ECL_LIBRARY)

# Find the ECL_VERSION_STRING
if(ECL_INCLUDE_DIR)
  if(EXISTS "${ECL_INCLUDE_DIR}/ecl/config.h")
    file(STRINGS "${ECL_INCLUDE_DIR}/ecl/config.h" ecl_version_str
      REGEX "^#define[\t ]+ECL_VERSION_NUMBER[\t ]+[0-9]*.*")

    string(REGEX REPLACE "^#define[\t ]+ECL_VERSION_NUMBER[\t ]+([0-9]*).*"
      "\\1" ECL_VERSION_STR_RAW ${ecl_version_str})

    # at this moment ${ECL_VERSION_STR_RAW} contain version number in format
    # 160103 for version 16.1.3. Let's convert it to period separated format.
    set (version_helper_regex "^([0-9][0-9])([0-9][0-9])([0-9][0-9])$")
    string(REGEX REPLACE ${version_helper_regex} "\\1" major_raw ${ECL_VERSION_STR_RAW})
    string(REGEX REPLACE ${version_helper_regex} "\\2" minor_raw ${ECL_VERSION_STR_RAW})
    string(REGEX REPLACE ${version_helper_regex} "\\3" patch_raw ${ECL_VERSION_STR_RAW})

    string(REGEX REPLACE "^[0]*([0-9]+)" "\\1" ECL_VERSION_MAJOR ${major_raw})
    string(REGEX REPLACE "^[0]*([0-9]+)" "\\1" ECL_VERSION_MINOR ${minor_raw})
    string(REGEX REPLACE "^[0]*([0-9]+)" "\\1" ECL_VERSION_PATCH ${patch_raw})
    # version format conversion is done

    set(ECL_VERSION "${ECL_VERSION_MAJOR}.${ECL_VERSION_MINOR}.${ECL_VERSION_PATCH}")

    unset(ecl_version_str)
    unset(ECL_VERSION_STR_RAW)
    unset(version_helper_regex)
    unset(major_raw)
    unset(minor_raw)
    unset(patch_raw)
  endif()
endif()

find_path(ECL_BIN_DIR bin/ecl)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(ECL
                                  REQUIRED_VARS ECL_LIBRARY ECL_INCLUDE_DIR
                                  VERSION_VAR ECL_VERSION_STRING)

if(ECL_FOUND)
  set(ECL_LIBRARIES ${ECL_LIBRARY})
  if(ECL_BIN_DIR)
    set(ECL_BIN_DIR "${ECL_BIN_DIR}/bin")
    set(ECL_BIN_PATH "${ECL_BIN_DIR}/ecl")
    set(ECL_CONFIG_BIN_PATH "${ECL_BIN_DIR}/ecl-config")
  endif()
endif()

