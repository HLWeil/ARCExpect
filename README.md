# ArcExpect

ArcExpect helps authors write ARC validation packages in Python. A package is a
normal Python script with validation metadata, a list of checks, and a standard
result folder containing a JSON summary, JUnit report, and SVG badge.

## Write a validation package

Install the package and its runtime dependencies in your Python environment,
then write a script such as `example-validator.py`. The frontmatter at the top
is used when the package is indexed and is parsed by the package itself; keep
it as the first content in the file.

```python
"""
---
Name: example-python-validator
MajorVersion: 1
MinorVersion: 0
PatchVersion: 0
Summary: Checks the basic structure of an ARC.
Description: |
  A minimal native-Python ARC validation package.
Publish: true
Authors:
  - FullName: ARC validation team
    Email: validation@example.org
Tags:
  - Name: example
  - Name: python
ReleaseNotes: |
  - Initial version.
---
"""

from __future__ import annotations

import argparse
from pathlib import Path

from arcexpect import Execute, Expect, Setup, test_case, test_list


parser = argparse.ArgumentParser(description="Validate the basic structure of an ARC.")
parser.add_argument("--input", "-i", required=True, type=Path, help="Path to the ARC directory")
parser.add_argument("--output", "-o", required=True, type=Path, help="Directory for validation results")
args = parser.parse_args()


def input_directory_exists() -> None:
    Expect.is_true(args.input.is_dir(), "The input path must be an ARC directory")


def has_readme() -> None:
    Expect.is_true((args.input / "README.md").is_file(), "The ARC must contain README.md")


def has_investigation_or_study() -> None:
    has_investigation = (args.input / "investigation" / "isa.investigation.xlsx").is_file()
    has_study = (args.input / "studies").is_dir()
    Expect.is_true(has_investigation or has_study, "The ARC must contain an investigation or a study")


package = Setup.validation_package_from_script(
    __file__,
    critical=[
        test_list(
            "basic ARC structure",
            [
                test_case("input directory exists", input_directory_exists),
                test_case("README.md exists", has_readme),
                test_case("contains investigation or study", has_investigation_or_study),
            ],
        )
    ],
)

# Runs every check and writes validation_summary.json, validation_report.xml,
# and badge.svg below the output directory.
Execute.validation_pipeline(package, str(args.output))
```

Run the script with an ARC input directory and an output directory:

```shell
python example-validator.py --input ./my-arc --output ./results
```

The script creates the standard result layout:

```text
results/
└── .arc-validate-results/
    └── example-python-validator@1.0.0/
        ├── badge.svg
        ├── validation_report.xml
        └── validation_summary.json
```

The JSON summary records critical and non-critical totals and outcomes. The XML
file is a JUnit report suitable for CI systems, and the badge summarizes the
validation result. A failing non-critical check is recorded in the report but
does not make the badge a critical-error badge; failing or errored critical
checks do.

## Core API

- `Setup.validation_package_from_script(__file__, critical=[...],
  non_critical=[...])` parses the script's frontmatter and combines it with
  its checks. This is the usual entry point for a native Python package.
- `Setup.metadata(...)` and `Setup.validation_package(...)` remain available
  when metadata is supplied by another source.
- `test_case(name, check)` and `test_list(name, checks)` declare validation
  cases; use `pending_test_case` for a skipped case.
- `Expect.is_true`, `Expect.is_false`, `Expect.equal`, and related methods
  report a validation failure with a useful message.
- `Execute.validation_pipeline(package, output_path)` is the usual script
  entry point. `Execute.validation(package)` returns an in-memory summary when
  files are not needed.

See the legacy standalone Python packages in [references](references/) for
additional domain-specific examples.
