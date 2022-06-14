![LeanInkLogo](https://user-images.githubusercontent.com/24965150/145307859-30350f23-4f7f-4aab-a1ab-34889ad44d9a.png)

[![CI](https://github.com/leanprover/LeanInk/actions/workflows/build.yml/badge.svg)](https://github.com/insightmind/LeanInk/actions/workflows/build.yml)
[![LƎⱯN - 4](https://img.shields.io/static/v1?label=LƎⱯN&message=4&color=black)](https://github.com/leanprover/lean4)

LeanInk is a command line helper tool for [Alectryon](https://github.com/cpitclaudel/alectryon) which aims to ease the integration and support of [Lean 4](https://github.com/leanprover/lean4).
Alectryon uses the information provided by LeanInk to create a static code visualization for Lean 4 code.
For more information about Alectryon make sure to take a look at their repository.

> The official version of Alectryon does not yet support LeanInk, as LeanInk is still in active development. Please use our [Alectryon Fork](https://github.com/insightmind/alectryon/tree/lean4) to test LeanInk.

# Installation

You can either build LeanInk and install it yourself or use the build script to install a LeanInk release version:

```bash
sh -c "$(curl https://raw.githubusercontent.com/leanprover/LeanInk/main/init.sh -sSf)"
```

## Building from source

Before you can build LeanInk from source make sure to install the latest version of [Lean 4](https://github.com/leanprover/lean4) using `elan`.
This will also automatically install the [Lake](https://github.com/leanprover/lake) package manager.

```bash
git clone https://github.com/leanprover/LeanInk
cd LeanInk
lake build
```

# Usage

Analyzing a simple lean program `Input.lean` is very straightforward. To do so you simply use the `analyze` command (shorthand `a`) and provide LeanInk the input file.

```bash
leanInk analyze Input.lean
# OR
leanInk a Input.lean
```

The `analyze` command will generate an output file `Input.lean.leanInk` with the annotate lean program, encoded using Alectryons fragment json format. (For more information about the json format take a look at [Alectryon.lean](https://github.com/leanprover/LeanInk/blob/main/LeanInk/Annotation/Alectryon.lean))

---

If your lean program has external dependencies and uses Lake as its package manager you can use the `--lake` argument to provide the lakefile.

```bash
leanInk analyze Input.lean --lake lakefile.lean
```

LeanInk will then fetch any dependencies if necessary.

---

You can also analyze multiple files sequentially (concurrent analysis should be possible but is currently out of scope, feel free to contribute!):

```bash
leanInk analyze Input1.lean Input2.lean
```

This will create `Input1.leanink` and `Input2.leanink` respectively. However if you want to provide a lake should be valid for both input files, as you can only provide a single lake file.

---

To get the supported Lean 4 version of your instance of LeanInk you can do the following:

```bash
leanInk leanVersion
# OR
leanInk lV
```

## Usage in Alectryon

Alectryon automatically integrates LeanInk internally to analyze a Lean 4 code file or a documentation file with Lean 4 code blocks.
To embed Lean 4 in code blocks you have to use the `lean4::` directive for reStructuredText and `{lean4}` directive for myST markdown files. This is to distinguish the support of Lean 3 and Lean 4 in Alectryon.

For more information about Alectryon make sure to take a look at their repository.

# Development

## Experimental Features

### Additional Type Hover Metadata
The following flags are experimental and used to display additional information about a source text token in Alectryon. However this feature in Alectryon is still in active development and available here: [AlectryonFork:typeid](https://github.com/insightmind/alectryon/tree/typeid):

- `--x-enable-type-info` flag enables extraction of type information
- `--x-enable-docStrings` flag enables extraction of doc strings
- `--x-enable-semantic-token` flag enables extraction of semantic toke types for semantic syntax highlighting support

## Running Tests
There are some aspects you might want to take note of when attempting to develop a feature or fix a bug in LeanInk.

LeanInk uses simple diffing tests to make sure the core functionality works as expected. These tests are located in the `./test` folder.

You can run these tests using `make -C test run_tests`. This will run LeanInk for every `.lean`, that's not a `lakefile` or part of an `lean_package`. It will compare the output of LeanInk to the expected output within the `.lean.leanInk.expected` file.

To capture a new expected output file you can either run `make -C test capture` to capture the output for all files or use leanInk itself to generate an output for a single file and rename it afterwards.

# Contributing

LeanInk enforces the same [Contribution Guidelines](https://github.com/leanprover/lean4/blob/master/CONTRIBUTING.md) as Lean 4. Before contributing, make sure to read it.

We also highly encourage you to sign your commits.
