![LeanInkLogo](https://user-images.githubusercontent.com/24965150/145307859-30350f23-4f7f-4aab-a1ab-34889ad44d9a.png)

[![CI](https://github.com/leanprover/LeanInk/actions/workflows/build.yml/badge.svg)](https://github.com/insightmind/LeanInk/actions/workflows/build.yml)
[![LƎⱯN - 4](https://img.shields.io/static/v1?label=LƎⱯN&message=4&color=black)](https://github.com/leanprover/lean4)

This branch of `LeanInk` is a bare-bones version meant for extracting tactic data from `mathlib4`. It was derived from the original `LeanInk` source by iteratively simplifying and customising the code for tactic data extraction.

LeanInk is a command line helper tool for [Alectryon](https://github.com/cpitclaudel/alectryon) which aims to ease the integration and support of [Lean 4](https://github.com/leanprover/lean4).
Alectryon uses the information provided by LeanInk to create a static code visualization for Lean 4 code.
For more information about Alectryon make sure to take a look at their repository.

> The official version of Alectryon does not yet support LeanInk, as LeanInk is still in active development. Please use our [Alectryon Fork](https://github.com/insightmind/alectryon/tree/lean4) to test LeanInk.

# Installation

You can either build LeanInk and install it yourself as shown below, or you can use the build script to install a LeanInk release version:

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

To install this built version it is recommended you simply add the `LeanInk/build/bin` folder to your PATH environment.

# Usage

Analyzing a simple lean program `Input.lean` is very straightforward. To do so you simply provide `LeanInk` the input file.

```bash
leanInk Input.lean
```

This creates a file `Input.lean.json` with the data of the tactic states.

---

You can also analyze multiple files sequentially (concurrent analysis should be possible but is currently out of scope, feel free to contribute!):

```bash
leanInk Input1.lean Input2.lean
```

This will create `Input1.lean.json` and `Input2.lean.json` respectively. However if you want to provide a lake should be valid for both input files, as you can only provide a single lake file.

## Usage in Alectryon

Alectryon automatically integrates LeanInk internally to analyze a Lean 4 code file or a documentation file with Lean 4 code blocks.
To embed Lean 4 in code blocks you have to use the `lean4::` directive for reStructuredText and `{lean4}` directive for myST markdown files. This is to distinguish the support of Lean 3 and Lean 4 in Alectryon.

For more information about Alectryon make sure to take a look at their repository.

# Development

## Experimental Features

## Running Tests
There are some aspects you might want to take note of when attempting to develop a feature or fix a bug in LeanInk.

LeanInk uses simple diffing tests to make sure the core functionality works as expected. These tests are located in the `./test` folder.

You can run these tests using `lake script run tests`. This will run LeanInk for every `.lean`, that's not a `lakefile` or part of an `lean_package`. It will compare the output of LeanInk to the expected output within the `.lean.leanInk.expected` file.

To capture a new expected output file you can either run `lake script run capture` to capture the output for all files or use leanInk itself to generate an output for a single file and rename it afterwards.  Be sure to carefully examine the git diff before committing the new expected baselines.

# Contributing

LeanInk enforces the same [Contribution Guidelines](https://github.com/leanprover/lean4/blob/master/CONTRIBUTING.md) as Lean 4. Before contributing, make sure to read it.

We also highly encourage you to sign your commits.
