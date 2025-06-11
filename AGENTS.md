# Agent Development Guide

This repository follows the GNU Coding Standards and the commit conventions used
by the Guix project.  Agents working on this code base must comply with the
rules below.

## Commit Message Standards
- Use **GNU ChangeLog** style commit messages.
- Summaries should be short (max 72 chars) and start with an affected area
  followed by a colon, for example `packages: add foo`.
- Include a body listing affected files and describing changes.
- Sign commits with `git commit -S`.

## Branching and PR workflow
- Work on the `development` branch for new changes.
- Open pull requests targeting the `master` branch; do not use the `sync` script.
- Keep commits atomic and logically grouped.

## Pre-commit Checks
- Run `./bootstrap && ./configure` after dependency changes.
- Run `make` to build all Scheme modules.
- Run `make check` to execute the test-suite.
- Format code with `./pre-inst-env guix style -f FILE`.

## Pull Request Guidelines
- Title: short summary of the change.
- Description must include:
  - Rationale and overview of modifications.
  - Confirmation that `make check` succeeded.
  - Reference to relevant issues, if any.

## Versioning and Changelog
- Follow semantic versioning for releases.
- Update `NEWS` and `ChangeLog` when releasing a new version.

## Documentation
- Update `README` or `HACKING` when workflow or dependencies change.

