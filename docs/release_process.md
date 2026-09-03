# Release process

This document describes how to release a new oomd version.

## Branching model

Every change goes directly onto main. Releases will simply tag a commit
from main.

## Semantic versioning

oomd uses semantic versioning. Before version 1.0.0, the public API can change
between minor versions. Starting with version 1.0.0, incompatible public API
changes require a major version change.

## Tagging a release

1. Make sure main builds and passes all tests.

1. In fbsource, run
   `buck test fbcode//oomd:oss_source_manifest_test -- --timeout=300`. This
   test checks the public source manifest. It also rejects non-public include
   roots and selected internal markers in exported C and C++ files.

1. Update the `version` field in the internal `public_tld/meson.build` file.
   The exported path is `meson.build`. Use the format
   `v<MAJOR>.<MINOR>.<PATCH>`.

1. Land the version change and wait for it to sync to GitHub.

1. Create a GitHub release for the synced commit. The tag must match the
   `meson.build` version. Use `X.Y.Z` for the release title. Add high-level
   release notes and a link to the applicable commit log.
