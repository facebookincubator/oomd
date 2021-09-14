# Release process

This document describes how to release a new oomd version.

## Branching model

Every change goes directly onto main. Releases will simply tag a commit
from main.

## Semantic versioning

We choose to follow semantic versioning. Note that this doesn't matter much for
major version < 1 but will matter a lot for >= 1.0.0 releases.

## Tagging a release

1. Make sure main builds and passes all tests.

1. Update the `version` field in `meson.build`. The format is
   `v<MAJOR>.<MINOR>.<PATCH>`.

1. Tag a release. We do this in the github UI by clicking "releases" (on same
   line as "commits"), then "Draft a new release". The tag should be the same
   as in `meson.build`. The title should be in `X.Y.Z` format. The tag
   description should include some high level notes and a link to the
   appropriate commit log. Please see previous releases for an example.
