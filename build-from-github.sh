#!/bin/bash -xe

COMPATIBILITY_SYMLINK=oomd
OUTDIR=build


# Required for compatiblity with internal Facebook header path conventions
rm -f $COMPATIBILITY_SYMLINK
ln -s . $COMPATIBILITY_SYMLINK


mkdir $OUTDIR
meson $OUTDIR
ninja -C $OUTDIR test
