#!/bin/bash -xe

COMPATIBILITY_SYMLINK=oomd
OUTDIR=build


# Required for compatiblity with internal Facebook header path conventions
rm -f $COMPATIBILITY_SYMLINK
ln -s . $COMPATIBILITY_SYMLINK

# Force usage of new compiler. This is a hack until we use a newer ubuntu
# image (with newer compiler defaults)
ln -f -s $(which g++-8) $(which g++)

meson $OUTDIR
ninja -C $OUTDIR test
