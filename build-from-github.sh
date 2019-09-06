#!/bin/bash -xe

OUTDIR=build

# Force usage of new compiler. This is a hack until we use a newer ubuntu
# image (with newer compiler defaults)
ln -f -s $(which g++-8) $(which g++)

meson $OUTDIR
ninja -C $OUTDIR test
