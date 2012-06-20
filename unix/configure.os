#!/bin/sh
# Depending on the architecture, symlink in the correct tap_stubs file.

OS=`uname -s`

CFLAGS=${CFLAGS:--Wall -O3}
case `uname -m` in
x86_64)
  CFLAGS="${CFLAGS} -fPIC"
  ;;
esac
