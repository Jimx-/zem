#!/bin/sh

export GUILE_LOAD_PATH=`realpath ../modules`:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH=`realpath lib/`:$LD_LIBRARY_PATH

bin/zem
