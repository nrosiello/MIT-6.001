#!/bin/bash
rlwrap -r -c -f "data/mit_scheme_bindings.txt" scheme --load $1/*.scm
