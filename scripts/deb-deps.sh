#!/bin/sh

grep-aptavail -F Ghc-Package . -s Ghc-Package -n \
    | sort \
    | perl -ne 'BEGIN {print "constraints:\n"} /^(.*)-([0-9\.]*)-(.*)$/; print "  $1 ==$2,\n"' \
    | sed '$s/,$//' \
    > cabal-deb.config
