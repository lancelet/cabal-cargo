#!/usr/bin/env bash

readonly brittany=$(command -v brittany)
readonly cabalfmt=$(command -v cabal-fmt)

if [ -z "${brittany}" ]; then
    echo "Could not find brittany executable!"
fi
if [ -z "${cabalfmt}" ]; then
    echo "Could not find cabal-fmt executable!"
fi

echo "Using brittany:  ${brittany}"
echo "Using cabal-fmt: ${cabalfmt}"

find . -name '*.hs' -not -path '*/dist-newstyle/*' -exec \
    bash -xc "${brittany} --write-mode=inplace {}" \;
find . -name '*.cabal' -not -path '*/dist-newstyle/*' -exec \
    bash -xc "${cabalfmt} --inplace {}" \;
