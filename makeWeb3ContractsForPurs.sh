#!/bin/bash

pulp build -m Generator --src-path pureWeb3Gen --to generator.js && \
mkdir -p tmpSolDist && \
cp -a ./_solDist/*.abi ./tmpSolDist/

for file in ./tmpSolDist/*.abi; do
    echo $file
    mv "$file" "./tmpSolDist/$(basename "$file" .abi).json"
done
ls ./tmpSolDist

node generator.js --abis tmpSolDist/ --dest pureSrc/ --module SecureVote.Contracts && \
rm generator.js && rm -r tmpSolDist
