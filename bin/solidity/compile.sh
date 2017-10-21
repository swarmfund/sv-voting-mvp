#!/usr/bin/env bash

# colors
NC='\033[0m' # No Color
RED='\033[0;31m'
GREEN='\033[0;32m'

if [ $(pwd | grep "bin/solidity") ]; then
    echo $'Please run this with Yarn from the source root.\n'
    echo $'Usage: yarn sol-compile\n'
    exit 1
fi

if [ !$(command -v solc) ]; then
    echo "${RED}Error:${NC} `solc` not found."
    echo ""
    echo "Please see: http://solidity.readthedocs.io/en/develop/installing-solidity.html"
    echo ""
    exit 1
fi

if [ ! -e "./contract/contracts/SwarmVotingMVP.sol" ]; then
    echo "${RED}Error:${NC} Cannot find SwarmVotingMVP.sol"
    echo "Expecting to find it in ./contract/contracts/"
    echo ""
    echo "Are you running this from `./sv-voting-mvp/`?"
    echo ""
    exit 1
fi

