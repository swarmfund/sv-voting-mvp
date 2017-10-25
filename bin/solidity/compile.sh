#!/usr/bin/env bash

# colors
NC='\033[0m' # No Color
RED='\033[0;31m'
GREEN='\033[0;32m'
LGREEN='\033[1;32m'
LBLUE='\033[1;34m'
LCYAN='\033[1;36m'

# getopts 
CONTRACT_NAME="SwarmVotingMVP.sol"
while getopts ":c:" opt; do 
    case $opt in
        c)
            CONTRACT_NAME="$OPTARG"
            ;;
        \?)
            echo "invalid option -$OPTARG"
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument"
            exit 1
            ;;
    esac
done

# params
CONTRACT_PATH="./contract/contracts/$CONTRACT_NAME"
OUTPUT_DIR="_solDist"

if [ $(pwd | grep "bin/solidity") ]; then
    echo -e 'Please run this with Yarn from the source root.\n'
    echo -e 'Usage: yarn sol-compile\n'
    exit 1
fi

if [ ! $(command -v solc) ]; then
    echo -e "${RED}Error:${NC} 'solc' not found.\n"=
    echo -e "Please see: http://solidity.readthedocs.io/en/develop/installing-solidity.html\n"
    exit 1
fi

if [ ! -e "$CONTRACT_PATH" ]; then
    echo -e "${RED}Error:${NC} Cannot find $CONTRACT_PATH\n"
    echo -e "Are you running this from `./sv-voting-mvp/` with yarn?\n"
    exit 1
fi

mkdir -p "./$OUTPUT_DIR" 

function solcCommon {
    solc "$@" -o "./$OUTPUT_DIR/" --overwrite --optimize --optimize-runs 500 "$CONTRACT_PATH"
}


echo -e "${LGREEN}>>> Starting solidity compilation of $CONTRACT_NAME <<<${NC}\n"

if solcCommon --bin --abi ; then
    echo -e "${LGREEN}Solidity compilation of $CONTRACT_NAME succeeded.${NC}"
else
    echo -e "${RED}ERROR: Solidity compilation of $CONTRACT_NAME failed${NC}"
    exit 1;
fi

SOLC_VERSION=$(solc --version | grep Version | cut -d ' ' -f 2)

C_NAME_NO_SOL=$(echo "$CONTRACT_NAME" | cut -d '.' -f 1)

echo -e "\n${LCYAN}>>> Smart Contract Verification Details <<<${NC}\n"
echo -e "Contract Name: ${LCYAN}$C_NAME_NO_SOL${NC}"
echo -e "Solc Version: ${LCYAN}$SOLC_VERSION${NC}"
echo -e "Optimization: ${LCYAN}Enabled (500 runs)${NC}"
echo -e "Contract code: ${LCYAN}$CONTRACT_PATH${NC}\n"
