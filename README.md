# Swarm / SecureVote MVP

This repository contains three key items:

* The solidity contract for the ballot itself
* The web UI for voters
* The auditing software (for voters, or anyone, really)

## Developing

* Install Elm-Package: https://guide.elm-lang.org/install.html
* `yarn install` to install all JS dependencies.
* `elm-package install` to install Elm packages
* Install `solc`
* `npm install -g purescript pulp bower` and `bower install` to install purescript dependencies

### Output

* `yarn build-web` outputs to `_dist`
* `yarn sol-compile` outputs to `_solDist`
* `yarn admin-prod` and `yarn audit-prod` output to `_pureDist`

### Scripts

* `yarn sol-compile` (`./bin/solidity/compile.sh`) for compiling `SwarmVotingMVP.sol`; use `-c <Contract.sol>` to compile another contract.
* `yarn sol-deploy` (`node ./bin/solidity/deploy.js`) for deploying `SwarmVotingMVP.sol`; use `--deployOther <Contract>` (with dummy endTime, startTime, ballotEncPubkey arguments) - omit the `.sol`

## Web UI

Web code lives in `./web`

You'll need to set these environment variables:

* `MAIN_TITLE`: A string corresponding to the main title of the first page, e.g. "SWM Governance".
* `DEV`: a boolean to indicate if it's a dev environment or not.

If you're building locally you can put these in a `.env` file in the root of the repo.

The frontend is built in Elm using Material Design (`elm-mdl`).
Elm is a strongly typed purely functional language based on Haskell, and built with frontend in mind (and no other use cases currently).
It compiles to JS.
Elm uses The Elm Architecture (TEA) which is a Model, View, Update style framework.
It works similarly to React + Redux, though handles side effects very elegantly.
We're using it because it guarantees no runtime errors and its strictness helps write code that runs correctly the first time.

* `yarn web` to run Elm live-reloading dev
* `yarn build-web` to build SPA in `_dist`

### Elm-Format

`elm-format` is a program that automatically formats elm files according to a standard set of rules.
You should set it up for your editor and Elm code should be formatted with this tool before committing.

Docs: https://github.com/avh4/elm-format - instructions are listed here for most popular editors.

`npm install -g elm-format@exp` - currently using `elm-format-0.18 0.7.0-exp`

### Elm-Mdl

We use `elm-mdl` (material design light) for UI components.

* Docs: http://package.elm-lang.org/packages/debois/elm-mdl/8.1.0/Material
* Live example site + docs: http://debois.github.io/elm-mdl/

## Audit Suite

The Audit suite is built in Purescript.

To compile / develop it:

* `npm install -g purescript pulp bower`
* `yarn audit-prod` or `yarn admin-prod` (depending on which target you want to generate)

To run the audit script:

`node ./_pureDist/audit.js -e http://localhost:8545 --swmBallotAddr <address here> --erc20Addr <address>`

To run the admin script (key generation)

`node ./_pureDist/admin.js --genBallotKey`

## Solidity Contract

The contract lives in `./contract` and uses truffle for development, though not for deployment.

* `yarn sol-compile` to compile contract to `_solDist`
* `yarn sol-deploy <args>` to deploy (after compiling) to the blockchain

To play with a Web3-esq CLI run `yarn sol-compile && yarn sol-cli`

Example deployment:

(see `yarn sol-deploy --help` for all arguments)

```
yarn sol-deploy --startTime 1508824035 --endTime 1509667200 \
    --ballotEncPubkey 0xf13a7020b9d69380e8b91fc51acae296cf3368174edd6bfbd4edeac70bbca80f
```

### Notes:

#### Decimals

Currently `purescript-decimals` is causing a problem with imports and webpack (only for frontend).

This means `decimal.js` needs to be manually included with a `<script>` tag.

#### Build Flags

yarn 1.3.2 was giving us problems on netlify (though not on dev machines). If you have trouble try yarn 0.18.1.

additionally, if you run `INSTALL_WEBPACK=true yarn build-web` it run `npm install -g webpack` first.

#### Node Version

in addition to the yarn issue we also had issues with node v8.9.4. If you have trouble try v6.9.1.
