# Swarm / SecureVote MVP

This repository contains three key items:

* The solidity contract for the ballot itself
* The web UI for voters
* The auditing software (for voters, or anyone, really)

## Running ballots

Ballots are stored on the Ethereum chain. Currently the only integrity check is the _name_ of the ballot options.

To run a ballot, you should do these things in roughly this order:

### Initial Setup 
 
* Set some environment variables in your build host or `.env` file. Particularly you'll need `MAIN_TITLE` for the title
  of the website. See [#web-ui](#web-ui) for more.
* Deploy a copy of this repository with `yarn build-web` (builds to `_dist`)
* Run `yarn sol-compile` to generate copies of the smart contract binaries.
* Generate admin tools locally: `yarn admin-prod` (builds into `_pureDist`)

### For each ballot

* Generate an encryption keypair with `node _pureDist/admin.js --genBallotKey`. Save the `SECRET KEY` privately somewhere,
  and copy the `PUBLIC KEY` for use in a moment.
* Run `yarn sol-deploy` to prepare a copy of the ballot smart contract for deployment, being sure to set correct start 
  and end times, and use the `PUBLIC KEY` you generated before. You'll need to release the `SECRET KEY` at the end of
  the ballot.

Example `yarn sol-deploy`: 

```
yarn sol-deploy --startTime 1518420000 --endTime 1518421966 \
  --ballotEncPubkey 0x3ae1a05881fe3bc1abcebde6898ed30716111c52fef9a7895dd82a2b8a595163 \
  --optionNamesJson '["Option 1", "Option 2", "Option 3 with some caveat and a long title", "the last option", "not really"]'
```

Note: `--optionNamesJson` needs to be formatted well. You should confirm the options before generating the final code.

* Copy the contract code generated and deploy it to your chosen Ethereum network in the usual way.
* Note down the address of the deployed contract.
* Edit `./web/src/SecureVote/SPAs/SwarmMVP/Ballot.elm` using the example as a template and fill in all relevant details,
  including the ERC20 address you'd like the ballot to be paired with. Be sure to add your new variable name to the list
  of `allBallots`. This is required for it to show up in the UI.
* Deploy a new version of the UI (after testing) with the details of the new ballot.
* Tell your token holders and wait for the ballot to complete.
* After the ballot is complete use the `revealSecKey` method of the ballot contract and publish the `SECRET KEY` you generated
  earlier.
* After the `SECRET KEY` has been published (and confirmed), go to the ballot page in the UI and the audit suite will kick
  in, download the ballots, and show you the results. 

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
* `DEMOC_HASH`: the Eth encoded hash of the relevant democracy (note: in future this might be handled in-app)

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

#### Webpack 4

Some troubles exist with webpack 4 and the loaders used by elm and purescript.

elm-webpack-loader is pinned to a commit for the mean time to get around this. It should be removed once the loader
itself is fixed.
