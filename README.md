# Swarm / SecureVote MVP

This repository contains three key items:

* The solidity contract for the ballot itself
* The web UI for voters
* The auditing software (for voters, or anyone, really)

## Developing

* Install Elm-Package: https://guide.elm-lang.org/install.html
* `yarn install` to install all JS dependencies.
* `elm-package install` to install Elm packages

## Solidity Contract

The contract lives in `./contract` and uses truffle for development.

* `yarn solc` to compile contract via truffle to `./contract/build`

## Web UI

Web code lives in `./web`

The frontend is built in Elm using Material Design (`elm-mdl`).
Elm is a strongly typed purely functional language based on Haskell, and built with frontend in mind (and no other use cases currently).
It compiles to JS.
Elm uses The Elm Architecture (TEA) which is a Model, View, Update style framework.
It works similarly to React + Redux, though handles side effects very elegantly.
We're using it because it guarantees no runtime errors and its strictness helps write code that runs correctly the first time.

* `yarn web` to run Elm live-reloading dev
* `yarn build-web` to build SPA in `_dist`

## Audit Suite

(TODO)
