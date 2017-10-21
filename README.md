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
* `yarn build-web` to build SPA in `_dist` (TODO)

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

`npm install -g purescript pulp bower`


