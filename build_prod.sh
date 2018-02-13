#!/usr/bin/env bash

if [ $INSTALL_WEBPACK ]; then
  echo "##> Installing webpack..."
  npm install -g webpack
  echo "##> Installed webpack."
fi

# get cached elm stuff
mkdir -p .cache
cp -a .cache/elm-stuff elm-stuff || true

# do build
webpack "$@"

# save in cache
rm -rf .cache/elm-stuff || true
cp -a elm-stuff .cache/elm-stuff
