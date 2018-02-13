#!/usr/bin/env bash

CACHE_DIR="node_modules"

if [ $INSTALL_WEBPACK ]; then
  echo "##> Installing webpack..."
  npm install -g webpack
  echo "##> Installed webpack."
fi

# prep for netlify cache stuff
echo "Restoring .cache at $CACHE_DIR/.cache: \n`ls $CACHE_DIR/.cache`\n"
cp -a $CACHE_DIR/.cache .cache || true

# get cached elm stuff
mkdir -p .cache
cp -a .cache/elm-stuff elm-stuff || true

# print cache directory
echo "Cache Directory before build:\n"
ls -al .cache
echo ""

# do build
echo "Building now..."
webpack "$@"
echo "Build Complete"

# save in cache
echo "Caching elm stuff"
rm -rf .cache/elm-stuff || true
cp -a elm-stuff .cache/elm-stuff

# print cache directory
echo "Cache Directory after build: (ls -al .cache)\n"
ls -al .cache
echo ""

# save netlify cache
cp -a .cache $CACHE_DIR/.cache
echo "Saved .cache to $CACHE_DIR/.cache: \n`ls $CACHE_DIR/.cache`\n"
