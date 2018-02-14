#!/usr/bin/env bash

CACHE_DIR="node_modules"

if [ $INSTALL_WEBPACK ]; then
  echo "##> Installing webpack..."
  npm install -g webpack
  echo "##> Installed webpack."
fi

# detect netlify (or other ci)
if [ $REPOSITORY_URL ]; then
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
fi

function do_webpack {
  # sysconfcpus workaround: https://github.com/elm-lang/elm-compiler/issues/1473
  if sysconfcpus -n 1 webpack "$@" --progress ; then
    echo "sysconfcpus -n 1 build succeeeded"
  else
    echo "sysconfcpus failed, falling back to regular build"
    webpack "$@" --progress
  fi
}

# do build
echo "Building now..."
time do_webpack "$@"
echo "Build Complete"

if [ $REPOSITORY_URL ]; then
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
fi
