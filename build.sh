#!/usr/bin/env bash

mkdir -p build && \
    elm-make src/Main.elm --output=build/elm.js && \
    echo 'Copying files to build dir...' && \
    cp src/index.html src/main.js build/ && \
    echo 'Build successful!'
