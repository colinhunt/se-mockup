#!/usr/bin/env bash

python src/generate-elements.py > src/Layout/Element.elm && \
    elm-format src/Layout/Element.elm --yes && \
    elm-make src/Layout/Element.elm --output=/dev/null
