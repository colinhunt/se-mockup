mkdir -p build && \
    elm-make src/Main.elm --output=build/elm.js && \
    cp index.html src/main.js build/
