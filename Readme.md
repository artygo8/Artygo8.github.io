# Elm App

```bash
# to test
elm-live src/Main.elm -- --output elm.js
# to make
elm make src/Main.elm --output --optimize elm.js
uglifyjs --compress --mangle -o elm.min.js -- elm.js && mv elm.min.js elm.js
```
