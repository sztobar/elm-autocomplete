# elm-autocomplete
Elm autocomplete example

Inspired by [David Nolen](https://github.com/swannodette/) blog post on writing similar ui widget in [clojurescript](http://swannodette.github.io/2013/08/17/comparative). Elm doesn't have any csp library and dropped usage of FRP with 0.17 update. With [Elm architecture](https://guide.elm-lang.org/architecture/) the same functionality could be accomplished with readable and reasonable codebase.

To use:
* `npm install`
* `npm install -g elm-live` - if You don't have [Elm live](https://github.com/tomekwi/elm-live) as it's required
* `init.sh` or just `elm-live Main.elm --ouptut="elm.js"` and head to `localhost:800`
