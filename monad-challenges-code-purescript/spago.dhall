{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "monad-challenges-code-purescript"
, dependencies = [ "arrays"
                 , "console"
                 , "effect"
                 , "psci-support"
                 , "strings"
                 , "stringutils"
                 , "tuples"
                 , "foldable-traversable" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
