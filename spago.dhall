{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "expression-problem-purescript"
, dependencies = [ "console", "effect", "math", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
