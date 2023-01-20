{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "deku"
  , "effect"
  , "float32"
  , "foldable-traversable"
  , "gen"
  , "hyrule"
  , "integers"
  , "js-date"
  , "lcg"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "prelude"
  , "qualified-do"
  , "quickcheck"
  , "random"
  , "record"
  , "refs"
  , "st"
  , "tuples"
  , "uint"
  , "unsafe-coerce"
  , "web-dom"
  , "web-html"
  , "web-promise"
  , "webgpu"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
