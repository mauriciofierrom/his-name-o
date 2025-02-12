{ name = "his-name-o!"
, dependencies = [ "console", "effect", "halogen", "prelude", "aff"
                 , "maybe", "web-html"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
