module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoFunctionOutsideOfModules
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ Docs.NoMissing.rule
        { document = onlyExposed
        , from = exposedModules
        }
    , Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    , NoConfusingPrefixOperator.rule

    -- , NoDebug.Log.rule
    -- , NoDebug.TodoOrToString.rule
    --     |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
        |> Rule.ignoreErrorsForFiles [ "src/Cardano/TxExamples.elm" ]

    -- , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
        |> Rule.ignoreErrorsForFiles [ "src/Cardano.elm" ]
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForFiles [ "src/Cardano/TxExamples.elm" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults

    , NoFunctionOutsideOfModules.rule
        -- Hardware Wallets need definite-length encoding
        [ ( [ "Cbor.Encode.indefiniteList"
            , "Cbor.Encode.beginList"
            , "Cbor.Encode.beginDict"
            , "Cbor.Encode.beginString"
            , "Cbor.Encode.beginBytes"
            ]
          , [ "Cbor.Encode.Extra" ]
          )
        -- Hardware Wallets need Map keys sorted in a special order
        , ( [ "Cbor.Encode.associativeList"
            , "Cbor.Encode.dict"
            ]
          , [ "Cbor.Encode.Extra" ]
          )
        ]
    ]
        -- Globally ignore src/TempTxTest.elm for all rules
        |> List.map (Rule.ignoreErrorsForFiles [ "src/TempTxTest.elm" ])
