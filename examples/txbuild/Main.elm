module Main exposing (main)

import Browser
import Cardano
import Html exposing (Html, div, text)


main : Program () () ()
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        }


example ex =
    case ex () of
        Err error ->
            error

        Ok tx ->
            Cardano.prettyTx tx



-- VIEW


view : () -> Html ()
view _ =
    div []
        [ div [] [ text "Example transaction 1: send 1 ada from me to you" ]
        -- , Html.pre [] [ text <| example Cardano.example1 ]
        , div [] [ text "Example transaction 2: mint dog & burn 1 cat" ]
        , Html.pre [] [ text <| example Cardano.example2 ]
        ]
