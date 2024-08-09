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


example1 =
    case Cardano.example1 () of
        Err error ->
            error

        Ok tx ->
            Cardano.prettyTx tx



-- VIEW


view : () -> Html ()
view _ =
    div []
        [ div [] [ text "Example transaction 1:" ]
        , Html.pre [] [ text example1 ]
        ]
