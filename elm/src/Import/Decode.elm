-- | A modified version of the 'decodeSumTaggedObject' function from
-- | https://package.elm-lang.org/packages/bartavelle/json-helpers/
-- | that does not fail on nullary constructors.
-- | Until recently, the default Aeson encoding stored nullary constructors as
-- | {tag: "<tag>", content: []}, but it was recently changed to {tag: "<tag>"}.
-- | This causes Json.Helpers to fail due to the missing 'content' field.
-- | This modification simply causes it to permit a missing content field
-- | for nullary constructors.


module Import.Decode exposing (decodeSumTaggedObject)

import Dict exposing (Dict)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)


decodeSumTaggedObject : String -> String -> String -> Dict String (D.Decoder a) -> Set String -> D.Decoder a
decodeSumTaggedObject name fieldname contentname mapping objectKeys =
    D.field fieldname D.string
        |> D.andThen
            (\key ->
                let
                    decoder =
                        if Set.member key objectKeys then
                            D.value

                        else
                            -- This expression is modified from 'D.field contentname D.value'
                            D.oneOf
                                [ D.field contentname D.value
                                , D.succeed <| E.null
                                ]
                in
                customDecoder decoder (\val -> decodeSumFinal name key val mapping)
            )


customDecoder : D.Decoder a -> (a -> Result D.Error b) -> D.Decoder b
customDecoder decoder toResult =
    decoder
        |> D.andThen
            (\a ->
                case toResult a of
                    Ok b ->
                        D.succeed b

                    Err err ->
                        D.fail <| D.errorToString err
            )


decodeSumFinal :
    String
    -> String
    -> D.Value
    -> Dict String (D.Decoder a)
    -> Result D.Error a
decodeSumFinal name key val mapping =
    case Dict.get key mapping of
        Nothing ->
            Err <|
                D.Failure
                    ("Unknown constructor " ++ key ++ " for type " ++ name)
                    val

        Just dec ->
            D.decodeValue dec val
