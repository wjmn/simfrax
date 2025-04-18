module Triplet exposing (..)

type Triplet 
    = Cgg 
    | Agg

size : Int 
size = 3

cycle : Triplet -> Triplet
cycle t =
    case t of
        Cgg ->
            Agg

        Agg ->
            Cgg

toString : Triplet -> String
toString t =
    case t of
        Cgg ->
            "CGG"

        Agg ->
            "AGG"

fromString : String -> Maybe Triplet
fromString str =
    case str of
        "CGG" ->
            Just Cgg

        "AGG" ->
            Just Agg

        _ ->
            Nothing