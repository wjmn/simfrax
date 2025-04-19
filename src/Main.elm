module Main exposing (..)

import Allele exposing (Allele, AllelePair, AllelePairId(..))
import Array exposing (Array)
import Array.Extra
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import SimAgg
import SimTriplet
import Svg exposing (svg)
import Svg.Attributes as SA
import Triplet exposing (Triplet)



---- MODEL ----


type alias Model =
    { allelePair : AllelePair
    , lastAggPositionsA : List Int
    , lastAggPositionsB : List Int
    , simTripletValues : Array Int
    , simAggValues : Array Int
    }


initialModel : Model
initialModel =
    let
        initialPair =
            { alleleA = Allele.createWithSize 20
            , alleleB = Allele.createWithSize 24
            }
    in
    { allelePair = initialPair
    , lastAggPositionsA = []
    , lastAggPositionsB = []
    , simTripletValues = SimTriplet.calculateFragmentDistribution initialPair
    , simAggValues = SimAgg.calculateFragmentDistribution initialPair
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = ChangedAlleleSize AllelePairId String
    | ClickedAlleleBlock AllelePairId Int
    | NoOp


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedAlleleSize allelePairId string ->
            let
                newSize =
                    case String.toInt string of
                        Just size ->
                            if size >= 0 && size <= Allele.maxAlleleSize then
                                size

                            else if size < 0 then
                                0

                            else
                                Allele.maxAlleleSize

                        Nothing ->
                            0

                allelePair =
                    model.allelePair

                newAllelePair =
                    case allelePairId of
                        AlleleA ->
                            { allelePair | alleleA = List.foldl (\i acc -> Allele.setTripletAtIndex i Triplet.Agg acc) (Allele.changeTripletLength newSize model.allelePair.alleleA) model.lastAggPositionsA }

                        AlleleB ->
                            { allelePair | alleleB = List.foldl (\i acc -> Allele.setTripletAtIndex i Triplet.Agg acc) (Allele.changeTripletLength newSize model.allelePair.alleleB) model.lastAggPositionsB }

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllelePair
            in
            { model | allelePair = newAllelePair, simTripletValues = newCalculatedSimTripletValues, simAggValues = newCalculatedSimAggValues }
                |> withCmd Cmd.none

        ClickedAlleleBlock allelePairId index ->
            let
                allelePair =
                    model.allelePair

                newAllelePair =
                    case allelePairId of
                        AlleleA ->
                            { allelePair | alleleA = Allele.cycleTripletAtIndex index allelePair.alleleA }

                        AlleleB ->
                            { allelePair | alleleB = Allele.cycleTripletAtIndex index allelePair.alleleB }

                lastAggPositionsA =
                    case allelePairId of
                        AlleleA ->
                            if Allele.getTripletAtIndex index allelePair.alleleA == Just Triplet.Agg then
                                List.filter (\i -> i /= index) model.lastAggPositionsA

                            else
                                index :: model.lastAggPositionsA

                        AlleleB ->
                            model.lastAggPositionsA

                lastAggPositionsB =
                    case allelePairId of
                        AlleleB ->
                            if Allele.getTripletAtIndex index allelePair.alleleB == Just Triplet.Agg then
                                List.filter (\i -> i /= index) model.lastAggPositionsB

                            else
                                index :: model.lastAggPositionsB

                        AlleleA ->
                            model.lastAggPositionsB

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllelePair
            in
            { model | allelePair = newAllelePair, lastAggPositionsA = lastAggPositionsA, lastAggPositionsB = lastAggPositionsB, simTripletValues = newCalculatedSimTripletValues, simAggValues = newCalculatedSimAggValues }
                |> withCmd Cmd.none

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ id "section-top" ]
            [ div [ id "section-top-left" ]
                [ h1 [ class "title" ] [ text "SimFrax" ]
                , h2 [ class "subtitle" ] [ text "A fragment analysis simulator for Fragile X genotypes." ]
                , div []
                    [ p [] [ text "Specify the number of ", span [ class "triplet-text text-cgg" ] [ text "CGG" ], text " repeats in each Fragile X allele on the right. Click on a repeat to change it to an ", span [ class "triplet-text text-agg" ] [ text "AGG" ], text " interruption and back." ] ]
                , p [] [ text "The graphs below will update, showing a simulated fragment analysis plot using a standard triplet-repeat primed PCR kit as well as a specific A-primed PCR reaction." ]
                ]
            , div [ id "section-top-right" ]
                [ div [ id "section-top-right-bordered" ]
                    [ div [ class "allele-section-heading" ] [ text "Fragile X Genotype" ]
                    , viewAllele AlleleA model.allelePair.alleleA
                    , viewAllele AlleleB model.allelePair.alleleB
                    ]
                ]
            ]
        , div [ id "section-bottom" ]
            [ div [ id "section-bottom-left" ]
                [ div []
                    [ p [] [ text "This plot shows the simulated fragment distribution for a standard triplet-repeat primed PCR kit. " ] ]
                , p [] [ text "This plot shows the simulated fragment sizes for an A-primed PCR reaction." ]
                ]
            , div [ id "section-bottom-right" ]
                [ viewSimTriplet model
                , viewSimAgg model
                ]
            ]
        ]


viewAlleleVisualTriplet : AllelePairId -> Int -> Triplet -> Html Msg
viewAlleleVisualTriplet allelePairId index triplet =
    div [ class "allele-visual-block", class (Triplet.toString triplet), onClick (ClickedAlleleBlock allelePairId index) ]
        [ div [ class "allele-visual-block-text" ]
            [ text (String.fromInt (1 + index)) ]
        ]


inertBlock : String -> Html Msg
inertBlock inner =
    div [ class "allele-visual-block-inert" ] [ text inner ]


viewAllele : AllelePairId -> Allele -> Html Msg
viewAllele allelePairId allele =
    div [ class "allele-container" ]
        [ div [ class "allele-top" ]
            [ div [ class "allele-label-name" ] [ text ("Allele " ++ Allele.allelePairIdToString allelePairId ++ ":") ]
            , div [ class "allele-label-repeats" ]
                [ input [ class "allele-input", type_ "number", value (Allele.getSize allele |> String.fromInt), onInput (ChangedAlleleSize allelePairId) ] []
                , text " repeats ="
                ]
            , div [ class "allele-label-string" ] [ text (Allele.toGroupedString allele) ]
            ]
        , div [ class "allele-bottom" ]
            [ div [ class "allele-visual-container" ]
                (inertBlock "5'" :: List.indexedMap (viewAlleleVisualTriplet allelePairId) allele.triplets ++ [ inertBlock "3'" ])
            ]
        ]


viewSimTriplet : Model -> Html Msg
viewSimTriplet model =
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox "0 1 1000 110" ]
                [ Svg.line 
                    [ SA.x1 "0", SA.y1 "100", SA.x2 "1000", SA.y2 "100", SA.stroke "blue" ]
                    []
                , Svg.g []
                    [ Svg.polyline
                        [ SA.points
                            (SimTriplet.arrayValuesToPolylinePoints model.simTripletValues
                                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (100 - 10 * y))
                                |> String.join " "
                            )
                        , SA.stroke "blue"
                        , SA.fill "none"
                        , SA.strokeLinejoin "round"
                        ]
                        []
                    ]
                , Svg.g []
                    [Svg.rect [SA.x (String.fromInt (SimTriplet.primerRDistance + SimTriplet.primerRLength)), SA.y "1", SA.width (String.fromInt (Allele.maxAlleleSize * Triplet.size)), SA.height "10", SA.fill "#eee", SA.stroke "green"] []
                    , Svg.rect [SA.x "0", SA.y "11", SA.width "1000", SA.height "89", SA.fill "none", SA.stroke "black"] []]

                -- create small rectangles in increments of 3 along the entire x axis
                , Svg.g []
                    (List.range 0 (1000 // 3)
                        |> List.map (\i -> 
                            Svg.g [SA.class "graph-vline"] 
                             [Svg.rect [ SA.x (String.fromFloat (toFloat i * 3 - 1.5)), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc"] []
                             , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromFloat (toFloat i * 3 - 1.5)), SA.y "110", SA.fill "black", SA.fontSize "9px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt (i * 3) ++ "bp") ]
                             ])
                    )
                ]
            ]
        ]


viewSimAgg : Model -> Html Msg
viewSimAgg model =
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox "0 1 1000 100" ]
                [ Svg.g []
                    [ Svg.polyline
                        [ SA.points
                            (SimAgg.arrayValuesToPolylinePoints model.simAggValues
                                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (100 - 10 * y))
                                |> String.join " "
                            )
                        , SA.stroke "blue"
                        , SA.fill "none"
                        , SA.strokeLinejoin "round"
                        ]
                        []
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
