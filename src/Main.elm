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
import SimAggRev
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
    , simAggRevValues : Array Int
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
    , simAggRevValues = SimAggRev.calculateFragmentDistribution initialPair
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

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllelePair
            in
            { model | allelePair = newAllelePair, simTripletValues = newCalculatedSimTripletValues, simAggValues = newCalculatedSimAggValues, simAggRevValues = newCalculatedSimAggRevValues }
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

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllelePair
            in
            { model | allelePair = newAllelePair, lastAggPositionsA = lastAggPositionsA, lastAggPositionsB = lastAggPositionsB, simTripletValues = newCalculatedSimTripletValues, simAggValues = newCalculatedSimAggValues, simAggRevValues = newCalculatedSimAggRevValues }
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
                , p [] [ text "The graphs below will update, showing simulated fragment analysis plots." ]
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
                    [ p []
                        [ text "The first plot shows the ", b [] [text "idealised (noiseless)"], text " simulated fragment distribution for a standard triplet-repeat primed PCR kit such as described in "
                        , a [ href "https://pubmed.ncbi.nlm.nih.gov/20616364/" ] [ text "Chen et al 2010" ]
                        , text (". The simulation uses a forward primer of length " ++ String.fromInt SimTriplet.primerFLength ++ "bp located " ++ String.fromInt SimTriplet.primerFDistance ++ "bp from the start of the triplet repeat region, and a reverse primer of length " ++ String.fromInt SimTriplet.primerRLength ++ "bp located " ++ String.fromInt SimTriplet.primerRDistance ++ "bp from the end of the triplet repeat region.")
                        , text (" The triplet repeat primer consists of " ++ String.fromInt (Array.length SimTriplet.tripletPrimer) ++ "x CGG repeats with extra 5' sequence of " ++ String.fromInt SimTriplet.tripletPrimerAddLength ++ "bp.")
                        ]
                    ]
                , p [] [ text "The bottom two plots show the simulated fragment sizes for an A-primed PCR reaction and a T-primed PCR reaction specifically designed to detect AGG repeats as described in " 
                , a [href "https://pubmed.ncbi.nlm.nih.gov/28818679/"] [text "Hayward & Usdin 2017"]
                , text <| ". The simulated A-primed reaction uses a primer with 3x CGG repeats and a 3' A with extra 5' sequence of " ++ String.fromInt SimAgg.tripletPrimerAddLength ++ "bp. The T-primed reaction uses a primer with 3x CCG repeats and a 3' T with extra 5' sequence of " ++ String.fromInt SimAggRev.tripletPrimerAddLength ++ "bp."
                ]
                ]
            , div [ id "section-bottom-right" ]
                [ viewSimTriplet model
                , viewSimAgg model
                , viewSimAggRev model
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
    let

        xmax = 
            SimTriplet.primerRDistance + SimTriplet.primerRLength + (Allele.maxAlleleSize * Triplet.size) + SimTriplet.primerFDistance + SimTriplet.primerFLength + SimTriplet.tripletPrimerAddLength + 10

        xmin = 80
        xminString = String.fromInt xmin

        viewBoxString = 
            xminString ++ " 1 " ++ String.fromInt xmax ++ " 112"

        -- Get location of two full sized alleles
        fullAlleleAUnsorted =
            SimTriplet.getFullFragmentSize model.allelePair.alleleA
        alleleASizeUnsorted = 
            Allele.getSize model.allelePair.alleleA

        fullAlleleBUnsorted =
            SimTriplet.getFullFragmentSize model.allelePair.alleleB

        alleleBSizeUnsorted = 
            Allele.getSize model.allelePair.alleleB

        (alleleASize, alleleBSize) = 
            if alleleASizeUnsorted > alleleBSizeUnsorted then
                (alleleASizeUnsorted, alleleBSizeUnsorted)

            else
                (alleleBSizeUnsorted, alleleASizeUnsorted)

        (fullAlleleA, fullAlleleB) = 
            if fullAlleleAUnsorted > fullAlleleBUnsorted then
                (fullAlleleAUnsorted, fullAlleleBUnsorted)

            else
                (fullAlleleBUnsorted, fullAlleleAUnsorted)
    in
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox viewBoxString ]
                [ 
                -- draw rectangles at the two full alleles
                Svg.g []
                    [ Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleA - 1.5), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc", SA.opacity "0.5" ] []
                    -- put text in grey box
                    , Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleA - 10), SA.y "101", SA.width "20", SA.height "11", SA.fill "#ccc", SA.opacity "0.5", SA.stroke "green" ] []
                    , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromInt fullAlleleA), SA.y "110", SA.fill "black", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt alleleASize) ]
                    , Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleB - 1.5), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc", SA.opacity "0.5" ] []
                    , Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleB - 10), SA.y "101", SA.width "20", SA.height "11", SA.fill "#ccc", SA.opacity "0.5", SA.stroke "green" ] []
                    , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromInt fullAlleleB), SA.y "110", SA.fill "black", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt alleleBSize) ]
                    ]

  
                , Svg.line
                    [ SA.x1 xminString, SA.y1 "100", SA.x2 "1000", SA.y2 "100", SA.stroke "blue" ]
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
                    [ Svg.rect [ SA.x (String.fromInt (SimTriplet.primerRDistance + SimTriplet.primerRLength + 15)), SA.y "1", SA.width (String.fromInt (xmax - 10 - 15 - SimTriplet.primerRDistance - SimTriplet.primerRLength)), SA.height "10", SA.fill "#eee", SA.stroke "green" ] []
                    , Svg.rect [ SA.x xminString, SA.y "11", SA.width (String.fromInt xmax), SA.height "89", SA.fill "none", SA.stroke "black" ] []
                    , Svg.text_ [ SA.x "510", SA.y "9", SA.fill "#222", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text "FRAX" ]
                    , Svg.text_ [ SA.x (String.fromInt (xmin + 4)), SA.y "24", SA.fill "#222", SA.fontSize "10px", SA.textAnchor "left" ] [ Svg.text "Triplet-repeat primed PCR" ]
                    ]

                -- create small rectangles in increments of 3 along the entire x axis
                , Svg.g []
                    (List.range 0 (1000 // 3)
                        |> List.map
                            (\i ->
                                Svg.g [ SA.class "graph-vline" ]
                                    [ Svg.rect [ SA.x (String.fromFloat (toFloat i * 3 - 1.5)), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc" ] []
                                    , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromFloat (toFloat i * 3 - 1.5)), SA.y "110", SA.fill "black", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt (i * 3) ++ "bp") ]
                                    ]
                            )
                    )

                ]
            ]
        ]


viewSimAgg : Model -> Html Msg
viewSimAgg model =
    let
        -- Get array positions of any value above 0
        aboveZeroIndices =
            model.simAggValues
                |> Array.Extra.indexedMapToList
                    (\i v ->
                        if v > 0 then
                            Just i

                        else
                            Nothing
                    )
                |> List.filterMap identity
        xmin = 80
        xminString = String.fromInt xmin
        xmax = 
            SimTriplet.primerRDistance + SimTriplet.primerRLength + (Allele.maxAlleleSize * Triplet.size) + SimTriplet.primerFDistance + SimTriplet.primerFLength + SimTriplet.tripletPrimerAddLength + 10

        xmaxString = String.fromInt xmax
        viewBoxString = 
            xminString ++ " 1 " ++ xmaxString ++ " 110"
    in
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox viewBoxString ]
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

                -- Axis rectangle
                , Svg.g []
                    [ Svg.rect [ SA.x (xminString), SA.y "1", SA.width (String.fromInt xmax ), SA.height "99", SA.fill "none", SA.stroke "black" ] []
                    , Svg.text_ [ SA.x (String.fromInt (xmin + 4)), SA.y "14", SA.fill "#222", SA.fontSize "10px", SA.textAnchor "left", SA.rotate "90deg" ] [ Svg.text "A-primed PCR" ]
                    ]

                -- Add text above all aboveZeroIndices
                , Svg.g []
                    (List.map
                        (\i ->
                            Svg.text_ [ SA.x (String.fromFloat (toFloat i)), SA.y "110", SA.fill "#222", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt i ++ "bp") ]
                        )
                        aboveZeroIndices
                    )
                ]
            ]
        ]


viewSimAggRev : Model -> Html Msg
viewSimAggRev model =
    let
        -- Get array positions of any value above 0
        aboveZeroIndices =
            model.simAggRevValues
                |> Array.Extra.indexedMapToList
                    (\i v ->
                        if v > 0 then
                            Just i

                        else
                            Nothing
                    )
                |> List.filterMap identity

        xmin = 80
        xminString = String.fromInt xmin
        xmax = 
            SimTriplet.primerRDistance + SimTriplet.primerRLength + (Allele.maxAlleleSize * Triplet.size) + SimTriplet.primerFDistance + SimTriplet.primerFLength + SimTriplet.tripletPrimerAddLength + 10

        xmaxString = String.fromInt xmax
        viewBoxString = 
            xminString ++ " 1 " ++ xmaxString ++ " 110"

    in
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox viewBoxString ]
                [ Svg.g []
                    [ Svg.polyline
                        [ SA.points
                            (SimAggRev.arrayValuesToPolylinePoints model.simAggRevValues
                                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (100 - 10 * y))
                                |> String.join " "
                            )
                        , SA.stroke "blue"
                        , SA.fill "none"
                        , SA.strokeLinejoin "round"
                        ]
                        []
                    ]

                -- Axis rectangle
                , Svg.g []
                    [ Svg.rect [ SA.x (xminString), SA.y "1", SA.width (String.fromInt xmax), SA.height "99", SA.fill "none", SA.stroke "black" ] []
                    , Svg.text_ [ SA.x (String.fromInt (xmin + 4)), SA.y "14", SA.fill "#222", SA.fontSize "10px", SA.textAnchor "left", SA.rotate "90deg" ] [ Svg.text "T-primed PCR" ]
                    ]

                -- Add text above all aboveZeroIndices
                , Svg.g []
                    (List.map
                        (\i ->
                            Svg.text_ [ SA.x (String.fromFloat (toFloat i)), SA.y "110", SA.fill "#222", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt i ++ "bp") ]
                        )
                        aboveZeroIndices
                    )
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
