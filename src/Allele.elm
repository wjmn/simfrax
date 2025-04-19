module Allele exposing (..)

import Triplet exposing (Triplet, cycle, toString, fromString)
import List.Extra
import Array exposing (Array)
import Array.Extra 


type alias Allele = 
    { triplets: List Triplet }

type alias AllelePair = 
    { alleleA : Allele, alleleB: Allele }

type AllelePairId = AlleleA | AlleleB

allelePairIdToString : AllelePairId -> String
allelePairIdToString allelePairId =
    case allelePairId of
        AlleleA ->
            "A"

        AlleleB ->
            "B"

createWithSize : Int -> Allele
createWithSize size =
    { triplets = List.repeat size Triplet.Cgg }


maxAlleleSize : Int 
maxAlleleSize = 225

getSize : Allele -> Int
getSize allele =
    List.length allele.triplets

{-| Count number of Agg interruptions in allele -}
numInterruptions: Allele -> Int
numInterruptions allele =
    allele.triplets 
        |> List.filter (\t -> t == Triplet.Agg)
        |> List.length


addTriplet: Triplet -> Allele -> Allele
addTriplet triplet allele =
    { allele 
        | triplets = allele.triplets ++ [triplet]
    }

removeLastTriplet: Allele -> Allele
removeLastTriplet allele =
    { allele | triplets = List.take (List.length allele.triplets - 1) allele.triplets }

removeTripletAtIndex : Int -> Allele -> Allele
removeTripletAtIndex index allele =
    { allele | triplets = List.Extra.removeAt index allele.triplets }

getTripletAtIndex : Int -> Allele -> Maybe Triplet
getTripletAtIndex index allele =
    List.Extra.getAt index allele.triplets

setTripletAtIndex : Int -> Triplet -> Allele -> Allele
setTripletAtIndex index triplet allele =
    { allele | triplets = List.indexedMap (\i t -> if i == index then triplet else t) allele.triplets }

cycleTripletAtIndex : Int -> Allele -> Allele
cycleTripletAtIndex index allele =
    { allele | triplets = List.indexedMap (\i t -> if i == index then cycle t else t) allele.triplets }

changeTripletLength : Int -> Allele -> Allele
changeTripletLength newLength allele =
    let
        currentLength = List.length allele.triplets
    in
    if newLength > currentLength then
        { allele | triplets = allele.triplets ++ List.repeat (newLength - currentLength) Triplet.Cgg }
    else
        { allele | triplets = List.take newLength allele.triplets }


{-| Convert allele to string in format (CGG)n(AGG)m(CGG)k etc. -}
toGroupedString : Allele -> String
toGroupedString allele = 
    allele.triplets 
        |> List.Extra.groupWhile (==) 
        |> List.map (\ (first, rest) -> (first, List.length rest + 1))
        |> List.map (\(triplet, count) -> if count == 1 then toString triplet else "(" ++ toString triplet ++ ")" ++ (String.fromInt count))
        |> String.join " "


{-| Create all possible k-sized sliding windows of triplet repeats, converting to arrays -}
createSlidingWindows : Int -> Allele -> List (Array Triplet)
createSlidingWindows k allele =
    let
        tripletArray = Array.fromList allele.triplets
        length = Array.length tripletArray
        slidingWindows =
            List.range 0 (length - k)
                |> List.map (\i -> Array.slice i (i + k) tripletArray)
    in 
    slidingWindows

{-| Creates a list of primer binding sites in repeat sequence (from 5' end) -}
getPrimerBindIndices : Array Triplet -> Allele -> List Int
getPrimerBindIndices primer allele = 
    let
        tripletArray = Array.fromList allele.triplets
        length = Array.length tripletArray
        primerLength = Array.length primer
        slidingWindows = 
            List.range 0 (length - primerLength)
                |> List.filterMap (\i -> if Array.slice i (i + primerLength) tripletArray == primer then Just i else Nothing)
    in
    slidingWindows 

