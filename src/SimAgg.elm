module SimAgg exposing (..)

import Triplet exposing (Triplet(..))
import Allele exposing (Allele, AllelePair)
import Array exposing (Array)
import Array.Extra



primerRDistance : Int
primerRDistance = 90

primerRLength : Int
primerRLength = 25

tripletPrimerAddLength : Int
tripletPrimerAddLength = 23


tripletPrimer : Array Triplet
tripletPrimer = 
    Array.fromList [Cgg, Cgg, Cgg, Agg]

getTripletFragmentSizes : Allele -> List Int 
getTripletFragmentSizes allele = 
    let
        alleleSize = Allele.getSize allele
    in
    allele 
    |> Allele.getPrimerBindIndices tripletPrimer
    |> List.map (\i -> (alleleSize - i) * Triplet.size + primerRDistance + primerRLength + tripletPrimerAddLength)
    
calculateFragmentDistribution : AllelePair -> Array Int 
calculateFragmentDistribution allelePair = 
    let
        -- Initialise an array of max allele size with all values set to 0 
        relFreq = Array.repeat (Allele.maxAlleleSize * Triplet.size + primerRDistance + primerRLength + tripletPrimerAddLength) 0
        -- Get fragment distributions for each allele triplet repeat primed
        alleleAFragmentSizes = getTripletFragmentSizes allelePair.alleleA 
        alleleBFragmentSizes = getTripletFragmentSizes allelePair.alleleB 
        relFreqWithTrip = 
            List.foldl (\size acc -> Array.set size (Maybe.withDefault 0 (Array.get size acc) + 12) acc) relFreq (alleleAFragmentSizes ++ alleleBFragmentSizes)
        -- For every full fragment size, add 10 to the relFreq array at the index of the full fragment size
    in 
    -- Return the final array with all fragment sizes and their relative frequencies
    relFreqWithTrip

arrayValuesToPolylinePoints : Array Int -> List (Float, Float)
arrayValuesToPolylinePoints array = 
    let
        list = 
            array 
            |> Array.Extra.indexedMapToList (\i v -> (toFloat i, toFloat v * 1.003 ^ (0 - (toFloat i))))
    in 
    list 