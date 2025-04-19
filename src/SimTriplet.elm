module SimTriplet exposing (..)

import Triplet exposing (Triplet(..))
import Allele exposing (Allele, AllelePair)
import Array exposing (Array)
import Array.Extra

{- Brainstorming notes

The simulated kit produces fragments in two ways:
1. Fragments of the full repeat plus flanking primer distances using primers at each end
   ..... CGG etc.  .......
->                     <-

2. Fragments of all possible CGG triplet repeat sizes plus one flanking primer distance 

......CGG etc. ........
       ->           <- 

-}

{-| Distance from forward primer to start of triplet repeat region -}
primerFDistance : Int
primerFDistance = 91

primerFLength : Int 
primerFLength = 25

{-| Distance from reverse primer to end of triplet repeat region -}
primerRDistance : Int
primerRDistance = 90

primerRLength : Int
primerRLength = 25

tripletPrimer : Array Triplet
tripletPrimer = 
    Array.fromList [Cgg, Cgg, Cgg, Cgg, Cgg]

tripletPrimerAddLength : Int 
tripletPrimerAddLength =  23


getTripletFragmentSizes : Allele -> List Int 
getTripletFragmentSizes allele = 
    let
        alleleSize = Allele.getSize allele
    in
    allele 
    |> Allele.getPrimerBindIndices tripletPrimer
    |> List.map (\i -> (alleleSize - i) * Triplet.size + primerRDistance + primerRLength + tripletPrimerAddLength)
    
getFullFragmentSize : Allele -> Int
getFullFragmentSize allele = 
    let
        alleleSize = Allele.getSize allele
    in
    if alleleSize == 0 then 
        0 
    else 
        (alleleSize * Triplet.size) + primerFDistance  + primerFLength + primerRDistance + primerRLength

calculateFragmentDistribution : AllelePair -> Array Int 
calculateFragmentDistribution allelePair = 
    let
        -- Initialise an array of max allele size with all values set to 0 
        relFreq = Array.repeat (Allele.maxAlleleSize * Triplet.size + primerFDistance + primerFLength + primerRDistance + primerRLength + 1) 0
        -- Get fragment distributions for each allele triplet repeat primed
        alleleAFragmentSizes = getTripletFragmentSizes allelePair.alleleA 
        alleleBFragmentSizes = getTripletFragmentSizes allelePair.alleleB 
        -- Get full fragment size for each allele
        alleleAFullFragmentSize = getFullFragmentSize allelePair.alleleA
        alleleBFullFragmentSize = getFullFragmentSize allelePair.alleleB
        alleleFullFragmentSizes = [alleleAFullFragmentSize, alleleBFullFragmentSize] |> List.filter (\size -> size > 0)
        -- For every triplet fragment size, add 1 to the relFreq array at the index of the fragment size
        relFreqWithTrip = 
            List.foldl (\size acc -> Array.set size (Maybe.withDefault 0 (Array.get size acc) + 1) acc) relFreq (alleleAFragmentSizes ++ alleleBFragmentSizes)
        -- For every full fragment size, add 10 to the relFreq array at the index of the full fragment size
        relFreqWithFull = 
            List.foldl (\size acc -> Array.set size (Maybe.withDefault 0 (Array.get size acc) + 8) acc) relFreqWithTrip alleleFullFragmentSizes
    in 
    -- Return the final array with all fragment sizes and their relative frequencies
    relFreqWithFull

arrayValuesToPolylinePoints : Array Int -> List (Float, Float)
arrayValuesToPolylinePoints array = 
    let
        list = 
            array 
            |> Array.Extra.indexedMapToList (\i v -> (toFloat i, toFloat v * 5 * 1.005 ^ (0 - (toFloat i))))
    in 
    list 