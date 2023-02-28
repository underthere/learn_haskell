module Main (main) where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madLibBin :: Exclamation 
                -> Adverb 
                -> Noun
                -> Adjective
                -> String
madLibBin e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped in to hist car ", noun, " and drove with his ", adj, " wife."]


main :: IO ()
main = do
  putStrLn (madLibBin "Aha" "loudly" "bee" "special")
