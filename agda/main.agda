module main where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.List using (List ; [] ; _∷_ )
open import Agda.Builtin.String using (String ; primStringAppend)
open import Agda.Builtin.Unit using (⊤)


postulate putStr : String -> IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStr = putStr . T.unpack #-}

strcat : List String -> String
strcat [] = ""
strcat (x ∷ l) = primStringAppend x (strcat l)

tag : String -> String -> String
tag tag b = strcat ( "<" ∷ tag ∷ ">" ∷ b ∷ "</" ∷ tag ∷ ">" ∷ [] )

html_ : String -> String
html_ c = strcat ("<!DOCTYPE html>\n" ∷ (tag "html" c) ∷ [])

html_foo : String
html_foo = html_ "stuff"




