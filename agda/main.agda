module main where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.List using (List ; [] ; _∷_ )
open import Agda.Builtin.Maybe using (Maybe ; nothing ; just)
open import Agda.Builtin.String using (String ; primStringAppend ; primStringUncons)
open import Agda.Builtin.Unit using (⊤)


postulate putStr : String -> IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStr = putStr . T.unpack #-}

strcat : List String -> String
strcat [] = ""
strcat (x ∷ l) = primStringAppend x (strcat l)

{-


html_foo : String
html_foo = html_ "stuff"
-}

  module Tags where
    -- i'm sure there's a better way
    -- refactoring will be magical
    
    data Title : Set where
      title : String -> Title
    data Head : Set where
      head : Title -> Head
    data P : Set where
      p : String -> P
    data Body : Set where
      body : List P -> Body
    data Html : Set where
      html : Head -> Body -> Html

    ex : Html
    ex = html (head (title "hello")) (body ((p "paragraph 1") ∷ []))


    tag : String -> String -> String
    tag tag b = strcat ( "<" ∷ tag ∷ ">" ∷ b ∷ "</" ∷ tag ∷ ">" ∷ [] )


    html_ : String -> String
    html_ c = strcat ("<!DOCTYPE html>\n" ∷ (tag "html" c) ∷ [])

    print1 : Html -> String
    print1 (html _ _) = html_ ""

    parse1 : String -> Maybe Html
    parse1 s = nothing

    ex2 : String
    ex2 = print1 ex



