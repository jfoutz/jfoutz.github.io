module main where

open import Agda.Builtin.Bool using (Bool ; true ; false)
open import Agda.Builtin.Char using (Char ; primCharToNat)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.List using (List ; [] ; _∷_ )
open import Agda.Builtin.Maybe using (Maybe ; nothing ; just)
open import Agda.Builtin.Nat using (Nat ; _==_)
open import Agda.Builtin.Sigma using (Σ ; _,_)
open import Agda.Builtin.String using (String ; primStringAppend ; primStringUncons ; primStringToList ; primStringFromList)
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



--- ok, so the dumbest thing is to take a datatype, print something, and make that something come back to the datatype

data Foo : Set where
  foo : Foo

printfoo : Foo -> String
printfoo _ = "foo"

parsefoo : String -> Maybe Foo
parsefoo s = help (primStringUncons s)
  where
    help : Maybe (Σ Char (λ _ → String)) -> Maybe Foo
    help (just ('f' , rest)) = just foo
    help (just (c , rest)) = nothing
    help nothing =  nothing

-- ok, so maybe a match a whole string.

-- take x off the front of y
take : String -> String -> String
take x y = primStringFromList (help (primStringToList x) (primStringToList y))
  where
    help : List Char -> List Char -> List Char
    help [] y = y
    help _ [] = []
    help (x ∷ xs) (y ∷ ys) with (primCharToNat x) == (primCharToNat y)
    ...                       | true = help xs ys
    ...                       | false = []

{-
  html
    head
      title
    body
      para
      para
   need matched pairs of <thing></thing>
   i don't want to make a type for every stinking tag
   some tags can have attibutes.

   
-}

data Rel (A : Set) : Set where
  _∨_ : Rel A -> Rel A -> Rel A
  _∧_ : Rel A -> Rel A -> Rel A
  r_  : A -> Rel A



data Tag : String -> Rel String -> Set where
  tag : {name : String} {c : Rel String} -> Tag name c


foo1 : Tag "foo1" (r "bar1")
foo1 = tag

foo1print : {n : String} {c : Rel String} -> Tag n c  -> String
foo1print {n} {c ∨ c₁} _ = ""
foo1print {n} {c ∧ c₁} _ = ""
foo1print {n} {r x} _ = primStringAppend n (primStringAppend x n)

fs1 : String
fs1 = foo1print foo1

{- and or thing, Rel needs work
   need to get something out of the actual tag
   blurring layers trying to understand type syntax.
   -}

  
