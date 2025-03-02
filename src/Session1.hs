{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Session1 where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text

data Expr
  = Lit Int
  | Add Expr Expr
  deriving stock (Show)

--  e ::= i
--     | add(e,e)

-- [[ i ]]          = i
-- [[ add(e1,e2) ]] = [[ e1 ]] + [[ e2 ]]

eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

toText :: Expr -> Text
toText (Lit i) = Text.pack (show i)
toText (Add e1 e2) = "(" <> toText e1 <> " + " <> toText e2 <> ")"

-- Compile to a very simple stack machine
data Instruction
  = PUSH Int
  | ADD
  deriving stock (Show)

compile :: Expr -> [Instruction]
compile (Lit i) = [PUSH i]
compile (Add e1 e2) = compile e1 <> compile e2 <> [ADD]

run :: [Instruction] -> [Int] -> [Int]
-- run is stack = foldl (flip runInstruction) stack is
run [] stack = stack
run (i : is) stack = run is (runInstruction i stack)

runInstruction :: Instruction -> [Int] -> [Int]
runInstruction (PUSH i) stack = i : stack
runInstruction ADD (i1 : i2 : stack) = (i1 + i2) : stack
runInstruction _ _ = error "malformed stack!"

example :: Expr
example = Add (Lit 7) (Add (Lit 11) (Lit 13))

-- >>> compile example
-- [PUSH 7,PUSH 11,PUSH 13,ADD,ADD]

-- >>> run (compile example) []
-- [31]

-- Kleene star, generalization of list: zero or more repetitions of binary relation
data Star (rel :: k -> k -> Type) a b where
  Refl :: Star rel a a -- corresponding to the empty list
  Step :: rel a b -> Star rel b c -> Star rel a c -- corresponding to "cons"

-- data List a where
--   Nil :: List a
--   Cons :: a -> List a -> List a

data TInstruction (stkIn :: [Type]) (stkOut :: [Type]) :: Type where
  TPUSH :: Int -> TInstruction stack (Int : stack)
  TADD :: TInstruction (Int : Int : stack) (Int : stack)

append :: Star rel a b -> Star rel b c -> Star rel a c
append Refl ys = ys
append (Step x xs) ys = Step x (append xs ys)

tcompile :: Expr -> Star TInstruction stack (Int : stack)
tcompile (Lit i) = Step (TPUSH i) Refl
tcompile (Add e1 e2) = tcompile e1 `append` tcompile e2 `append` Step TADD Refl

data Stack (stack :: [Type]) :: Type where
  Nil :: Stack '[]
  Cons :: Int -> Stack stack -> Stack (Int : stack)

trun :: forall stkIn stkOut. Star TInstruction stkIn stkOut -> Stack stkIn -> Stack stkOut
trun Refl stack = stack
trun (Step i is) stack = trun is (trunInstruction i stack)

trunInstruction :: TInstruction stkIn stkOut -> Stack stkIn -> Stack stkOut
trunInstruction (TPUSH n) stack = Cons n stack
trunInstruction TADD (Cons n1 (Cons n2 stack)) = Cons (n1 + n2) stack

data N = Z | S N

data TInstruction' (stkIn :: N) (stkOut :: N) :: Type where
  TPUSH' :: Int -> TInstruction' stack (S stack)
  TADD' :: TInstruction' (S (S stack)) (S stack)

data Stack' (n :: N) :: Type where
  Nil' :: Stack' Z
  Cons' :: Int -> Stack' n -> Stack' (S n)

trun' :: forall stkIn stkOut. Star TInstruction' stkIn stkOut -> Stack' stkIn -> Stack' stkOut
trun' Refl stack = stack
trun' (Step i is) stack = trun' is (trunInstruction' i stack)

trunInstruction' :: TInstruction' stkIn stkOut -> Stack' stkIn -> Stack' stkOut
trunInstruction' (TPUSH' n) stack = Cons' n stack
trunInstruction' TADD' (Cons' n1 (Cons' n2 stack)) = Cons' (n1 + n2) stack