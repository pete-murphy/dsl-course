{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Session2 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

data Expr
  = LInt Int
  | LBool Bool
  | Add Expr Expr
  | Leq Expr Expr
  | IfThenElse Expr Expr Expr
  | Var Name
  | Let Name Expr Expr
  deriving stock (Show)

type Name = Text

data Val
  = VInt Int
  | VBool Bool
  deriving stock (Show)

eval :: Expr -> Map Name Val -> Val
eval (LInt i) _ = VInt i
eval (LBool b) _ = VBool b
eval (Add e1 e2) env = eval e1 env `add` eval e2 env
eval (Leq e1 e2) env = eval e1 env `leq` eval e2 env
eval (IfThenElse e1 e2 e3) env = eval (ifThenElse (eval e1 env) e2 e3) env
eval (Var x) env = env Map.! x
eval (Let x e1 e2) env = eval e2 (Map.insert x (eval e1 env) env)

add :: Val -> Val -> Val
add (VInt i1) (VInt i2) = VInt (i1 + i2)
add _ _ = error "type error"

leq :: Val -> Val -> Val
leq (VInt i1) (VInt i2) = VBool (i1 <= i2)
leq _ _ = error "type error"

ifThenElse :: Val -> a -> a -> a
ifThenElse (VBool True) x _ = x
ifThenElse (VBool False) _ x = x
ifThenElse _ _ _ = error "type error"

example :: Expr
example = Let "x" (Add (LInt 3) (LInt 7)) (IfThenElse (Leq (Var "x") (LInt 11)) (LBool False) (LBool True))

-- >>> eval example Map.empty
-- VBool False

--  e ::= i
--     | add(e,e)
--
--  v ::= i
--
--  C ::= [_]    -- hole
--     |  C + e
--     |  i + C

data Stack
  = Top
  | Add1 Stack Expr
  | Add2 Int Stack
  deriving stock (Show)

{-
forward :: Stack -> Expr -> Int
forward stack (Lit i) = backward stack i
forward stack (Add e1 e2) = forward (Add1 stack e2) e1

backward :: Stack -> Int -> Int
backward Top !i = i
backward (Add1 stack e2) !i = forward (Add2 i stack) e2
backward (Add2 i1 stack) !i = backward stack (i1 + i)

example :: Expr
example = Add (Add (Lit 3) (Lit 7)) (Add (Lit 5) (Lit 6))

-- >>> forward Top example
-- 21
-}