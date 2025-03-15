{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Session3 where

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
--     |  C <= e
--     |  i <= e
--     |  if C then e1 else e2
--     |  let n = C in e

type Env = Map Name Val

data Stack
  = Top
  | Add1 Stack Expr Env
  | Add2 Val Stack
  | Leq1 Stack Expr Env
  | Leq2 Val Stack
  | IfThenElse1 Stack Expr Expr Env
  | Let1 Name Stack Expr Env
  deriving stock (Show)

-- "forward" is for evaluating expressions into values
forward :: Stack -> Expr -> Env -> Val
forward stack (LInt i) _ = backward stack (VInt i)
forward stack (LBool b) _ = backward stack (VBool b)
forward stack (Add e1 e2) env = forward (Add1 stack e2 env) e1 env
forward stack (Leq e1 e2) env = forward (Leq1 stack e2 env) e1 env
forward stack (IfThenElse e1 e2 e3) env = forward (IfThenElse1 stack e2 e3 env) e1 env
forward stack (Var name) env = backward stack (env Map.! name)
forward stack (Let name e1 e2) env = forward (Let1 name stack e2 env) e1 env

-- call "backward" when you have produced a value, consult the stack to see what to do with it
backward :: Stack -> Val -> Val
backward Top v = v
backward (Add1 stack e2 env) v1 = forward (Add2 v1 stack) e2 env
backward (Add2 v1 stack) v2 = backward stack (add v1 v2)
backward (Leq1 stack e2 env) v1 = forward (Leq2 v1 stack) e2 env
backward (Leq2 v1 stack) v2 = backward stack (leq v1 v2)
backward (IfThenElse1 stack e2 e3 env) v1 = forward stack (ifThenElse v1 e2 e3) env
backward (Let1 name stack e2 env) v1 = forward stack e2 (Map.insert name v1 env)

example2 :: Expr
example2 =
  Let
    "x"
    (LInt 3)
    ( IfThenElse
        (Leq (Add (LInt 4) (LInt 3)) (LInt 4))
        (LInt 1)
        (Var "x")
    )

-- >>> forward Top example2 Map.empty
-- VInt 3

