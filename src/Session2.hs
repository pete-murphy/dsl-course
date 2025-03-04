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
--     |  C <= e
--     |  i <= e
--     |  if C then e1 else e2
--     |  if b then C else e
--     |  if b then e else C
--     |  let n = C in e

type Env = Map Name Val

data Stack
  = Top
  | Add1 Stack Expr Env
  | Add2 Int Stack
  | Leq1 Stack Expr Env
  | Leq2 Int Stack
  | IfThenElse1 Stack Expr Expr Env
  | IfThenElse2 Bool Stack Expr Env
  | IfThenElse3 Bool Val Stack
  | Let1 Name Stack Expr Env
  deriving stock (Show)

forward :: Stack -> Expr -> Env -> Val
forward stack (LInt i) _ = backward stack (VInt i)
forward stack (LBool b) _ = backward stack (VBool b)
forward stack (Add e1 e2) env = forward (Add1 stack e2 env) e1 env
forward stack (Leq e1 e2) env = forward (Leq1 stack e2 env) e1 env
forward stack (IfThenElse e1 e2 e3) env = forward (IfThenElse1 stack e2 e3 env) e1 env
forward stack (Var name) env = backward stack (env Map.! name)
forward stack (Let name e1 e2) env = forward (Let1 name stack e2 env) e1 env

backward :: Stack -> Val -> Val
backward Top i = i
backward (Add1 stack e env) v = forward (add1 v stack) e env
backward (Add2 i stack) v = backward stack (add2 i v)
backward (Leq1 stack e env) v = forward (leq1 v stack) e env
backward (Leq2 i stack) v = backward stack (leq2 i v)
backward (IfThenElse1 stack e1 e2 env) v = forward (ifThenElse1 v stack e2 env) e1 env
backward (IfThenElse2 b stack e env) v = forward (IfThenElse3 b v stack) e env
backward (IfThenElse3 b v1 stack) v2 = backward stack (if b then v1 else v2)
backward (Let1 name stack e env) v = forward stack e (Map.insert name v env)

add1 :: Val -> Stack -> Stack
add1 (VInt i) stack = Add2 i stack
add1 _ _ = error "type error"

add2 :: Int -> Val -> Val
add2 i1 (VInt i2) = VInt (i1 + i2)
add2 _ _ = error "type error"

leq1 :: Val -> Stack -> Stack
leq1 (VInt i) stack = Leq2 i stack
leq1 _ _ = error "type error"

leq2 :: Int -> Val -> Val
leq2 i1 (VInt i2) = VBool (i1 <= i2)
leq2 _ _ = error "type error"

ifThenElse1 :: Val -> Stack -> Expr -> Env -> Stack
ifThenElse1 (VBool b) stack e env = IfThenElse2 b stack e env
ifThenElse1 _ _ _ _ = error "type error"

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
