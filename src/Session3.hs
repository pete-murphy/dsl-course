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
  | Catch Expr Expr
  deriving stock (Show)

type Name = Text

data Val
  = VInt Int
  | VBool Bool
  deriving stock (Show)

eval :: Expr -> Map Name Val -> Val
eval (LInt i) _ = VInt i
eval (LBool b) _ = VBool b
eval (Add e1 e2) env = eval e1 env `add'` eval e2 env
eval (Leq e1 e2) env = eval e1 env `leq'` eval e2 env
eval (IfThenElse e1 e2 e3) env = eval (ifThenElse' (eval e1 env) e2 e3) env
eval (Var x) env = env Map.! x
eval (Let x e1 e2) env = eval e2 (Map.insert x (eval e1 env) env)
eval (Catch _ e) env = eval e env

add' :: Val -> Val -> Val
add' (VInt i1) (VInt i2) = VInt (i1 + i2)
add' _ _ = error "type error"

leq' :: Val -> Val -> Val
leq' (VInt i1) (VInt i2) = VBool (i1 <= i2)
leq' _ _ = error "type error"

ifThenElse' :: Val -> a -> a -> a
ifThenElse' (VBool True) x _ = x
ifThenElse' (VBool False) _ x = x
ifThenElse' _ _ _ = error "type error"

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

data Error
  = ScopeError Name
  | TypeError
  deriving stock (Show)

data Stack
  = Top
  | Add1 Stack Expr Env
  | Add2 Val Stack
  | Leq1 Stack Expr Env
  | Leq2 Val Stack
  | IfThenElse1 Stack Expr Expr Env
  | Let1 Name Stack Expr Env
  | Catch1 Stack Expr Env
  deriving stock (Show)

note :: forall a. Error -> Maybe a -> Either Error a
note _ (Just x) = Right x
note e Nothing = Left e

-- "forward" is for evaluating expressions into values
forward :: Stack -> Expr -> Env -> Either Error Val
forward stack (LInt i) _ = backward stack (VInt i)
forward stack (LBool b) _ = backward stack (VBool b)
forward stack (Add e1 e2) env = forward (Add1 stack e2 env) e1 env
forward stack (Leq e1 e2) env = forward (Leq1 stack e2 env) e1 env
forward stack (IfThenElse e1 e2 e3) env = forward (IfThenElse1 stack e2 e3 env) e1 env
forward stack (Var name) env = case env Map.!? name of
  Just v -> backward stack v
  Nothing -> unwind stack (ScopeError name)
forward stack (Let name e1 e2) env = forward (Let1 name stack e2 env) e1 env
forward stack (Catch e1 e2) env = forward (Catch1 stack e2 env) e1 env

-- call "backward" when you have produced a value, consult the stack to see what to do with it
backward :: Stack -> Val -> Either Error Val
backward Top v = Right v
backward (Add1 stack e2 env) v1 = forward (Add2 v1 stack) e2 env
backward (Add2 v1 stack) v2 =
  case add v1 v2 of
    Right e -> backward stack e
    Left err -> unwind stack err
backward (Leq1 stack e2 env) v1 = forward (Leq2 v1 stack) e2 env
backward (Leq2 v1 stack) v2 =
  case leq v1 v2 of
    Right e -> backward stack e
    Left err -> unwind stack err
backward (IfThenElse1 stack e2 e3 env) v1 =
  case ifThenElse v1 e2 e3 of
    Right e -> forward stack e env
    Left err -> unwind stack err
backward (Let1 name stack e2 env) v1 = forward stack e2 (Map.insert name v1 env)
backward (Catch1 stack _ _) v = backward stack v

-- like "backward" but for errors
unwind :: Stack -> Error -> Either Error Val
unwind Top err = Left err
unwind (Add1 stack _ _) err = unwind stack err
unwind (Add2 _ stack) err = unwind stack err
unwind (Leq1 stack _ _) err = unwind stack err
unwind (Leq2 _ stack) err = unwind stack err
unwind (IfThenElse1 stack _ _ _) err = unwind stack err
unwind (Let1 _ stack _ _) err = unwind stack err
unwind (Catch1 stack e env) _ = forward stack e env

add :: Val -> Val -> Either Error Val
add (VInt i1) (VInt i2) = Right (VInt (i1 + i2))
add _ _ = Left TypeError

leq :: Val -> Val -> Either Error Val
leq (VInt i1) (VInt i2) = Right (VBool (i1 <= i2))
leq _ _ = Left TypeError

ifThenElse :: Val -> a -> a -> Either Error a
ifThenElse (VBool True) x _ = Right x
ifThenElse (VBool False) _ x = Right x
ifThenElse _ _ _ = Left TypeError

example2 :: Expr
example2 =
  Let
    "x"
    (LInt 3)
    ( IfThenElse
        (Leq (Add (LInt 4) (LInt 3)) (LInt 4))
        (LInt 1)
        (Var "z")
    )

-- >>> forward Top example2 Map.empty
-- Left (ScopeError "z")

example3 :: Expr
example3 =
  Catch (Catch example2 (LInt 99)) (LInt 0)

-- >>> forward Top example3 Map.empty
-- Right (VInt 99)
