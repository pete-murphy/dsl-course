{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Session4 where

import Data.Text (Text)

type Name = Text

data Expr
  = LInt Int
  | Add Expr Expr
  | Var Name
  | Let Name Expr Expr
  deriving stock (Show)

example :: Expr
example =
  Let "x" (Add (LInt 1) (LInt 1)) $
    Let "y" (Var "x") $
      Let "x" (Add (LInt 7) (LInt 7)) $
        Add (Var "y") (Var "x")

-- >>> inlineSmall 100 example
-- Add (Add (LInt 1) (LInt 1)) (Add (LInt 7) (LInt 7))

size :: Expr -> Int
size (LInt _) = 1
size (Add e1 e2) = size e1 + size e2 + 1
size (Var _) = 1
size (Let _ e1 e2) = size e1 + size e2 + 1

inlineSmall :: Int -> Expr -> Expr
inlineSmall _ (LInt i) = LInt i
inlineSmall t (Add e1 e2) = Add (inlineSmall t e1) (inlineSmall t e2)
inlineSmall _ (Var n) = Var n
inlineSmall t (Let n e1 e2) =
  let e1' = inlineSmall t e1
      e2' = inlineSmall t e2
      s1 = size e1'
   in if s1 <= t
        then subst n e1' e2'
        else Let n e1' e2'

subst :: Name -> Expr -> Expr -> Expr
subst _ _ (LInt i) = (LInt i)
subst n e (Add e1 e2) = Add (subst n e e1) (subst n e e2)
subst n e (Var n')
  | n == n' = e
  | otherwise = Var n'
subst n e (Let n' e1 e2)
  -- Leave e2 alone, because references to n will be to a different n
  | n == n' = Let n' e1' e2
  | otherwise = Let n' e1 (subst n e e2)
  where
    e1' = subst n e e1
