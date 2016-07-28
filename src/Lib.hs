{-# language TypeFamilies, MultiParamTypeClasses #-}
module Lib
    -- ( someFunc
    -- )
    where

import Data.Char
-- import qualified Data.IntMap as IM
import qualified Data.Map.Strict as Map
import qualified Data.Graph.Inductive.Graph as G

import Control.Monad


someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- | symbolic differentiation

-- | linear operator application : y = A x

-- | solution of inverse linear system : x ~ A^-1 y

-- | inner product : <x1, x2>
-- -- norm, distance etc.

-- | pointwise function application : exp(<x, x>) or sqrt(abs(<x, A x>))
-- -- " : linear, quadratic etc forms are instances of Num

-- notion of `equation` :
-- -- two functional terms
-- -- invertible ("cancellative") operations : e.g. full rank matrices

{- laws:

f (Lterm) = Rterm === Lterm = f^-1 (Rterm)

-}


-- class Biject a b where
--   fwdAp :: (a -> b) -> a -> b
--   bwdAp :: (b -> a) -> b -> a

-- {- laws :
-- bwd g . fwd f == id
-- -}


  
-- | observable sharing between AST nodes (see A.Gill, "Type-safe observable sharing")


-- o -- o -- o -- o -- o -- o -- 

-- | version 1:
-- -- pure symbolic manipulation
-- -- no environment (therefore no lambdas or application to arguments)



data UnOp = Abs | Neg |  Sin | Cos | Exp | Sqrt | Pow Int deriving (Eq, Show)
showOp f = map toLower (show f)
data BinOp = Sum | Prod | Div | Sub deriving (Eq, Show)
showBinOp t = case t of Sum -> " + "
                        Prod -> " * "
                        Div -> " / "
                        Sub -> " - "


type Name = String
data Expr a = EConst a
            | EVar Name
            | EUop UnOp (Expr a)
            | EBop BinOp (Expr a) (Expr a)

instance Show a => Show (Expr a) where
  show (EConst a) = show a
  show (EVar n) = n
  show (EUop (Pow n) x) = "("++ show x ++")^" ++ show n
  show (EUop f x) = showOp f ++ "( "++ show x ++" )"
  show (EBop f2 x y) = show x ++ showBinOp f2 ++ show y




-- Show example

x = EVar "X"
y = EVar "Y"
cos_ = EUop Cos
exp_ = EUop Exp
sum_ = EBop Sum
prod_ = EBop Prod
sqrt_ = EUop Sqrt

ex0 = sqrt_ ( cos_ (sum_ x y)) `prod_` cos_ y




-- addEnv (EVar v) (EConst x) = Map.insert v


