module LambdaTypes where

import Data.List (union, (\\),find,intersperse)
import qualified Data.Map.Strict as Map
--import Control.Applicative (liftA2)

import Text.Parsec (ParseError)

--------
--Lambda term definition
--------

type Variable = String
type ContextName = String
type Environment = Map.Map ContextName Lterm

data Lterm = Var Variable
           | Abs Variable Lterm
           | App Lterm Lterm
           | Context
           deriving (Eq)

instance Show Lterm where
  show (Var x) = x

  show (Abs x t@(Abs _ _)) = "(lambda " ++ x ++nestedShow t ++ ")"
    where nestedShow (Abs y t'@(Abs _ _)) = " " ++ y ++ nestedShow t'
          nestedShow (Abs y t') = " " ++ y ++ " . " ++ show t'
          nestedShow _ = "Damn son, this case should not be possible!"

  show (Abs x t) = "(lambda " ++ x ++ " . " ++ show t ++ ")"

  show (App t1 t2@(App _ _)) = show t1 ++ " (" ++ show t2 ++ ")"

  show (App t1 t2) = show t1 ++ " " ++ show t2

  show Context = "[ ]"

--------
--Program inputs
--------

data ReplInput =
    ReplLterm                       [ContextName] Lterm
  | ReplContextDef     ContextName  [ContextName] Lterm
  deriving (Eq)
  
instance Show ReplInput where

  show (ReplLterm usedNames term) = show term

  show (ReplContextDef contextName usedNames term) =
    let usedNamesString = concat $ intersperse ", " usedNames
     in  case usedNames of
          [] -> contextName ++ " = " ++ show term
          _  -> concat [ contextName , " = [", usedNamesString
                       , " | ", show term, "]"]

----------
--Errors
----------

data Error =
    ContextNotFound String
  | CannotParse ParseError
  | MultipleContexts String Lterm
  | ZeroContexes String Lterm
  | CannotAlphaConvert { convertThis :: Variable
                       , convertInto :: Variable
                       , inTerm      :: Lterm}
{-| CannotSubstitute { subThis  :: Lterm
                     , var     :: Variable
                     , subInto :: Lterm}
With automatic alpha conversion this should no longer be needed-}

instance Show Error where
  show (ContextNotFound name) =
    "The context \"" ++ name ++ "\" is not defined!"

  show (CannotParse err) = show err
    
  show (MultipleContexts name term) =
    "You can not have multiple contexts in your context definition: "
    ++ name ++ " = " ++ show term

  show (ZeroContexes name term) =
    "You must have an empty context in your context definition: "
    ++ name ++ " = " ++ show term

  show x@(CannotAlphaConvert _ _ _) =
    "Cannot alpha convert " ++ convertThis x ++ " into "
    ++ convertInto x ++ " in " ++ show (inTerm x) ++ "!"
    
{-show x@(CannotSubstitute _ _ _) =
    "Cannot substitute " ++ show (subThis x) ++ " into "
    ++ var x ++ " in " ++ show (subInto x) ++ "!"-}
