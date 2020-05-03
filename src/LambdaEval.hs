module LambdaEval where

import LambdaTypes

import Data.List (union, (\\),find)
import qualified Data.Map.Strict as Map
import Control.Applicative (liftA2)

lreduce :: Lterm -> Either Error Lterm
lreduce Context = Right Context
lreduce (Var x) = Right $ Var x
--doing the substitution before reducing t2 thus we get normal order evaluation
lreduce (App (Abs x t1) t2) =
  lreduce =<< sub t1 x t2
--bellow case was added because (lambda x . (lambda y . x y)) z would
--stop after one application without it.
lreduce (App t1@(App _ _) t2) =
  case lreduce t1 of
    Left err -> Left err
    Right t1Abs@(Abs _ _) -> lreduce $ App t1Abs t2 
    Right t1Any -> App t1Any <$> lreduce t2
lreduce (App t1 t2) = App <$> (lreduce t1) <*> (lreduce t2)
lreduce (Abs x t) = Abs x <$> (lreduce t)


freeVars :: Lterm -> [String]
freeVars Context = []
freeVars (Var x) = [x]
freeVars (App t1 t2) = union (freeVars t1) (freeVars t2) 
freeVars (Abs x t) = freeVars t \\ [x]

--sub expToSubInto variable expToSub
--If you try to sub into the head variable ie: into y in \y.xxy sub will return the
--original expression (\y.xxy in this case) Reason is it can appear as a subexpression
--and we want it to return without an error for example: sub y(\y.xxy) y t
--should be t(\y.xxy)
--sub now has automatic alpha conversion of bound variables
sub :: Lterm -> Variable -> Lterm -> Either Error Lterm
sub (Var x) y t = if x == y then Right t else Right $ Var x
sub exp@(Abs x t1) y t2 = 
  case (x == y, elem x (freeVars t2)) of 
    --(_,True) -> Left $ CannotSubstitute t2 y exp
    (False, True)  ->
      do converted <- (alphaConvert exp newVar)
         sub converted y t2
    (True, _)      -> Right exp
    (False, False) -> Abs x <$> (sub t1 y t2)
  where findNewVar var = if (var ++ "'") `elem` freeVars t2
                           then findNewVar (var ++ "'")
                           else (var ++ "'")
        newVar = findNewVar x
sub (App t1 t2) x t3 = App <$> (sub t1 x t3) <*> (sub t2 x t3)
sub Context _ _ = Right Context

--alphaConvert expression convertToThis
--alphaConvert will only convert expressions that are in head normal form
alphaConvert :: Lterm -> Variable -> Either Error Lterm
alphaConvert (Abs head t) changeTo = 
  if elem changeTo (freeVars t)
  then Left $ CannotAlphaConvert head changeTo t 
  else Right $ Abs changeTo (change t)
    where change (Var x) = if x == head then Var changeTo else Var x
          change (Abs x t1) = if x == head then Abs changeTo (change t1) else Abs x (change t1)
          change (App t1 t2) = App (change t1) (change t2)
alphaConvert t _ = Right t

evalWithContext :: Environment -> [ContextName] -> Lterm -> Either Error Lterm
evalWithContext env usedNames term =
  lreduce =<< collapseContexts env usedNames term

--First element of the list will be the outermost context
--This means that in the case of duplicate definition, the later one
--will overwrite the first.
collapseContexts :: Environment -> [ContextName] -> Lterm -> Either Error Lterm
collapseContexts env usedNames term =
  foldr (liftA2 subContext) (pure term) (map cLookup usedNames)
  where cLookup name = 
         case Map.lookup name env of
           Nothing -> Left $ ContextNotFound name
           Just contextTerm -> Right contextTerm
           
-- context -> term to sub -> result
subContext :: Lterm -> Lterm -> Lterm
subContext Context term = term
subContext v@(Var _) _ = v
subContext (App t1 t2) term = App (subContext t1 term) (subContext t2 term)
subContext (Abs v t) term = Abs v (subContext t term)

--current environment -> new context name -> names of contexts used in the definition
-- -> lambda term defining the new context -> result
insertNewContext :: Environment
                 -> ContextName
                 -> [ContextName]
                 -> Lterm
                 -> Either Error Environment
insertNewContext env contextName usedNames term =
  case collapseContexts env usedNames term of
    Left err -> Left err
    Right colTerm -> Right $ Map.insert contextName colTerm env
