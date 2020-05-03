module LambdaRepl where

import LambdaTypes
import LambdaEval
import LambdaParser

import Control.Monad
import System.IO
import Control.Monad.Trans
import System.Console.Haskeline

import qualified Data.Map as Map

type Repl a = InputT IO a


process :: Environment -> String -> (Environment, String)
process env input =
  case runReplParser input of
    
    Left parseError -> (env, show parseError)
    
    Right (ReplLterm usedNames term) ->
      let result = evalWithContext env usedNames term
       in (env,show' result)
       
    Right inp@(ReplContextDef contextName usedNames term) ->
      case insertNewContext env contextName usedNames term of
        Left err -> (env, show err)
        Right newEnv -> (newEnv, show inp)       
  where show' = either show show
        

repl :: Environment -> Repl ()
repl env = do
  minput <- getInputLine "Repl>"
  case minput of
    Nothing -> outputStrLn "Goodbye"
    Just ":q" -> outputStrLn "Goodbye"
    Just ":r" -> 
      do (newEnv, result) <- lift (loadFile env)
         (liftIO $ putStrLn result) >> repl newEnv
    Just input -> 
      let (newEnv, result) = process env input
       in (liftIO $ putStrLn result) >> repl newEnv

loadFile :: Environment -> IO (Environment, String)
loadFile env = do
  errOrContexts <- contextFile
  case (makeEnvironment =<< errOrContexts) of 
    Left err          -> return (env, show err) 
    Right envFromFile -> return (Map.union envFromFile env,"Success")
                        --Map.union is left biased so the order is important here
                        --We want newer definitions to overwrite old ones

runRepl :: IO ()
runRepl = do
  errOrContexts <- contextFile
  case (makeEnvironment =<< errOrContexts) of 
    Left err          ->
       putStrLn (show err) >> runInputT defaultSettings (repl mempty)
    Right rawContexts ->
       runInputT defaultSettings (repl rawContexts)
  

makeEnvironment :: [ReplInput] -> Either Error Environment
makeEnvironment ls =
  foldl foldingFun (pure mempty) ls
  where foldingFun :: Either Error Environment -> ReplInput -> Either Error Environment
        foldingFun eeEnv (ReplContextDef contextName usedNames term) = do
          env <- eeEnv
          insertNewContext env contextName usedNames term
        foldingFun eeEnv _ = eeEnv
  

