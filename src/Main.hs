-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : BSD-style
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Playing around with the GHC API.
-----------------------------------------------------------------------------

module Main (main, inspectWorld) where

import DynFlags
import GHC
import GHC.Paths
import Unsafe.Coerce
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import Control.Monad (filterM)
import Data.List (find)
import World

main :: IO ()
main = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      _ <- setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                       , ghcLink   = LinkInMemory
                                       , ghcMode   = CompManager
                                       }
      setContext [ preludeCtx ]
      loop


preludeCtx :: InteractiveImport
preludeCtx = IIDecl . simpleImportDecl $ mkModuleName "Prelude"

loop :: Ghc ()
loop = do
  cmd <- liftIO $ putStr "> " >> hFlush stdout >> getLine
  if cmd == ":q" then return () else do
    case cmd of
      ':' : c : p -> let param = if null p then "" else tail p in case c of
        'l' -> loadModuleStr param
        'd' -> showDirs
        _   -> liftIO $ putStrLn "Unrecognized command"
      expr -> do
        act <- unsafeCoerce <$> compileExpr ("print (" ++ expr ++ ")")
        liftIO act
    loop

loadModuleStr :: String -> Ghc ()
loadModuleStr str = do
  t <- guessTarget str Nothing
  setTargets [t]
  _ <- load LoadAllTargets
  ms <- getLoadedModules
  case find (byTarget t) ms of
         Nothing -> liftIO $ putStrLn "Module not found?!"
         Just sm -> setContext [ IIModule . moduleName $ ms_mod sm
                               , preludeCtx
                               ]
  where
    byTarget (Target (TargetModule m) _ _) sm = ms_mod_name sm == m
    byTarget (Target (TargetFile f _) _ _) sm
      | Just f' <- ml_hs_file (ms_location sm) = f == f'
    byTarget _ _ = False

getLoadedModules :: GhcMonad m => m [ModSummary]
getLoadedModules = do
  graph <- getModuleGraph
  filterM (isLoaded . ms_mod_name) graph

showDirs :: Ghc ()
showDirs = liftIO $ do
  putStrLn $ "ghc: " ++ ghc
  putStrLn $ "ghc_pkg: " ++ ghc_pkg
  putStrLn $ "libdir: " ++ libdir
  putStrLn $ "docdir: " ++ docdir
 
inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id: " ++ show r ++ ", health: " ++ show (health e) ++ ") "