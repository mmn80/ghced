{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module World (Entity(..), Ref, spawn, withEntity, foldWorld) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as Map

class Entity a where
  health :: a -> Int
  attack :: Int -> a -> a

data EntityRecord = forall a. Entity a => EntityRecord a

world :: MVar (Map.IntMap EntityRecord)
world = unsafePerformIO $ newMVar Map.empty

newtype Ref = Ref { getRef :: Int }

instance Show Ref where show (Ref k) = show k

ref :: MVar Ref
ref = unsafePerformIO . newMVar $ Ref 0

spawn :: Entity a => a -> IO Ref
spawn e = do
  Ref k <- takeMVar ref
  let r = Ref (k + 1)
  putMVar ref (r `seq` r)
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (EntityRecord e) w
  return r

withEntity :: Ref -> (forall a. Entity a => a -> (a, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just (EntityRecord e) -> let (e', x) = f e in
                                            (Map.insert k (EntityRecord e') w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: Monoid m => (forall a. Entity a => Ref -> a -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' k (EntityRecord e) = f (Ref k) e