module FRP.GUI.AddHandler where

import FRP.BearRiver
import qualified Data.Unique
import qualified Data.Map as Map
import Data.IORef
import Control.Monad


-- * Code borrowed from Reactive Banana!

type Handler a = a -> IO ()

newtype AddHandler a
  = AddHandler { register :: Handler a -> IO (IO ())}

instance Functor AddHandler where
    fmap f = mapIO (return . f)

-- | Map the event value with an 'IO' action.
mapIO :: (a -> IO b) -> AddHandler a -> AddHandler b
mapIO f e = AddHandler $ \h -> register e (f >=> h)

-- | Filter event values that don't return 'True'.
filterIO :: (a -> IO Bool) -> AddHandler a -> AddHandler a
filterIO f e = AddHandler $ \h ->
    register e $ \x -> f x >>= \b -> when b $ h x

{-----------------------------------------------------------------------------
    Construction
------------------------------------------------------------------------------}


-- | Build a facility to register and unregister event handlers.
-- Also yields a function that takes an event handler and runs all the registered
-- handlers.
--
-- Example:
--
-- > do
-- >     (addHandler, fire) <- newAddHandler
-- >     register addHandler putStrLn
-- >     fire "Hello!"
newAddHandler :: IO (AddHandler a, Handler a)
newAddHandler = do
    handlers <- newIORef Map.empty
    let register handler = do
            key <- Data.Unique.newUnique
            atomicModifyIORef_ handlers $ Map.insert key handler
            return $ atomicModifyIORef_ handlers $ Map.delete key
        runHandlers a =
            runAll a =<< readIORef handlers
    return (AddHandler register, runHandlers)

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())

-- | A callback is a @a -> IO ()@ function. We define this newtype to provide
-- a way to combine callbacks ('Monoid' and 'Semigroup' instances), which
-- allow us to write the efficient 'runAll' function.
newtype Callback a = Callback { invoke :: a -> IO () }

instance Semigroup (Callback a) where
    Callback f <> Callback g = Callback $ \a -> f a >> g a

instance Monoid (Callback a) where
    mempty = Callback $ \_ -> return ()

-- This function can also be seen as
--
--   runAll a fs = mapM_ ($ a) fs
--
-- The reason we write this using 'foldMap' and 'Callback' is to produce code
-- that doesn't allocate. See https://github.com/HeinrichApfelmus/reactive-banana/pull/237
-- for more info.
runAll :: a -> Map.Map Data.Unique.Unique (a -> IO ()) -> IO ()
runAll a fs = invoke (foldMap Callback fs) a
