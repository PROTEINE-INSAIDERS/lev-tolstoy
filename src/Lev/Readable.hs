{-# LANGUAGE TypeFamilies #-}

module Lev.Readable where

import Lev.Reader

class Readable a where
    type ReaderConsumable a
    type ReaderMonad a :: * -> *
    readWith :: Reader (ReaderConsumable a) (ReaderMonad a) r -> a -> (ReaderMonad a) r -- TODO: should I return Consumable here?