{-# LANGUAGE TypeFamilies #-}

module Lev.Readable where

import Lev.Reader

class Readable a where
    type ReaderCursor a
    type ReaderMonad a :: * -> *
    readWith :: Reader (ReaderCursor a) (ReaderMonad a) r -> a -> (ReaderMonad a) r -- TODO: should I return cursor here?