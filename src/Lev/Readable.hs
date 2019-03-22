{-# LANGUAGE TypeFamilies #-}

module Lev.Readable where

import Lev.Reader

class Readable a where
    type ReaderCursor a
    type ReaderMonad a :: * -> *
    readWith :: a -> Reader (ReaderCursor a) (ReaderMonad a) r -> (ReaderMonad a) r -- TODO: should I return cursor here?