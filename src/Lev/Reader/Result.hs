module Lev.Reader.Result where

import Control.Exception
import Data.Typeable
    
data Result a = Done !a 
              | Fail !SomeException deriving (Show, Typeable)