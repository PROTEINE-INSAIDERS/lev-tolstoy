data Result = Done | Fail 

{-# NOINLINE ask #-}
ask :: Int -> Result
ask a = if a == 42 then Done else Fail

main :: IO ()
main = case (ask 42) of
    Done -> putStrLn "done."
    Fail -> putStrLn "error!"
