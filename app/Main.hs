import Lib (exec, Instruction (Push, Ret, Call), Value (Number), Operation (Subtraction))
main :: IO ()
main = do
    print $ exec [Push (Number 42), Ret] []
    putStrLn ""
    print $ exec
        [ Push (Number 10)
        , Push (Number 52)
        , Call Subtraction
        , Ret
        ]
        []
