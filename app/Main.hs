import Lib (exec,
    Instruction (Push,
    Ret,
    Call),
    Value (Number),
    Operation (Subtraction),
    Insts,
    )

runCases :: [Insts] -> IO ()
runCases cases = do
    mapM_ (\insts -> do
        putStr "Running: "
        print insts
        putStr "Result:"
        print $ exec insts []
        putStrLn "==="
        ) cases
    putStrLn "end of tests"

main :: IO ()
main = do
    putStrLn ""
    runCases
        [ [Push (Number 42), Ret]
        , [ Push (Number 10)
          , Push (Number 52)
          , Call Subtraction
          , Ret
        ]
        , [ Push (Number 10)
          , Call Addition
        ]
        , [ Push (Number 0)
          , Push (Number 42)
          , Call Division
          ]
        ]
