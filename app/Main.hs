import Lib (exec,
    Instruction (
      Push,
      Ret,
      Call,
      JumpIfFalse
    ),
    Value (Number),
    Operation (Subtraction, Addition, Division, Equals, LessThan),
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
          , Ret
          ]
        , [ Push (Number 10)
          , Push (Number 10)
          , Call Equals
          , Ret
          ]
        , [ Push (Number 10)
          , Push (Number 42)
          , Call Equals
          , Ret
          ]
        , [ Push (Number 2)
          , Push (Number 5)
          , Call LessThan
          , Ret
          ]
        , [ Push (Number 7)
          , Push (Number 9)
          , Call LessThan
          , Ret
          ]
        , [ Push (Number 10)
          , Push (Number 10)
          , Call Equals
          , JumpIfFalse 2
          , Push (Number 1)
          , Ret
          , Push (Number 2)
          , Ret
          ]
        , [ Push (Number 9)
          , Push (Number 10)
          , Call Equals
          , JumpIfFalse 2
          , Push (Number 1)
          , Ret
          , Push (Number 2)
          , Ret
          ]
        ]
