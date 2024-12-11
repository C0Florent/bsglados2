module Lib (
    Value(..),
    Operation(..),
    Instruction(..),
    Insts,
    Stack,
    exec
) where

data Value = Number Int | Boolean Bool
    deriving (Eq, Show)

data Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division
    deriving (Eq, Show)

data Instruction
    = Push Value
    | Call Operation
    | Ret
    deriving (Eq, Show)

type Stack = [Value]
type Insts = [Instruction]

applyOp :: Operation -> Stack -> Either String Stack
applyOp Addition ((Number op1):(Number op2):stack) = Right $ Number (op1 + op2):stack
applyOp Subtraction ((Number op1):(Number op2):stack) = Right $ Number (op1 - op2):stack
applyOp Multiplication ((Number op1):(Number op2):stack) = Right $ Number (op1 * op2):stack
applyOp Division ((Number op1):(Number op2):stack) = Right $ Number (op1 `div` op2):stack
applyOp Division (_:(Number 0):_) = Left "Division by zero"
applyOp op _ = Left $ "Invalid arguments to " ++ show op

exec :: Insts -> Stack -> Either String Value
exec [] _ = Left "Missing return instruction"
exec (Ret:_) [] = Left "No value to return"
exec (Ret:_) (val:_) = Right val
exec ((Push val):insts) stack = exec insts (val:stack)
exec ((Call op):insts) stack = applyOp op stack >>= exec insts
