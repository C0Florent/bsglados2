module Lib (
    Value(..),
    Operation(..),
    Instruction(..),
    Insts,
    Stack,
    Args,
    exec
) where

data Value = Number Int | Boolean Bool
    deriving (Eq, Show)

data Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Equals
    | LessThan
    deriving (Eq, Show)

data Instruction
    = Push Value
    | Call Operation
    | JumpIfFalse Int
    | Ret
    deriving (Eq, Show)

type Stack = [Value]
type Insts = [Instruction]

type Args = [Value]

applyOp :: Operation -> Stack -> Either String Stack
applyOp Addition ((Number op1):(Number op2):stack) = Right $ Number (op1 + op2):stack
applyOp Subtraction ((Number op1):(Number op2):stack) = Right $ Number (op1 - op2):stack
applyOp Multiplication ((Number op1):(Number op2):stack) = Right $ Number (op1 * op2):stack
applyOp Division (_:(Number 0):_) = Left "Division by zero"
applyOp Division ((Number op1):(Number op2):stack) = Right $ Number (op1 `div` op2):stack
applyOp Equals (val1:val2:vals) = Right $ Boolean (val1 == val2):vals
applyOp LessThan ((Number val1):(Number val2):vals) = Right $ Boolean (val1 > val2):vals
applyOp op _ = Left $ "Invalid arguments to " ++ show op

exec :: Args -> Insts -> Stack -> Either String Value
exec args [] _ = Left "Missing return instruction"
exec args (Ret:_) [] = Left "No value to return"
exec args (Ret:_) (val:_) = Right val
exec args ((Push val):insts) stack = exec args insts (val:stack)
exec args ((JumpIfFalse n):insts) ((Boolean False):stack) = exec args (drop n insts) stack
exec args ((JumpIfFalse _):insts) ((Boolean True):stack) = exec args insts stack
exec args ((JumpIfFalse _):_) _ = Left "Invalid Jump arguments"
exec args ((Call op):insts) stack = applyOp op stack >>= exec args insts
