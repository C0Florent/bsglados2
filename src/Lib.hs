module Lib (
    Value(..),
    Operation(..),
    Instruction(..),
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

applyOp :: Operation -> Stack -> Stack
applyOp Addition ((Number op1):(Number op2):stack) = Number (op1 + op2):stack
applyOp Subtraction ((Number op1):(Number op2):stack) = Number (op1 - op2):stack
applyOp Multiplication ((Number op1):(Number op2):stack) = Number (op1 * op2):stack
applyOp Division ((Number op1):(Number op2):stack) = Number (op1 `div` op2):stack
applyOp _ _ = error "Invalid operation"

exec :: Insts -> Stack -> Value
exec [] _ = error "Missing return instruction"
exec (Ret:_) [] = error "No value to return"
exec (Ret:_) (val:_) = val
exec ((Push val):insts) stack = exec insts (val:stack)
exec ((Call op):insts) stack = exec insts (applyOp op stack)
