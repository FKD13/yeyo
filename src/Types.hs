module Types(Expression(..), Value(..), Program) where

type Program = [Expression]

data Expression = SetExpr Value Value
                | EmptyExpr
                | UnsetExpr Value
                | SayExpr Value
                | AskExpr Value
                | IfExpr Value [Expression] [Expression]
                | WhileExpr Value [Expression] deriving (Show, Eq)

data Value = AddValue Value Value
           | MultValue Value Value
           | SubValue Value Value
           | NumValue Int
           | StringValue String
           | VarValue String deriving (Show, Eq)

