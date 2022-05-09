module Lib where

import Control.Monad

-- Buisness Logic  --

data Name = Name {
    firstName :: String,
    lastName :: String
}

instance Show Name where
    show (Name first last) = mconcat [first, " " , last]

data GradeLevel = Freshman | Sophmore | Junior | Senior deriving (Show,Eq,Enum,Ord)

data Student  = Student {
    studentId :: Int,
    gradeLevel :: GradeLevel,
    studentName :: Name
} deriving Show

students :: [Student]
students =  [(Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))] 

-- Select allows you to target a property from your students list for example --

_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
    val <- vals
    return (prop val)

-- Where is a bit more complex (you're using guard imported from 
-- control monad to filter out parameters tha tdont pass your test) -- 

_where :: (a -> Bool) -> [a] -> [a]
_where test vals = do
    val <- vals
    guard (test val)
    return val
