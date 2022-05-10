module Lib where

import Control.Monad

-- Buisness Logic Module  --

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

data Teacher = Teacher {
    teacherId :: Int,
    teacherName :: Name
} deriving Show

data Course = Course {
    courseId :: Int,
    courseTitle :: String,
    teacher :: Int
} deriving Show

students :: [Student]
students =  [(Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))] 

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
            ,Teacher 200 (Name "Susan" "Sontag")] 

courses :: [Course]
courses = [Course 101 "French" 100
          ,Course 201 "English" 200]

-- Functions -- 

startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

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

-- The join function joins two data sets on matching properties --

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1,d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs
