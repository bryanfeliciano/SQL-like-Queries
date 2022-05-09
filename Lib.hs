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

