{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module Lib where
import Control.Monad
import Control.Applicative

-- Buisness Logic Module  --

-- Data Types --

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

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)| HINQ_ (m a -> m b) (m a)

data Enrollment = Enrollment
                            { student :: Int
                            , course :: Int } deriving Show

-- Variables --

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

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101)
              ,(Enrollment 2 101)
              ,(Enrollment 2 201)
              ,(Enrollment 3 101)
              ,(Enrollment 4 201)
              ,(Enrollment 4 101)
              ,(Enrollment 5 101)
              ,(Enrollment 6 201) ]

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
-- Input and Output example -> GHCi> _join teachers courses teacherId teacher
-- [(Teacher {teacherId = 100, teacherName = Simone De Beauvior},
-- Course {courseId = 101, courseTitle = "French", teacher = 100}),
-- (Teacher {teacherId = 200, teacherName = Susan Sontag},Course
-- {courseId = 201, courseTitle = "English", teacher = 200})]

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1,d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

-- What you now want is to structure these as if they're SQL queries --

_hinq selectQuery joinQuery whereQuery = (\joinData -> (\whereResult -> selectQuery whereResult)(whereQuery joinData)) joinQuery

finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") .courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)
                         
possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

-- Making a generic HINQ type to represent the queries you're interested in running (Line 35)--
-- Then creating functions to execute HINQ queries --

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") .courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers

-- Example output GHCi> runHINQ query2
-- [Simone De Beauvior,Susan Sontag]

maybeQuery1 :: HINQ Maybe (Teacher,Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst)) (_join possibleTeacher possibleCourse teacherId teacher) (_where ((== "French") .courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher,Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((== "French") .courseTitle . snd))

studentEnrollmentsQ = HINQ_ (_select (\(st,en) -> (studentName st, course en))(_join students enrollments studentId student))

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

-- Example of output --
-- GHCi> studentEnrollments
-- [(Audre Lorde,101),(Leslie Silko,101),(Leslie Silko,201),
-- (Judith Butler,101),(Guy Debord,201),(Guy Debord,101),
-- (Jean Baudrillard,101),(Julia Kristeva,201)]

englishStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrollments courses snd courseId)
                        (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ 

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
    where courseQuery = HINQ (_select (fst . fst))
                             (_join studentEnrollments courses snd courseId)
                             (_where ((== courseName) . courseTitle . snd))