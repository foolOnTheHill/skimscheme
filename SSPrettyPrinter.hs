module SSPrettyPrinter(show) where
import LispVal

-----------------------------------------------------------
--                    PRETTY PRINTER                     --
-----------------------------------------------------------

-- Pretty-printing for LispVal values. 
instance Show LispVal where
  show (Null) = "null" 
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number num) = show num
  show (String str) = "\"" ++ str ++ "\""
  show (Atom name) = name
  show (List (Atom "lambda" : l)) = "lambda " ++ show (List l) 
  show (List l) = "(" ++ showListContents l ++ ")"
  show (DottedList h t) = "(" ++ showListContents h ++ " . " ++ show t ++ ")" 
  show (Class c) = "{ " ++ (showObject c) ++ "}" 
  show (Native p) = "<native procedure>"
  show (Error s) = s
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [LispVal] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ " " ++ (showListContents as)

showObject :: [(String, LispVal)] -> String
showObject [] = ""
showObject ((a, v):as) = (show a) ++ ": " ++ (show v) ++ "; " ++ (showObject as)