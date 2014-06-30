{-

A basic interpreter for a purely functional subset of Scheme named SkimScheme.
Part of this interpreter has been derived from the "Write Yourself a Scheme in
48 Hours - An Introduction to Haskell through Example", by Jonathan Tang. It
does not implement a number of Scheme's constructs. Moreover, it uses a
different approach to implement mutable state within the language.

The name "SkimScheme" refers to the stripped down nature of this interpreter.
According to the New Oxford American Dictionary, "skim" can mean:

(as a verb) ... read (something) quickly or cursorily so as to note only
the important points.

(as a noun) ... an act of reading something quickly or superficially. 

"skimmed/skim milk" is milk from which the cream has been removed. 

The name emphasizes that we do not want to cover the entire standard, small as
it may be. Instead, we want to focus on some of the important aspects, taking a
language implementer's point of view, with the goal of using it as a teaching
tool. Many, many, many aspects of Scheme standards are not covered (it does not
even support recursion!).

Written by Fernando Castor
Started at: August 28th 2012
Last update: December 17th 2012

-}

module Main where
import System.Environment
import Control.Monad
import Data.Map as Map
import LispVal
import SSParser
import SSPrettyPrinter

-----------------------------------------------------------
--                      INTERPRETER                      --
-----------------------------------------------------------
eval :: StateT -> LispVal -> StateTransformer LispVal
eval env val@(String _) = return val
eval env val@(Atom var) = stateLookup env var 
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "begin":[v])) = eval env v
eval env (List (Atom "begin": l: ls)) = (eval env l) >>= (\v -> case v of { (error@(Error _)) -> return error; otherwise -> eval env (List (Atom "begin": ls))})
eval env (List (Atom "begin":[])) = return (List [])
eval env lam@(List (Atom "lambda":(List formals):body:[])) = return lam
eval env letArgs@(List (Atom "let":(List bindings):body:[])) = lispLet env bindings body
-- The following line is slightly more complex because we are addressing the
-- case where define is redefined by the user (whatever is the user's reason
-- for doing so. The problem is that redefining define does not have
-- the same semantics as redefining other functions, since define is not
-- stored as a regular function because of its return type.
eval env (List (Atom "define": args)) = maybe (define env args) (\v -> return v) (Map.lookup "define" env)
eval env (List (Atom "if":cond:consequent:alternate:[])) = eval env cond >>= (\t -> ifThenElse env (t:consequent:alternate:[]))
eval env (List (Atom "set!": args)) = maybe (set env args) (\v -> return v) (Map.lookup "set!" env)
eval env (List (Atom "define-struct":(Atom id):attributes:[])) = eval env attributes >>= classDeclaration env id
eval env (List (Atom "list-comp":var:list:result:condition:[])) = listComp env (var:list:result:condition:[])

eval env (List (Atom "new":(Atom class_name):(Atom id):attributes:[])) = ST (\s t -> let (ST f)                           = stateLookup env class_name
                                                                                         (class_body, s0, l0)             = f s t
                                                                                         (ST g)                           = eval env attributes
                                                                                         (attr, s1, l1)                   = g s0 l0
                                                                                         (ST obj)                         = newObject env class_body id attr
                                                                                         (result, s2, l2)                 = obj s1 l1
                                                                                      in (result, s2, l2)
                                                                            )

eval env (List (Atom "applyMethod":class_name:(Atom method):args:[])) = eval env class_name >>= \c -> getAttribute method c >>= defineLocal env method >> eval env args >>= (\t -> objectApply env c method [t])
eval env (List (Atom "getAttribute":class_name:(Atom attribute):[])) = eval env class_name >>= getAttribute attribute
eval env (List (Atom "setAttribute":class_name:(Atom attribute):value:[])) = eval env class_name >>= \c -> eval env value >>= setAttribute env c attribute class_name

eval env (List (Atom func : args)) = mapM (eval env) args >>= apply env func 
eval env (List list) = return (List list)
eval env (Class lisp_class) = return (Class lisp_class) 
eval env (Error s)  = return (Error s) 
eval env form = return (Error ("Could not eval the special form: " ++ (show form)))

stateLookup :: StateT -> String -> StateTransformer LispVal
stateLookup env var = ST $ 
  (\s t -> 
    (maybe (Error "variable does not exist.") id (Map.lookup var (union (union t s) env)), s, t)
  )

-- Because of monad complications, define is a separate function that is not
-- included in the state of the program. This saves  us from having to make
-- every predefined function return a StateTransformer, which would also
-- complicate state management. The same principle applies to set!. We are still
-- not talking about local definitions. That's a completely different
-- beast.
define :: StateT -> [LispVal] -> StateTransformer LispVal
define env [(Atom id), val] = defineGlobal env id val
define env [(List [Atom id]), val] = defineGlobal env id val
-- define env [(List l), val]                                       
define env args = return (Error "wrong number of arguments")

-- Defines a global variable.
defineGlobal :: StateT -> String -> LispVal -> StateTransformer LispVal
defineGlobal env id val = 
  ST (\s t -> let (ST f)                       = eval env val
                  (result, newState, newLocal) = f s t
              in (result, (insert id result newState), newLocal)
     )

-- Defines a local variable.
defineLocal :: StateT -> String -> LispVal -> StateTransformer LispVal
defineLocal env id val = 
  ST (\s t -> let (ST f)                       = eval env val
                  (result, newState, newLocal) = f s t
              in (result, newState, (insert id result newLocal))
     )

-- The maybe function yields a value of type b if the evaluation of 
-- its third argument yields Nothing. In case it yields Just x, maybe
-- applies its second argument f to x and yields (f x) as its result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply :: StateT -> String -> [LispVal] -> StateTransformer LispVal
apply env func args =  
                  case (Map.lookup func env) of
                      Just (Native f)  -> return (f args)
                      otherwise -> 
                        (stateLookup env func >>= \res -> 
                          case res of 
                            List (Atom "lambda" : List formals : body:l) -> lambda env formals body args                              
                            otherwise -> return (Error "not a function.")
                        )

-- The lambda function is an auxiliary function responsible for
-- applying user-defined functions, instead of native ones. We use a very stupid 
-- kind of dynamic variable (parameter) scoping that does not even support
-- recursion. This has to be fixed in the project.
lambda :: StateT -> [LispVal] -> LispVal -> [LispVal] -> StateTransformer LispVal
lambda env formals body args = 
  let dynEnv = Prelude.foldr (\(Atom f, a) m -> Map.insert f a m) env (zip formals args)
  in  eval dynEnv body

-- Initial environment of the programs. Maps identifiers to values. 
-- Initially, maps function names to function values, but there's 
-- nothing stopping it from storing general values (e.g., well-known
-- constants, such as pi). The initial environment includes all the 
-- functions that are available for programmers.
environment :: Map String LispVal
environment =   
            insert "number?"        (Native predNumber)
          $ insert "boolean?"       (Native predBoolean)
          $ insert "list?"          (Native predList)
          $ insert "eqv?"           (Native eqv)
          $ insert "lt?"            (Native lessThan)
          $ insert "="              (Native equalsTo)
          $ insert ">"              (Native greaterThan)
          $ insert "<"              (Native lessThan)
          $ insert "<="             (Native lessThanOrEq)
          $ insert ">="             (Native greaterThanOrEq) 
          $ insert "+"              (Native numericSum) 
          $ insert "*"              (Native numericMult) 
          $ insert "-"              (Native numericSub)
          $ insert "/"              (Native intDiv)
          $ insert "mod"            (Native intMod) 
          $ insert "car"            (Native car)           
          $ insert "cdr"            (Native cdr)
          $ insert "append"         (Native append)
          $ insert "cons"           (Native cons)          
            empty

type StateT = Map String LispVal

-- StateTransformer is a data type that embodies computations
-- that transform the state of the interpreter (add new (String, LispVal)
-- pairs to the state variable). The ST constructor receives a function
-- because a StateTransformer gets the previous state of the interpreter 
-- and, based on that state, performs a computation that might yield a modified
-- state (a modification of the previous one). 
data StateTransformer t = ST (StateT -> StateT -> (t, StateT, StateT)) -- adding two scopes (local and global)

instance Monad StateTransformer where
  return x = ST (\s t -> (x, s, t)) -- s is the global scope, while t is the local scope.
  (>>=) (ST m) f = ST (\s t -> let (v, newS, newT) = m s t
                                   (ST resF) = f v
                               in  resF newS newT
                      )
    
-----------------------------------------------------------
--          HARDWIRED PREDEFINED LISP FUNCTIONS          --
-----------------------------------------------------------

-- Includes some auxiliary functions. Does not include functions that modify
-- state. These functions, such as define and set!, must run within the
-- StateTransformer monad. 

car :: [LispVal] -> LispVal
car [List (a:as)] = a
car [DottedList (a:as) _] = a
car ls = Error "invalid list."

cdr :: [LispVal] -> LispVal
cdr (List (a:as) : ls) = List as
cdr (DottedList (a:[]) c : ls) = c
cdr (DottedList (a:as) c : ls) = DottedList as c
cdr ls = Error "invalid list."

predNumber :: [LispVal] -> LispVal
predNumber (Number _ : []) = Bool True
predNumber (a:[]) = Bool False
predNumber ls = Error "wrong number of arguments."

predBoolean :: [LispVal] -> LispVal
predBoolean (Bool _ : []) = Bool True
predBoolean (a:[]) = Bool False
predBoolean ls = Error "wrong number of arguments."

predList :: [LispVal] -> LispVal
predList (List _ : []) = Bool True
predList (a:[]) = Bool False
predList ls = Error "wrong number of arguments."

numericSum :: [LispVal] -> LispVal
numericSum [] = Number 0
numericSum l = numericBinOp (+) l

numericMult :: [LispVal] -> LispVal
numericMult [] = Number 1
numericMult l = numericBinOp (*) l

numericSub :: [LispVal] -> LispVal
numericSub [] = Error "wrong number of arguments."
-- The following case handles negative number literals.
numericSub [x] = if onlyNumbers [x]
                 then (\num -> (Number (- num))) (unpackNum x)
                 else Error "not a number."
numericSub l = numericBinOp (-) l

-- We have not implemented division. Also, notice that we have not 
-- addressed floating-point numbers.

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = if onlyNumbers args 
                       then Number $ foldl1 op $ Prelude.map unpackNum args 
                       else Error "not a number."
                       
onlyNumbers :: [LispVal] -> Bool
onlyNumbers [] = True
onlyNumbers (Number n:ns) = onlyNumbers ns
onlyNumbers ns = False             
                       
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--- unpackNum a = ... -- Should never happen!!!!

-----------------------------------------------------------
--                      additionals                      --
-----------------------------------------------------------

-- eqv?
eqv :: [LispVal] -> LispVal
eqv ((String a):(String b):[]) = (Bool (a == b))
eqv ((Number a):(Number b):[]) = (Bool (a == b))
eqv ((List a):(List b):[])     = (listEqv a b)
eqv (a:b:[])                   = (Bool False)
eqv (_:[]) = (Error "wrong number of arguments in eqv?.")
eqv args@(_:_:_) = (Error ("wrong number of arguments in eqv?."++(show args)))

listEqv :: [LispVal] -> [LispVal] -> LispVal
listEqv [] [] = (Bool True)
listEqv [] ys = (Bool False)
listEqv xs [] = (Bool False)
listEqv (x:xs) (y:ys) = if tmp == True
                        then listEqv xs ys
                        else (Bool False)
    where (Bool tmp) = eqv (x:[y])  
--

-- set!
set :: StateT -> [LispVal] -> StateTransformer LispVal
set env [(Atom id), val]          = setVarValue env id val
set env [(List [(Atom id)]), val] = setVarValue env id val
set env args                      = return (Error ("wrong number of arguments in set! = "++(show args)))

setVarValue :: StateT -> String -> LispVal -> StateTransformer LispVal
setVarValue env id val = ST $
  (\s t -> let (ST m)          = eval env val
               (r, newS, newT) = m s t
           in if (id `member` newT)
              then (r, newS, (insert id r newT))
              else (r, (insert id r newS), newT)
  )
--

-- /
intDiv :: [LispVal] -> LispVal
intDiv [] = (Error "wrong number of arguments in /.")
intDiv args@((Number x):[]) = (Error ("wrong number of arguments in / = "++(show args)))
intDiv ((Number x):(Number y):[]) = (Number (div x y))
intDiv args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in / = "++(show args)))
intDiv xs = (Error "not a number.")
--

-- mod
intMod :: [LispVal] -> LispVal
intMod [] = (Error "wrong number of arguments in mod.")
intMod args@((Number x):[]) = (Error ("wrong number of arguments in mod = "++(show args)))
intMod ((Number x):(Number y):[]) = (Number (mod x y))
intMod args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in mod = "++(show args)))
intMod xs = (Error "not a number.")
--

-- =
equalsTo :: [LispVal] -> LispVal
equalsTo [] = (Error "wrong number of arguments in =.")
equalsTo args@((Number x):[]) = (Error ("wrong number of arguments in = = "++(show args)))
equalsTo ((Number x):(Number y):[]) = (Bool (x == y))
equalsTo args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in = = "++(show args)))
equalsTo xs = (Error "not a number.")
--

-- >
greaterThan :: [LispVal] -> LispVal
greaterThan [] = (Error "wrong number of arguments in >.")
greaterThan args@((Number x):[]) = (Error ("wrong number of arguments in > = "++(show args)))
greaterThan ((Number x):(Number y):[]) = (Bool (x > y))
greaterThan args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in > = "++(show args)))
greaterThan xs = (Error "not a number.")
--

-- <
lessThan :: [LispVal] -> LispVal
lessThan [] = (Error "wrong number of arguments in <.")
lessThan args@((Number x):[]) = (Error ("wrong number of arguments in < = "++(show args)))
lessThan ((Number x):(Number y):[]) = (Bool (x < y))
lessThan args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in < = "++(show args)))
lessThan xs = (Error "not a number.")
--

-- >=
greaterThanOrEq :: [LispVal] -> LispVal
greaterThanOrEq [] = (Error "wrong number of arguments in >=.")
greaterThanOrEq args@((Number x):[]) = (Error ("wrong number of arguments in >= = "++(show args)))
greaterThanOrEq ((Number x):(Number y):[]) = (Bool (x >= y))
greaterThanOrEq args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in >= = "++(show args)))
greaterThanOrEq xs = (Error "not a number.")
--

-- <=
lessThanOrEq :: [LispVal] -> LispVal
lessThanOrEq [] = (Error "wrong number of arguments in <=.")
lessThanOrEq args@((Number x):[]) = (Error ("wrong number of arguments in <= = "++(show args)))
lessThanOrEq ((Number x):(Number y):[]) = (Bool (x <= y))
lessThanOrEq args@((Number x):(Number y):xs) = (Error ("wrong number of arguments in <= ="++(show args)))
lessThanOrEq xs = (Error "not a number.")
--

-- append
append :: [LispVal] -> LispVal
append ((List a):(List b):[])       = (List (a++b))
append args@((List a):(List b):cs)  = (Error ("wrong number of arguments in 'append' = "++(show args)))
append args@((List a):[])           = (Error ("wrong number of arguments in 'append' = "++(show args)))
append xs                           = (Error "not lists.")
--

-- cons
cons :: [LispVal] -> LispVal
cons (a:(List b):[]) = (List (a:b))
cons xs = (Error ("invalid list construction = "++(show xs)))
--

-----------------------------------------------------------
--                      list-comp                        --
-----------------------------------------------------------

listCompAux :: StateT -> [LispVal] -> StateTransformer LispVal
listCompAux env ((Atom var):(List (x:xs)):result:condition:[]) = set env [(Atom var), x] >> 
                                                                 ST (\s t -> let (ST f)                       = eval env condition
                                                                                 ((Bool condVal), tmpS, tmpT) = f s t
                                                                                 (ST g)                       = eval env result
                                                                                 (eltVal, newS1, newT1)       = g s t
                                                                                 (ST h)                       = listCompAux env ((Atom var):(List xs):result:condition:[])
                                                                                 (List tmpList, newS, newT)   = h newS1 newT1
                                                                                 res                          = if (condVal == True)
                                                                                                                then (eltVal:tmpList)
                                                                                                                else tmpList
                                                                             in ((List res), s, t)
                                                                    )

listCompAux _ ((Atom var):(List []):result:condition:[]) = ST (\s t -> ((List []), s, t))
listCompAux _ xs = return (Error ("wrong number of arguments in list-comp = "++(show xs)))

listComp :: StateT -> [LispVal] -> StateTransformer LispVal
listComp env ((Atom var):list:result:condition:[]) = ST $
  (\s t -> let (ST f)                  = eval env list
               (listValue, tmpS, tmpT) = f s t
               (ST g)                  = defineLocal env var (Number 0) >> listCompAux env ((Atom var):listValue:result:condition:[])
               (r, newS, newT)         = g s t
           in (r, s, t)
  )

listComp env xs = return (Error ("wong number of arguments in list-comp = "++(show xs)))


-----------------------------------------------------------
--                       comments                        --
-----------------------------------------------------------
ignoreComments :: LispVal -> LispVal
ignoreComments (List as) = (List (eraseCmnt as))
ignoreComments xs = xs

eraseCmnt :: [LispVal] -> [LispVal]
eraseCmnt [] = []
eraseCmnt ((Atom "comment"):(String cm):ls) = eraseCmnt ls -- we ignore the string cm, i.e., the comment
eraseCmnt ((Atom func):args:body) = (Atom func):(ignoreComments args):(eraseCmnt body)
eraseCmnt ((List args):ls) = (ignoreComments (List args)):(eraseCmnt ls)
eraseCmnt (y:ys) = y:(eraseCmnt ys)

-----------------------------------------------------------
--                     control flow                      --
-----------------------------------------------------------
ifThenElse :: StateT -> [LispVal] -> StateTransformer LispVal
ifThenElse env ((Bool cond):consequent:alternate:[]) = if (cond == True)
                                                       then eval env consequent
                                                       else eval env alternate
ifThenElse env ((Bool cond):consequent:[])           = if (cond == True)
                                                       then eval env consequent
                                                       else return (Error "if without an else.")
ifThenElse env ((Error e):xs)                        = return (Error e)
ifThenElse env xs                                    = return (Error ("wrong number of arguments in a if-then-else = "++(show xs)))

-----------------------------------------------------------
--                          let                          --
-----------------------------------------------------------
lispLet :: StateT -> [LispVal] -> LispVal -> StateTransformer LispVal
lispLet env ((List ((Atom id):val:[])):[]) body = defineLocal env id val >> eval env body
lispLet env ((List ((Atom id):val:[])):xs) body = defineLocal env id val >> lispLet env xs body
lispLet _ _ _ = return (Error "wrong number of arguments in a let.")

-----------------------------------------------------------
--                    classes & objects                  --
-----------------------------------------------------------
classDeclaration :: StateT -> String -> LispVal -> StateTransformer LispVal
classDeclaration env id attr@(List at) = defineGlobal env id (Class (initAttr attr))
classDeclaration _ _ _ = return (Error "invalid class declaration!")

initAttr :: LispVal -> [(String, LispVal)]
initAttr (List []) = []
initAttr (List ((Atom name):l)) = (name, Null):(initAttr (List l))

newObject :: StateT -> LispVal -> String -> LispVal -> StateTransformer LispVal
newObject env (Class klass) id (List attributes) = defineGlobal env id (Class (createObjects klass attributes))
newObject _ _ _ _ = return (Error "class doesn't exist!")

createObjects :: [(String, LispVal)] -> [LispVal] -> [(String, LispVal)]
createObjects [] [] = []
createObjects ((name, default_val):as) (val:xs) = (name, val):(createObjects as xs)

getAttribute :: String -> LispVal -> StateTransformer LispVal
getAttribute name (Class klass) = return (search name klass)
getAttribute _ _ = return (Error ("invalid attribute name!"))

search :: String -> [(String, LispVal)] -> LispVal
search _ [] = Error ("attribute not found!")
search name ((id,val):as) | name == id = val
                          | otherwise = search name as

setAttribute :: StateT -> LispVal -> String -> LispVal -> LispVal -> StateTransformer LispVal
setAttribute env (Class myclass) attribute id value = set env [id, (Class newValue)]
    where changeValue [] _ _ = []
          changeValue ((attr, v):xs) name newV = if attr == name
                                                 then (attr, newV):xs
                                                 else (attr, v):(changeValue xs name newV)
          newValue = changeValue myclass attribute value

objectApply :: StateT -> LispVal -> String -> [LispVal] -> StateTransformer LispVal
objectApply env obj func args =  
                  case (Map.lookup func env) of
                      Just (Native f)  -> return (f args)
                      otherwise -> 
                        (stateLookup env func >>= \res -> 
                          case res of 
                            List (Atom "lambda" : List formals : body:l) -> objectLambda env obj formals body args                              
                            otherwise -> return (Error "not a function.")
                        )

objectLambda :: StateT -> LispVal -> [LispVal] -> LispVal -> [LispVal] -> StateTransformer LispVal
objectLambda env (Class obj) formals body args =
  let currentObj = [[(Atom ("this:"++attr), val) | (attr, val) <- obj]]++(zip formals args)
      dynEnv = Prelude.foldr (\(Atom f, a) m -> Map.insert f a m) env currentObj
  in  eval dynEnv body

-----------------------------------------------------------
--                     main FUNCTION                     --
-----------------------------------------------------------

showResult :: (LispVal, StateT, StateT) -> String
showResult (val, defs, locals) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer LispVal -> (LispVal, StateT, StateT)
getResult (ST f) = f empty empty -- we start with an empty state. 

main :: IO ()
main = do args <- getArgs
          program <- readFile $ (args !! 0)
          putStr $ showResult $ getResult $ eval environment $ ignoreComments $ readExpr $ program
