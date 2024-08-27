{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}



{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module DSL where

import Control.Monad.Writer
import Control.Monad.State
import Data.Int

import Data.Dynamic
import Data.IORef
import Data.Word
import Hlist (HList (HNil))
import ListSearch
--import Hlist (HList (HNil), hCreate, hAdd)
--import GHC.TypeLits (Symbol)
--import Data.Proxy


data Prog a where
  Return :: a -> Prog a
  (:>>=) :: Prog a -> (a -> Prog b) -> Prog b
  CMD    :: CMD a -> Prog a

instance Functor Prog where
  fmap f m = m >>= return . f

instance Applicative Prog where
  pure  = Return 
  (<*>) = ap

instance Monad Prog where
  (>>=)  = (:>>=)


data CMD a where
  -- References:
  InitRef :: Type a => Exp a -> CMD (Ref a)
  GetRef  :: Type a => Ref a -> CMD (Val a)
  SetRef  :: Type a => Ref a -> Exp a -> CMD ()

  -- Input/output:
  Read     :: CMD (Val Int32)
  Write    :: Exp Int32 -> CMD ()
  PrintStr :: String -> CMD ()

  -- Loops:
  For :: Exp Int32 -> (Val Int32 -> Prog ()) -> CMD ()

  -- Conditional
  If :: Exp Bool -> Prog () -> Prog() -> CMD ()

  -- Function calls

  Funcall :: Type a => Exp String -> [Exp a] -> CMD () 
  -- Example 2 (portfilter.c)

  Funprocess_packet :: HList a -> Word64 -> CMD () -- Int
  Funpfilter :: HList a -> CMD ()
  Funbpf_htons :: Word16 -> CMD ()
  Funreturn :: CMD ()

  -- Example 3 (xdp1_user.c)
  FunInt_Exit :: String -> Int -> CMD ()
  FunPoll_Stats :: String -> Int -> Int -> CMD ()
  FunUsage :: String -> Char -> CMD ()


data Exp a where
  Var :: Type a => VarId -> Exp a
  Lit :: Type a => a -> Exp a
  Add :: (Num a, Type a) => Exp a -> Exp a -> Exp a
  Mul :: (Num a, Type a) => Exp a -> Exp a -> Exp a
  Not :: Exp Bool -> Exp Bool
  EEq  :: Type a => Exp a -> Exp a -> Exp Bool 
  

instance (Num a, Type a) => Num (Exp a) where
  fromInteger = Lit . fromInteger
  (+) = Add
  (*) = Mul



class CType a 

instance CType Int32
instance CType [Char]

class    (Eq a, Ord a, Show a, CType a) => Type a
instance (Eq a, Ord a, Show a, CType a) => Type a


data Val a
  = ValRun a       -- Concrete value
  | ValComp VarId  -- Symbolic value

-- Variable identifier
type VarId = String


data Ref a
  = RefRun (IORef a)  -- Concrete reference
  | RefComp VarId     -- Symbolic reference


interpret :: Monad m
          => (forall a . CMD a -> m a)
          -> Prog b -> m b
interpret _ (Return a) = return a
interpret int (p :>>= k) = interpret int p >>= interpret int . k
interpret int (CMD cmd)  = int cmd


runIO :: Prog a -> IO a
runIO = interpret runCMD

runCMD :: CMD a -> IO a
runCMD (InitRef a)           = RefRun <$> newIORef (evalExp a)
runCMD (GetRef (RefRun r))   = ValRun <$> readIORef r
runCMD (SetRef (RefRun r) a) = writeIORef r (evalExp a)
runCMD Read                  = ValRun . read <$> getLine
runCMD (Write a)             = putStr $ show $ evalExp a
runCMD (PrintStr s)          = putStr s
runCMD (For n body)          =
    mapM_ (runIO . body . ValRun) [0 .. evalExp n - 1]
runCMD (If condition exp1 exp2) = do
  if evalExp condition then runIO exp1 else runIO exp2 
runCMD _ = error "Impossible! Invalid program construction"

evalExp :: Exp a -> a 
evalExp (Lit a)   = a
evalExp (Add a b) = evalExp a + evalExp b
evalExp (Mul a b) = evalExp a * evalExp b
evalExp (Not a)   = not $ evalExp a
evalExp (EEq a b)  = evalExp a == evalExp b
evalExp _ = error "Impossible! Invalid program construction"

-- smart constructors 

initRef :: Type a => Exp a -> Prog (Ref a)
initRef = CMD . InitRef

setRef :: Type a => Ref a -> Exp a -> Prog () 
setRef r a = CMD (SetRef r a)

getRef :: Type a => Ref a -> Prog (Exp a)
getRef = fmap valToExp . CMD . GetRef


valToExp :: Type a => Val a -> Exp a
valToExp (ValRun a)  = Lit a
valToExp (ValComp v) = Var v

readInput :: Prog (Exp Int32)
readInput = valToExp <$> CMD Read

writeOutput :: Exp Int32 -> Prog ()
writeOutput = CMD . Write

printStr :: String -> Prog ()
printStr = CMD . PrintStr

for :: Exp Int32 -> (Exp Int32 -> Prog ()) -> Prog ()
for n body = CMD $ For n (body . valToExp)

-- If with else
if1 :: Exp Bool -> Prog () -> Prog () -> Prog ()
if1 condition exp1 exp2 = CMD (If condition exp1 exp2)
 

modifyRef :: Type a => Ref a -> (Exp a -> Exp a) -> Prog ()
modifyRef r f = setRef r . f =<< getRef r

-- example program

sumInput :: Prog ()
sumInput = do
  r <- initRef 0
  printStr "Please enter 4 numbers\n"
  for 10 $ \ _ -> do
    n <- readInput
    modifyRef r (+n)
  printStr "The sum of your numbers is "
  s <- getRef r
  writeOutput s
  printStr ".\n"

factorial :: Prog () 
factorial = do 
  r <- initRef 1 
  printStr "Enter the number:"
  n <- readInput 
  printStr "\n"
  for n $ \ i -> do 
    modifyRef r (* (i + 1))
  printStr "The result factorial is:"
  s <- getRef r 
  writeOutput s 
  printStr "\n"


-- Code generation monad
type Code = WriterT [Stmt] (State Unique)

type Stmt   = String
type Unique = Integer

runCode :: Code a -> String
runCode = unlines . flip evalState 0 . execWriterT . indent

-- Emit a statement in the generated code
stmt :: Stmt -> Code ()
stmt s = tell [s]

-- Modify a code generator by indenting the generated code
indent :: Code a -> Code a
indent = censor $ map ("    " ++)

-- Code generation of instructions
codeCMD :: CMD a -> Code a
codeCMD (InitRef a) = do
  r <- freshRef
  stmt $ unwords ["int ", show r, "=" , showExp a, ";"]
  return r
codeCMD (GetRef r) = do
  v <- freshVar
  stmt $ unwords ["int ", show v, "=", show r, ";"]
  return v
codeCMD (SetRef r a) = stmt $ unwords [show r, "=" , showExp a, ";"]
codeCMD Read = do
   v <- freshVar
   stmt $ unwords ["int ", show v, ";"]
   stmt $ unwords ["scanf(\"%d\", &", show v , ");"]
   return v
codeCMD (Write a)    = stmt $ unwords ["printf(\"%d\",", showExp a,");"]
codeCMD (PrintStr s) = stmt $ unwords ["printf(", show s, ");"]
codeCMD (For n body) = do
   i <- freshVar
   stmt $ unwords ["for(int", show i, " = 0;", show i , "<", showExp n, ";" , show i, "++){"]
   indent $ code (body i)
   stmt "}"
codeCMD (If condition exp1 exp2) = do
    stmt $ "if (" ++ showExp condition ++ ") {"
    indent $ code exp1
    stmt "}"
    stmt "else {"
    indent $ code exp2
    stmt "}"
codeCMD (Funprocess_packet ctx off) = do
  stmt $ unwords ["process_packet(", show off, ");"]




instance Show (Val a) where show (ValComp a) = a
instance Show (Ref a) where show (RefComp r) = r
instance Show a => Show (Exp a)


bracket :: String -> String
bracket s = "(" ++ s ++ ")"


showExp :: Exp a -> String
showExp (Var v)   = v
showExp (Lit a)   = show a
showExp (Add a b) = bracket (showExp a ++ " + " ++ showExp b)
showExp (Mul a b) = showExp a ++ " * " ++ showExp b
showExp (Not a)   = "!" ++ showExp a
showExp (EEq a b) = showExp a ++ " == " ++ showExp b


-- Generate a unique symbolic value
freshVar :: Type a => Code (Val a)
freshVar = ValComp <$> unique "v"

-- Generate a unique reference
freshRef :: Type a => Code (Ref a)
freshRef = RefComp <$> unique "r"

-- Generate a unique identifier
unique :: String -> Code VarId
unique base = do
  u <- get
  put (u + 1)
  return (base ++ show u)


code :: Prog a -> Code a
code = interpret codeCMD

-- Generate C code from Prog
generateC :: Prog a -> String
generateC prog = runCode (code prog)


-- process_packet :: Exp (HList a) -> Exp Integer -> Prog ()
-- process_packet ctx off = do

--   data_end <- initRef (Lit $ toDyn 0)
--   data_ptr <- initRef (Lit $ toDyn 0)

--   tcp <- initRef (Lit $ HNil)
--   iph <- initRef (Lit $ HNil)
--   payload_len <- initRef (Lit 0)
--   protocol <- initRef (Lit 0)


--   --Como o modifyRef só aceita funções parciais do tipo (Exp a -> Exp a),
--   --precisamos guardar o valor de data_ptr primeiro, para depois aplicar a função parcial com o off
--   iph <- getRef data_ptr
--   modifyRef iph ( `Add` off)

 