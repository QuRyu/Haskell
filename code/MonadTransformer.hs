import Control.Monad (guard) 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class (lift) 
import Control.Monad.Trans.Except
import Data.Functor.Identity (Identity, runIdentity) 
import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromJust)
import qualified Data.Map as Map 

parseInput :: MaybeT IO Int 
parseInput = do 
    lift $ putStrLn "type a number" 
    n <- lift $ getLine 
    guard (all isDigit n) 
    return $ sum $ zipWith (*) (iterate (*10) 1) $ 
        reverse $ map digitToInt n 

exp0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Val 3) (Val 3)) 

type Env = Map.Map Name Val
type Name = String 

data Exp = Val Integer
         | Var Name 
         | Add Exp Exp 
         | Lam Name Exp
         | App Exp Exp 
         deriving (Show)

data Val = IntVal Integer
         | FunVal Env Name Exp 
         deriving (Show) 

eval0 :: Env -> Exp -> Val 
eval0 env (Val i) = IntVal i 
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Add e1 e2) = let v1 = eval0 env e1 
                            v2 = eval0 env e2 
                        in case (v1, v2) of 
                            (IntVal a, IntVal b) -> IntVal (a + b) 
                            _                    -> v1 
eval0 env (Lam n exp) = FunVal env n exp 
eval0 env (App e1 e2) = let v1 = eval0 env e1 
                            v2 = eval0 env e2 
                        in case v1 of 
                            FunVal e n exp -> eval0 (Map.insert n v2 e) exp


type Eval1 a = Identity a 
runEval1 :: Exp -> Val
runEval1 exp = runIdentity $ eval1 Map.empty exp

eval1 :: Env -> Exp -> Eval1 Val 
eval1 env (Val i) = return $ IntVal i 
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Add e1 e2) = do v1 <- eval1 env e1 
                           v2 <- eval1 env e2 
                           case (v1, v2) of 
                              (IntVal a, IntVal b) -> return $ IntVal (a+b)
eval1 env (Lam n exp) = return $ FunVal env n exp 
eval1 env (App e1 e2) = do v1 <- eval1 env e1 
                           v2 <- eval1 env e2 
                           case v1 of 
                              FunVal e n exp -> eval1 (Map.insert n v2 e) exp


type Eval2 a = ExceptT String Identity a 
runEval2 :: Exp -> Either String Val 
runEval2 exp = runIdentity $ runExceptT $ eval2 Map.empty exp


eval2 :: Env -> Exp -> Eval2 Val
eval2 env (Val i) = return $ IntVal i 
eval2 env (Var n) = let v = Map.lookup n env 
                    in case v of 
                        Just v' -> return v' 
                        _       -> throwE $ "Rariable " ++ n ++ " is not in scope"
eval2 env (Add e1 e2) = do v2 <- eval2 env e1 
                           v2 <- eval2 env e2 
                           case (v2, v2) of 
                              (IntVal a, IntVal b) -> return $ IntVal (a+b)
                              _                    -> throwE $ "Expression \"add " ++ show e1 
                                                                ++ " " ++ show e2 ++ "\" is wrong"
eval2 env (Lam n exp) = return $ FunVal env n exp 
eval2 env (App e1 e2) = do v1 <- eval2 env e1 
                           v2 <- eval2 env e2 
                           case v1 of 
                              FunVal e n exp -> eval2 (Map.insert n v2 e) exp
                              _              -> throwE $ "Expression \"" ++ show e1 ++ "is not a lambda"
                                                            ++ " expression"


