module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar conforme a sugestao abaixo, 
   mas a sua definicao (corpo) pode ficar a mesma
   executeP :: RContext -> Program  -> Either ErrorMessage RContext
-}
executeP :: RContext -> Program  -> Either ErrorMessage RContext -- Seguindo a sugestão.
executeP context (Prog stm) = execute context stm
   

{- Dica: o tipo de execute deve mudar para 
 execute :: RContext -> Stm -> Either ErrorMessage RContext   
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos 
 serao afetados
-}
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
   -- SAss id exp -> update context (getStr id) (eval context exp)
   SBlock [] -> Right context
   -- SBlock (s:stms) -> execute (execute context s) (SBlock stms) 
   -- SWhile exp stm -> if ( (eval context exp) /= 0) 
   --                    then execute (execute context stm) (SWhile exp stm)
   --                    else context

   SAss id exp -> let value = eval context exp in
      case value of
         Left msg -> Left msg
         Right integer -> Right (update context (getStr id) integer)

   SBlock (s:stms) -> let result = execute context s in
      case result of
         Left msg -> Left msg
         Right newcontext -> execute newcontext (SBlock stms)

   SWhile exp stm -> let condition = eval context exp in
      case condition of
         Left msg -> Left msg
         Right value -> if ( value /= 0)
                        then let result = execute context stm in
                           case result of
                              Left msg -> Left msg
                              Right newcontext -> execute newcontext (SWhile exp stm)
                        else Right context


{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
   --  EAdd exp0 exp  -> eval context exp0 + eval context exp
   --  ESub exp0 exp  -> eval context exp0 - eval context exp
   --  EMul exp0 exp  -> eval context exp0 * eval context exp
   --  EDiv exp0 exp  -> eval context exp0 `div` eval context exp
   EInt n  -> Right n
   EVar id  -> Right (lookup context (getStr id))
{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of 
                    Right ve1 -> case eval context e2 of 
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: " 
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg  
                    Left msg -> Left msg  
    EInt n  ->  Right n 
-}                
   EAdd exp0 exp -> let val0 = eval context exp0
                        val1 = eval context exp in
                           if ((noError val0) && (noError val1))
                              then Right ((getValue val0) + (getValue val1))
                              else Left (getErrorMessage [val0, val1])
   ESub exp0 exp -> let val0 = eval context exp0
                        val1 = eval context exp in
                           if ((noError val0) && (noError val1))
                              then Right ((getValue val0) - (getValue val1))
                              else Left (getErrorMessage [val0, val1])
   EMul exp0 exp -> let val0 = eval context exp0
                        val1 = eval context exp in
                           if ((noError val0) && (noError val1))
                              then Right ((getValue val0) * (getValue val1))
                              else Left (getErrorMessage [val0, val1])
   EDiv exp0 exp -> let val0 = eval context exp0
                        val1 = eval context exp in
                           if ((noError val0) && (noError val1))
                              then if ((getValue val1) == 0)
                                    then Left "divisao por 0"
                                    else Right ((getValue val0) `div` (getValue val1))
                              else Left (getErrorMessage [val0, val1])

noError :: Either ErrorMessage Integer -> Bool 
noError e = case e of
   Left _ -> False
   Right _ -> True

getErrorMessage :: [Either ErrorMessage Integer] -> ErrorMessage
getErrorMessage [] = "No Error"
getErrorMessage (e0:list) = case e0 of
   Left msg -> msg
   Right _  -> getErrorMessage list

getValue :: Either ErrorMessage Integer -> Integer
getValue (Right int) = int

-- Dica: voce nao precisa mudar o codigo a partir daqui
type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
