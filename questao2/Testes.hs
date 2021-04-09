import AbsLI
import Interpreter

prog1 = Prog (SBlock [SAss (Ident "w") (EStr "hello"),
                      SAss (Ident "v") (EStr "world"),
                      SAss (Ident "a") (ECon (EVar (Ident "w")) (EVar (Ident "v")))])
                      
{-

 {
  w = "hello";
  v = "world";
  a = w ++ v;
 }

-}                    
                      
-- executeP :: RContext -> Program  -> RContext
testCase1 = executeP [] prog1  ==  [("w", ValorStr "hello"),("v",ValorStr "world"),("a",ValorStr "helloworld")]   


-----------------------------------------
-----------------------------------------


{--
 {
  a = true;
  b = false;
  e = true;
  d = ! a || b && e;
  f = a && b || ! e;
 }

--}

prog2 = Prog (SBlock [SAss (Ident "a") ETrue,
                      SAss (Ident "b") EFalse, 
                      SAss (Ident "e") ETrue, 
                      SAss (Ident "d") (EOr (ENot (EVar (Ident "a"))) 
                                            (EAnd (EVar (Ident "b")) (EVar (Ident "e")))),
                      SAss (Ident "f") (EOr (EAnd (EVar (Ident "a")) (EVar (Ident "b"))) 
                                            (ENot (EVar (Ident "e"))))
                     ])

testCase2= executeP [] prog2 == [("a",ValorBool True),("b",ValorBool False),("e",ValorBool True),
                                  ("d",ValorBool False),("f", ValorBool False)]     



-- uma condicao necessaria (mas nao suficiente) da implementacao eh que o valor de testSuite seja "True"
testSuite = foldl (&&) True [testCase1,testCase2]                  