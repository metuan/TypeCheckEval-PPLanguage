-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module Tests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
 
tests =
  [ Test "inc"      (SrcString "input x in x + 1")  (Eval [42] (Value 43))
  , Test "noInput"      (SrcString "42 + 0") (Eval [] (Value 42))
  , Test "undefVar" (SrcString "x") TypeError
  , Test "simplifyOperators" (SrcString " input x in x + 10 - - -  1") (Eval [1] (Value 10))
  , Test "ifStatement"       (SrcString "input x y in if x >= 1 then 144 else y * 5") (Eval [2, 3] (Value 144))
  , Test "negationSimplifiedToTrue" (SrcString "input x in if not not true then x * 1 else x div 1") (Eval [1] (Value 1))
  , Test "simpleInputIn"       (SrcString "input x y in x * 1 - y mod 4") (Eval [1,12] (Value 1))
  , Test "errorWithBoolInIf"     (SrcString "input x in if 11 then x * 22 else x * 33") TypeError
  , Test "evaluateBoolExpression" (SrcString "if true or false or false or true then 1 else 0") (Eval [] (Value 1))
  , Test "checkBoolInIf"      (SrcString "input x in if x div 2 >= 1 then x * 3 else x div 3") (Eval [6] (Value 18))
  , Test "undefinedVariables"      (SrcString "input x y in x*y + z") TypeError
  , Test "boolErrorInLet"      (SrcString "input x y z in let x  = y in z + x") (Eval [1,2,3] (Value 5))
  , Test "quiteBadTypes" (SrcString "true div true") TypeError
  , Test "prosteSkrypty" (SrcString "2") (Eval [] (Value 2))
  , Test "drugiElementPary" (SrcString "input a in let x = (a+1, a+10) in snd x") (Eval [5] (Value 15))
  , Test "errWithListConstructor" (SrcString "let x = [1,2,3]:int in 1") TypeError
  , Test "notValidReturnThen"      (SrcString "input x in if true then true else x * 2") TypeError
  , Test "properIfLazy" (SrcString "input x in if x >= 0 then x else x div 0") (Eval [1] (Value 1))
  , Test "quiteHardIfExpr" (SrcString "input a b c in if a < 0 and b < 0 then c else a + b") (Eval [-1, -1, 1] (Value 1))
  , Test "multipleSums"      (SrcString "input x y z a b in x + y + z + a + b") (Eval [1,11,11,11,11] (Value 45))
  , Test "thereIsUnusedVar" (SrcString "input x y in x*2") (Eval [1,2] (Value 2))
  , Test "scriptInterpretLanguage" (SrcString "1 + 10 + 100") (Eval [] (Value 111))
  , Test "orAppliedToNumberAndBool" (SrcString "input a in if a or true then a else a") TypeError 
  , Test "multipleSubstractions"      (SrcString "input a b c d in a - b - c - d") (Eval [3,1,1,1] (Value 0))
  , Test "errorWithBoolInIf" (SrcString "if 1 then 2 else 3") TypeError
  , Test "priorityOperators"      (SrcString "input a b c in a * b - c") (Eval [2,2,3] (Value 1))
  , Test "lazinessOfMatcher" (SrcString "let x = [1,2,3]:int list in match x with [] -> 1 div 0 | y :: ys -> y") (Eval [] (Value 1))
  , Test "constructorEmptyList" (SrcString "let x = []:int list in match x with [] -> 0 | y :: ys -> y div 0") (Eval [] (Value 0))
  , Test "wrongTypesOConstructors" (SrcString "let x = [1,2] : int list in let y = [3] : int list in let a = x :: y in match a with [] -> 1 | b :: bs -> 2") TypeError
  , Test "firstTryOfMatcher" (SrcString "let x = [151,2,3]: int list in match x with [] -> 0 | y :: ys -> y") (Eval [] (Value 151))
  , Test "eagerOfPairProject" (SrcString "input a in let x = (a div 0, a) in snd x") (Eval [5] RuntimeError)
  , Test "canWeTryPustaMatcher" (SrcString "let li = [] : int list in match li with [] -> 1 | x :: xs -> x") (Eval [] (Value 1))
  , Test "easyFuncTried" (SrcString "fun add5(x : int) : int = x + 5 input a in let a = 20 in add5(a)") (Eval [1] (Value 25))
  , Test "binaryWithUnaryOperator" (SrcString "10 + -7") (Eval [] (Value 3))
  , Test "listOfBooleanValues" (SrcString "let x = true in let y = [false,false] : bool list in let a = x :: y in match a with [] -> 0 | b :: bs -> if b then 1 else 2") (Eval [] (Value 1))       
  , Test "alwaysReturnInput"      (SrcString "input x in if true then x else x mod 2") (Eval [1] (Value 1))
  , Test "usingUndefinedVariable" (SrcString "input x y in x + y + z") TypeError
  , Test "mixedBoolAndIntegerOperator" (SrcString "input a in if - true then 1 else 0") TypeError
  , Test "nestedIfElse" (SrcString "input x in if if x >= 1 then true else false then if true then x * 3 else 42 + x else x") (Eval [1] (Value 3))
  , Test "letStatementNested" (SrcString "let x = 1 in let x = 2*x in let x = x + 5 in let x = 3*x in x + -1") (Eval [] (Value 20))
  , Test "negationIf" (SrcString "input x in if not false then x else -x") (Eval [1] (Value 1))
  , Test "muchInput" (SrcString "input q w e r t y u i o p a s d f g h j k l z x c v b n m in h + a + s + k + e + l + l") (Eval [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7] (Value 38))
  , Test "trickyLogic" (SrcString "if true then 21 else 37 div 0") (Eval [] (Value 21))
  , Test "unitValErr" (SrcString "()") TypeError
  , Test "SummingTrick" (SrcString "69 + -69") (Eval [] (Value 0))
  , Test "conditionalsNested" (SrcString "if (if 1 = 1 then false else true) and (if 1 = 1 then false else true) then 1 else 0") (Eval [] (Value 0))
  , Test "wrongFirst" (SrcString "input a b in fst a + b") TypeError
  , Test "gniazdoLambdy" (SrcString "let func1 = fn(x : bool) -> fn (y: int) -> fn (z: int) -> if x then y * z else y - z in func1(true)(3)(4)") (Eval [] (Value 12))
  , Test "wrongSecond" (SrcString "snd 1") TypeError
  , Test "klopotZArgLambdy" (SrcString "let fun1 = fn(x : bool) -> if x then 3 else 5 in fun1(5)") TypeError
  , Test "usedLocalVar" (SrcString "let fun1 = fn(x : int) -> x in x") TypeError
  , Test "trudLambady" (SrcString "fn(x: int) -> x + 2 + 3 - 4 - 15") TypeError
  , Test "lambdaWrongRes" (SrcString "let fun1 = fn(x : int) -> if x > 0 then true else false in fun1 10") TypeError
  , Test "badCons" (SrcFile "badCons.pp6") TypeError
  , Test "wartoscBoolWFunInt" (SrcFile "wrongApplication.pp6") TypeError
  , Test "mixedErrorsMatch" (SrcFile "mixedErr.pp6") TypeError
  , Test "sourceToFileDivisors" (SrcFile "ppProgramDivisors.pp6") (Eval[15] (Value 3))
  , Test "anotherSrc" (SrcFile "ppProgramDivisors.pp6") (Eval[18] (Value 6))
  , Test "firstElementsOfFib" (SrcFile "fibonacciPro.pp6") (Eval[1] (Value 1))
  , Test "tryAnotherFibNumbers" (SrcFile "fibonacciPro.pp6") (Eval [5] (Value 5)) 
  , Test "toSumUpPairInput" (SrcFile "summingInput.pp6") (Eval[1,2] (Value 3))
  , Test "zagniezdzoneFunkcje" (SrcFile "recursiveSign.pp6") (Eval [4] (Value 1))
  , Test "anonymousSumming" (SrcFile "anonimoweDodawanie.pp6") (Eval [1,1,1] (Value 6))
  , Test "gniazdoAnonimowejLogiki" (SrcFile "nestedLogic.pp6") (Eval [1,0] (Value 1))
  , Test "parowanieLambdaLet" (SrcFile "projekcjeiLambdy.pp6") (Eval [11, 12] (Value 11))
  , Test "unusedLambda" (SrcFile "nestesLambadas.pp6") (Eval [13] (Value 125))
  , Test "globalAnonim" (SrcFile "publicAnonLambda.pp6") (Eval [2] (Value 0))
  , Test "skryptLambda" (SrcFile "skrytaLambda.pp6") TypeError
  , Test "returnLambda" (SrcFile "funcInReturn.pp6") (Eval [(1024)] (Value 0))
  , Test "factorialOfN" (SrcFile "factorial.pp6") (Eval [5] (Value 120))
  , Test "ackermannYouShallNotPass" (SrcFile "ackermann.pp6") (Eval [2, 2] (Value (7)))
  , Test "projectionOfTwoTypes" (SrcFile "rzutowaniePary.pp6") (Eval [1,2] (Value 1))
  , Test "differentTypesPairs" (SrcFile "mixedPairs.pp6") (Eval [5] (Value 10))
  , Test "isEmptyList" (SrcFile "matchingList.pp6") (Eval [1,2,3,4] (Value 1))
  , Test "checkWhetherIsNumber" (SrcFile "matchingList.pp6") (Eval [1,1,1,1] (Value 1))
  , Test "matcherSkrypcix" (SrcFile "matcherSkrypt.pp6") (Eval [] (Value 111))
  , Test "unitFunctions" (SrcFile "fewFuncAndUnit.pp6") (Eval [0,0] (Value 1))
  , Test "defOfModulo" (SrcFile "funMod.pp6") (Eval [12, 3] (Value 0))
  , Test "resztaModJeden" (SrcFile "funMod.pp6") (Eval [12, 11] (Value 1))
  , Test "bladPary" (SrcFile "pairFailType.pp6") TypeError
  , Test "lengthOfTheList" (SrcFile "length.pp6") (Eval [1,1,1,1] (Value 4))
  , Test "baseAndExp" (SrcFile "power.pp6") (Eval [2,4] (Value 16))
  , Test "powerToCatchExcept" (SrcFile "power.pp6") (Eval [1024, 0] (Value 1))
  ]
