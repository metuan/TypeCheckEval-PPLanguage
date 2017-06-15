{-# LANGUAGE Safe #-}
module KarolRodzinski (typecheck, eval) where

import AST
import DataTypes


-- OSTATNIA PRACOWNIA --
-- Praca na rzecz tego jezyka PP, ktory rozszerzalismy co dwa tygodnie o cos nowego teraz przynosi prawdziwa
-- satysfkacje. Mamy jezyk, ktory posiada lambda-abstrakcje, wyrazenia matching list, operacje arytmetyczne,
-- funkcje. Stworzylismy narzedia do jego interpretacji i ewaulowania, a zatem to juz COS.   

-- Zadanie opieralo sie na napisaniu modulu eksportujacego dwie funkcje eval oraz typechechk.
-- W pliku dotyczacym pracowni mozemy dowiedziec sie jak wygladaja reguly wnioskowania dla tego jezyka.
-- Nasza praca sprowadzala sie do przepisania tych regul do Haskella. Trudnosciami mogly byc rozne zwracane 
-- przez typy funkcji eval, to znaczym nasz program nie zawsze zwraca int, to dzieje sie jedynie na koncu, 
-- ale w trakcie dzialania funkcja eval moze zwrocic rowniez wartosc typu bool, zatem przyjmujemy, iz
-- nasze dane tzn. Typy to: bool type, int type oraz error, a wartosci, to bool value, int value oraz nowa
-- wartosc DivisionByZero. Definiujemt rowniez kilka funkcji, ktore przyporzadkuja wartosci oraz typ kazdej zmiennej, 
-- rozszerzone funkcje typePrim oraz valuePrim, ktore uwzgledniaja wczesniej wspomniana trudnosc. Pobawilem sie takze
-- z systemem informowania o bledzie wykonania, dlatego typ Error moze miec argument w postaci Stringa, ktory
-- informuje nas z jaka trudnoscia spotkal sie program. Pozwala to na szybkie diagnozowanie bledow.


-- Dodatkowa trudnoscia, z ktora przyszlo nam sie zmierzyc w tym zadaniu sa funkcje, wyrazenia matching_list, pary oraz ich 
-- projekcje, funkcje_unit() a takze listy wyrazen typu bool oraz int. Nieodzowna pomoca w zadaniu okazal sie plik AST.hs dla tej 
-- pracowni, poniewaz bardzo dobrze ukazywal strukture abstrakcyjnego drzewa rozbioru poszczegolnych wyrazen, zatem bezposredni dostep
-- do skladowych np. funkcji nie wydawal sie juz taki trudny.


-- W tym zadaniu przyszlo nam sie zmierzyc z funkcjami anonimowymi, zmieniala sie takze troche regula aplikacji funkcji
-- z tego powodu musialem przebudowac rozwiazanie z poprzedniej pracowni, ktore dotyczylo funkcji. Koniec koncow zadanie polegalo,
-- jak zwykle, na przepisaniu regulek z pliku w SKOS, a co za tym idzie, akurat 6 czesc spowodowala dopisanie moze 20, 30 linijek kodu.

---------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------ROZWIAZANIE WRAZ Z OBJASNIENIAMI ----------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

-- Dwa nowe typy zdefiniowane na potrzeby rozwiazania tj. 
-- 1)TypyProgramu, ktory mowi nam, ze cos moze bym dobrym typem, ewentualnie zlym 
-- wraz z informacja (konstrukcja podobna do wyrazeniu typu Maybe Just).
-- 2)WartosciProgramy - bez zbednej filozofii, czyli co moze byc typem wynikowym naszego programu.
-- 3)W porownaniu do poprzedniego zadania pojawila sie nowa wartosc dotyczaca lamd oraz funkcji.

data TypyProgramu p = OKType Type | ErrType p String

data WartosciProgramu p = IntegerValue Integer | BooleanValue Bool | UnitValue | PairValue (WartosciProgramu p) (WartosciProgramu p) | ListValue [WartosciProgramu p] | LambdaValue p Var [(Var, WartosciProgramu p)] (Expr p) | FuncValue Var (Expr p) | DivisonByZero

---------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------TypeChecker--------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------
-- Mankamentem mogła okazać się zabawa z obsluga bledow, jednak jest ona dosc podobna do zeszlej pracowni, to znaczy bledy przekazujemy wyzej.
-- Pierwsza czesc to wlasciwie poprzednia pracownia, a jedynie wymogiem bylo dodanie funkcji typecheckPrim dla wyrazen dodanych na tej liscie.
-- Wystarczylo podazac za plikiem AST.hs oraz regulami z pliku ze SKOS. W porownanie do poprzedniej listy uproscilem jedynie system typw, to znaczy
-- ze jest OK albo Err, czyli powyzsza konstrukacja typu, ktora niejako nawiazauje do Maybe, Just.   

-- funkcja pomocnicza, ktora bezposrednio odnosi sie do pliku AST.hs, czyli wyszukiwanie funkcji w prorgamie poprzez odniesenie 
czyJestFun :: [FunctionDef p] -> Var -> Maybe (FunctionDef p)
czyJestFun [] name = Nothing
czyJestFun (f1:funDefinicje) name = 
    if funcName f1 == name 
        then Just f1 
        else czyJestFun funDefinicje name  

--- Dopisany kod na pracownie nr 6 ---
-- przegladamy liste funkcji w celu sprawdzenia typu poszczegolnych z nich, ale takze zachowania porzadku
checkListOfFunc :: [FunctionDef p] -> [FunctionDef p] -> TypeCheckResult p
checkListOfFunc srodowisko funDefinicje =
    let srodowiskoFun = map aux srodowisko where aux fun = (funcName fun, OKType (TArrow (funcArgType fun) (funcResType fun)))
    in case funDefinicje of
        [] -> Ok
        (function:rest) -> case typecheckPrim ((funcArg function, OKType (funcArgType function)):srodowiskoFun) (funcBody function) of
                    ErrType p mesg -> Error p mesg
                    OKType a -> if a == funcResType function 
                        then checkListOfFunc srodowisko rest 
                        else Error (funcPos function) ("Func err: Zly typ wynikowy funkcji")    

-- typ standardowej funkcji sprawdzajacej typ
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
-- definicja standardowej funkcji sprawdzajacej typ
typecheck funDefinicje vars input =
    case checkListOfFunc funDefinicje funDefinicje of
        Error p mesg -> Error p mesg
        _ -> let zmienneGood1 = map aux1 vars where
                    aux1 var = (var, OKType TInt)
             in let zmiennegoGood2 = map aux2 funDefinicje where 
                    aux2 fun = (funcName fun, OKType (TArrow (funcArgType fun) (funcResType fun)))
             in case typecheckPrim (zmiennegoGood2 ++ zmienneGood1) input of
                    OKType TInt -> Ok
                    OKType _ -> Error (getData input) ("TypeChecker Err: Zly typ")
                    ErrType p mesg -> Error p mesg

-- definicje funkcji przyporzadkowania typow
zmienneTyp :: p -> [(Var, TypyProgramu p)] -> Var -> TypyProgramu p
zmienneTyp p [] var = ErrType p ("Nieznana zmienna" ++  var)
zmienneTyp p ((value, posType):xs) var = 
    if value == var 
        then posType 
        else zmienneTyp p xs var                        

-- typ rozszerzonej funkcji sprawdzajacej typ
typecheckPrim :: [(Var, TypyProgramu p)] -> Expr p -> TypyProgramu p
-- definicja rozszerzonej funkcji sprawdzajacej typ
typecheckPrim vars (EVar p var) = zmienneTyp p vars var
typecheckPrim _ (ENum p _) = OKType TInt
typecheckPrim _ (EBool p _) = OKType TBool

-- typy wyrazen z operatorami unarnymi
typecheckPrim vars (EUnary p unop e1) =
    case typecheckPrim vars e1 of
        ErrType p mesg -> ErrType p mesg
        OKType TBool -> case unop of
            UNot -> OKType TBool
            UNeg -> ErrType p ("TypeCheck Err: Operator (-) przy wyrazeniu typu boolowskim")
        OKType TInt -> case unop of
            UNeg -> OKType TInt
            UNot -> ErrType p ("TypeCheck Err: Operator (not) przy wyrazeniu typu int")
        _ -> ErrType p ("TypeCheck Err: Operator unarny został zaaplikowany do niewlasciwego typu")

-- typy wyrazen z operatorai binarnymi 
typecheckPrim vars (EBinary p binop e1 e2) =
    let x = typecheckPrim vars e1
    in let y = typecheckPrim vars e2 
    in case (x, y) of
            (ErrType p1 m, _) -> ErrType p1 m
            (_, ErrType p1 m) -> ErrType p1 m
            (OKType TBool, OKType TBool) -> case binop of
                BAnd -> OKType TBool
                BOr -> OKType TBool
                _ -> ErrType p ("TypeCheck Err: Operator int przy bool")
            (OKType TInt, OKType TInt) -> case binop of
                BEq -> OKType TBool
                BNeq -> OKType TBool
                BLt -> OKType TBool 
                BGt -> OKType TBool
                BLe -> OKType TBool
                BGe -> OKType TBool
                BAdd -> OKType TInt 
                BSub -> OKType TInt
                BMul -> OKType TInt
                BDiv -> OKType TInt
                BMod -> OKType TInt
                _ -> ErrType p ("TypeCheck Err: Operator bool przy int")
            _ -> ErrType p ("TypeCheck Err: Rozne typy argumentow operatora")

-- typy wyrazen Let In
typecheckPrim vars (ELet p var e1 e2) =
    case typecheckPrim vars e1 of
        ErrType p1 m -> ErrType p1 m
        t -> typecheckPrim ((var,t):vars) e2

-- typy wyrazen If
typecheckPrim vars (EIf p e0 e1 e2) =
    case typecheckPrim vars e0 of 
        OKType TBool -> case (typecheckPrim vars e1, typecheckPrim vars e2) of
                (ErrType p2 m, _) -> ErrType p2 m
                (_, ErrType p2 m) -> ErrType p2 m
                (OKType t1, OKType t2) -> if t1 == t2 
                    then OKType t1 
                    else ErrType (getData e1) ("TypeCheck Err: Rozne typy wyrazen")
        ErrType p1 m -> ErrType p1 m
        _ -> ErrType p ("TypeCheck Err: zly typ warunku")

-- typy wyrazenia Unit()
typecheckPrim _ (EUnit _) = OKType TUnit
typecheckPrim vars (EPair p e1 e2) = 
    case typecheckPrim vars e1 of
        ErrType p1 m -> ErrType p1 m
        OKType t1 -> case typecheckPrim vars e2 of
            ErrType p2 m2 -> ErrType p2 m2
            OKType t2 -> OKType $ TPair t1 t2

-- typ projekcji fst
typecheckPrim vars (EFst p e1) = 
    case typecheckPrim vars e1 of
        ErrType p1 m -> ErrType p1 m
        OKType (TPair t1 _) -> OKType t1
        _ -> ErrType p ("TypeCheck Err: Blad przy projekcji first.")

-- typ projekcji snd
typecheckPrim vars (ESnd p e1) =
    case typecheckPrim vars e1 of
        ErrType p1 m -> ErrType p1 m
        OKType (TPair _ t2) -> OKType t2
        _ -> ErrType p ("TypeCheck Err: Blad przy projekcji second")

-- typ wyrazen ENil - pusta lista 
typecheckPrim vars (ENil p t) = case t of 
        TList _ -> OKType t
        _ -> ErrType p ("TypeCheck Err: Brak typu przy liscie.")

-- typ List
typecheckPrim vars (ECons p e1 e2) = 
    case typecheckPrim vars e2 of
        ErrType p1 mesg1 -> ErrType p1 mesg1
        OKType (TList t1) -> case typecheckPrim vars e1 of
            ErrType p2 mesg2 -> ErrType p2 mesg2
            OKType t2 -> if t1 == t2 
                then OKType (TList t1) 
                else ErrType p ("TypeCheck Err: Blad miedzy elemntami listy a jej typem.")
        _ -> ErrType p ("TypeCheck Err: Blad konstruktora")


-- typ wyrazen Matching List - Ciekawe :) 
typecheckPrim vars (EMatchL p e1 e2 (head,tail,e3)) = 
    case typecheckPrim vars e1 of
        ErrType p1 m1 -> ErrType p1 m1
        OKType (TList t) -> case (typecheckPrim vars e2, typecheckPrim ((tail, OKType (TList t)):(head, OKType t):vars) e3) of
                (ErrType p1 m1, _) -> ErrType p1 m1
                (_, ErrType p1 m1) -> ErrType p1 m1
                (OKType a, OKType b) -> case a == b of 
                    True -> OKType a
                    _ -> ErrType p ("TypeCheck Err: Bledny typ w wyrazeniu match.")
        _ -> ErrType p ("TypeCheck Err: Brak listy w matching list")

--- Dopisany kod na pracownie nr 6 ---

-- typy funkcji
typecheckPrim vars (EFn p var t e1) = 
    case typecheckPrim ((var, OKType t):vars) e1 of
        ErrType p1 m -> ErrType p1 m
        OKType a -> OKType (TArrow t a)

-- typy aplikacji
typecheckPrim vars (EApp p e1 e2) =
    case typecheckPrim vars e1 of
        ErrType p1 m -> ErrType p1 m
        OKType (TArrow t1 t2) -> case typecheckPrim vars e2 of
            ErrType p1 m -> ErrType p1 m
            OKType tn -> if tn == t1 then OKType t2 else ErrType p ("TypeCheck Err: zly typ argumentu")
        _ -> ErrType p ("TypeCheck Err: zly typ warunku, brak funkcji") 


---------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------Eval----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

-- Akurat w tym przypadku nie mamy za bardzo wielkiej filozofii, zakladajac, ze wyrazenia sa poprawnie otypowane mozemy swobodnie wywolywac
-- funkcje eval w poszegolnych przypadkach podazajac za wskazowkami z pliku ze SKOS.

-- typ standardowej funkcji eval
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
-- definicja standardowej funkcji eval
eval funDefinicje vars input =
    let mapWartosc = map aux1 vars where aux1 (var,val) = (var, IntegerValue val)
    in let mapFunck = map aux2 funDefinicje where aux2 fun = (funcName fun, FuncValue (funcArg fun) (funcBody fun))
    in case evalPrim funDefinicje (mapWartosc ++ mapFunck) input of
        IntegerValue value -> Value value
        _ -> RuntimeError

-- funkcja przyporzadkujaca zmiennym wartosci, gdzie argumentami sa mary (Key, Value)
-- niejako mapowanie wartosci do zmiennych, jesli warunek Key == Var jest spelniony
zmiennaWartosc :: [(Var, WartosciProgramu p)] -> Var -> WartosciProgramu p
zmiennaWartosc ((key, value):xs) var = 
    case (key == var) of
        True -> value 
        False -> zmiennaWartosc xs var

-- typ rozszerzonej funkcji eval
evalPrim :: [FunctionDef p] -> [(Var, WartosciProgramu p)] -> Expr p -> WartosciProgramu p
-- definicja rozszerzonej funkcji eval
-- podstawy wnioskowania o wartosciach
evalPrim funDefinicje srodowisko (EVar _ var) = zmiennaWartosc srodowisko var
evalPrim funDefinicje _ (ENum _ int) = IntegerValue int
evalPrim funDefinicje _ (EBool _ bool) = BooleanValue bool
-- wartosc wyrazen z operatorami unarnymi
evalPrim funDefinicje srodowisko (EUnary _ unop expr) =
    case evalPrim funDefinicje srodowisko expr of
        DivisonByZero -> DivisonByZero
        IntegerValue v -> IntegerValue (- v)
        BooleanValue v -> BooleanValue (not v)
-- wartosc wyrazen z operatorami binarnymi     
evalPrim funDefinicje srodowisko (EBinary _ binop e1 e2) = 
    case evalPrim funDefinicje srodowisko e1 of 
        DivisonByZero -> DivisonByZero
        IntegerValue v1 -> case evalPrim funDefinicje srodowisko e2 of
            DivisonByZero -> DivisonByZero
            IntegerValue v2 -> case binop of 
                -- wartosc wyrazen z operatorami porownania
                BEq -> BooleanValue (v1 == v2)
                BNeq -> BooleanValue (v1 /= v2)
                BLt -> BooleanValue (v1 < v2)
                BGt -> BooleanValue (v1 > v2)
                BLe -> BooleanValue (v1 <= v2)
                BGe -> BooleanValue (v1 >= v2)
                -- wartosc wyrazen z operatorami grupy multiplikatywnej
                BAdd -> IntegerValue (v1 + v2)
                BSub -> IntegerValue (v1 - v2)
                BMul -> IntegerValue (v1 * v2)
                BDiv -> case v2 == 0 of
                    False -> IntegerValue (v1 `div` v2)
                    True -> DivisonByZero
                BMod -> case v2 == 0 of
                    False -> IntegerValue (v1 `mod` v2)
                    True -> DivisonByZero
                -- wartosc wyrazen boolowskich
        BooleanValue p1 -> case evalPrim funDefinicje srodowisko e2 of 
            DivisonByZero -> DivisonByZero 
            BooleanValue p2 -> case binop of
                BAnd -> BooleanValue (p1 && p2)
                BOr -> BooleanValue (p1 || p2)

-- wartosc wyrazen typu Let In
evalPrim funDefinicje srodowisko (ELet _ var e1 e2) =
    case evalPrim funDefinicje srodowisko e1 of
        DivisonByZero -> DivisonByZero    
        av -> evalPrim funDefinicje ((var, av):srodowisko) e2

-- wartosc wyrazen typu If
evalPrim funDefinicje srodowisko (EIf _ e0 e1 e2) =
    case evalPrim funDefinicje srodowisko e0 of
        DivisonByZero -> DivisonByZero
        BooleanValue True -> evalPrim funDefinicje srodowisko e1
        BooleanValue False -> evalPrim funDefinicje srodowisko e2

-- wartosc funkcji
evalPrim funDefinicje srodowisko (EFn p var _ expr) = LambdaValue p var srodowisko expr

-- wartosc aplikacji
evalPrim funDefinicje srodowisko (EApp p e1 e2) = 
    case evalPrim funDefinicje srodowisko e2 of
        DivisonByZero -> DivisonByZero
        val -> let srodowiskoFun = map aux funDefinicje where aux fun = (funcName fun, FuncValue (funcArg fun) (funcBody fun))
            in case evalPrim funDefinicje srodowisko e1 of 
            LambdaValue p var updatedSrodowisko expr -> evalPrim funDefinicje ((var,val):updatedSrodowisko) expr
            FuncValue var body -> evalPrim funDefinicje ((var, val):srodowiskoFun) body

-- wartosc wyrazen Unit()
evalPrim _ _ (EUnit p) = UnitValue

-- wartosc wyrazen Pary
evalPrim funDefinicje srodowisko (EPair p e1 e2) = 
    case (evalPrim funDefinicje srodowisko e1, evalPrim funDefinicje srodowisko e2) of
        (DivisonByZero, _) -> DivisonByZero
        (_, DivisonByZero) -> DivisonByZero
        (a,b) -> PairValue a b

-- wartosc wyrazen Projkecji Fst
evalPrim funDefinicje srodowisko (EFst p e1) = 
    case evalPrim funDefinicje srodowisko e1 of 
        DivisonByZero -> DivisonByZero
        PairValue a b -> a
-- wartosc wyrazen Projkecji Snd
evalPrim funDefinicje srodowisko (ESnd p e1) = 
    case evalPrim funDefinicje srodowisko e1 of 
        DivisonByZero -> DivisonByZero
        PairValue a b -> b

-- wartosc wyrazen konstruktora listy pustej
evalPrim funDefinicje srodowisko (ENil p _) = ListValue []

-- wartosc wyrazen konstruktora listy niepustej
evalPrim funDefinicje srodowisko (ECons p e1 e2) = 
    case evalPrim funDefinicje srodowisko e2 of
        DivisonByZero -> DivisonByZero
        ListValue xs -> case evalPrim funDefinicje srodowisko e1 of
            DivisonByZero -> DivisonByZero
            x -> ListValue (x:xs)

-- wartosc wyrazen Matching List
evalPrim funDefinicje srodowisko (EMatchL p e1 e2 (head,tail,e3)) = 
    case evalPrim funDefinicje srodowisko e1 of
            DivisonByZero -> DivisonByZero
            ListValue [] -> evalPrim funDefinicje srodowisko e2
            ListValue (x:xs) -> evalPrim funDefinicje ((tail,ListValue xs):(head,x):srodowisko) e3