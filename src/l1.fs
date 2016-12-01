open System
module L1 = 
    
            
     
    type Ident = string
    
    type Operator = 
        | OpPlus
        | OpMinus
        | OpMult
        | OpDiv
        | OpLT (*<*)
        | OpLE (*<=*)
        | OpEQ (*==*)
        | OpNEQ (*!=*)
        | OpGT (*>*)
        | OpGE (*>=*)
    type Term = 
        | TmBool of bool         
        | TmInt of int
        | TmOp of Operator * Term * Term
        | TmIf of Term * Term * Term
        | TmX of Ident (*Variável X*)
        | TmApply of Term * Term (*e1 e2*)
        | TmFn of Ident * Term (*fn X:T -> e*)        
        | TmLet of Ident * Term * Term (*let x = e1 in e2*)
        | TmLetRec of Ident * Term * Term (*let rec f = (fn y => e1) in e2  *)
        | TmNil 
        | TmCons of Term * Term (*e1::e2*)
        | TmIsEmpty of Term
        | TmHd of Term 
        | TmTl of Term
        | TmRaise
        | TmTry of Term * Term 

    exception NoRuleApplies
            
        
        
    let rec isValue x = 
        match x with
        | TmBool x -> true
        | TmInt x -> true
        | TmFn (a, b) -> true
        | TmNil -> true
        | TmCons(a, b) ->
            isValue a && isValue b
        | _ -> false
    (*{v/x} e*)
    let rec substitute x v e = 
        match (x, e) with        
        | (TmX b,TmOp(op, y , z))  -> 
            let sy = substitute x v y in
                let sz = substitute x v z in 
                    TmOp(op, sy, sz)
        | (TmX b,TmApply(e1, e2))   ->
            let e1' = substitute x v e1 in
                let e2' = substitute x v e2 in
                    TmApply(e1',e2')
        | (TmX b,TmIf(e1, e2 , e3))  ->
            let e1' = substitute x v e1 in
                let e2' = substitute x v e2 in 
                    let e3' = substitute x v e3 in
                        TmIf(e1', e2' , e3')
        | (TmX b,TmFn(id, e1)) when b <> id ->
            let e1' = substitute x v e1 in
                TmFn(id, e1')
        | (TmX b, TmLet(id, e1, e2)) when b <> id -> 
             let e1' = substitute x v e1 in
                let e2' = substitute x v e2 in 
                    TmLet(id, e1', e2')
        | (TmX b,TmLetRec(id, e1, e2)) when b <> id  ->
            let e1' = substitute x v e1 in
                 let e2' = substitute x v e2 in
                    TmLetRec(id, e1', e2')
        | (TmX b,z)  when (TmX b) = z -> v  
        | _ -> e

        
    let rec step t = 
        match t with
        | TmTry (TmRaise, e2) -> e2
        | TmTry (t, e2) -> 
            let t' = step t in
            TmTry(t', e2)

        | TmIf ( TmRaise , t2 , t3) -> TmRaise
        | TmIf ( TmBool true , t2 , t3 ) -> t2
        | TmIf ( TmBool false, t2 , t3 ) -> t3
        | TmIf ( t1 , t2 , t3 ) -> 
            let t1' = step t1 in
                TmIf ( t1' , t2 , t3 )
        (*Op rules*)
        | TmOp (t, TmRaise, TmInt y) -> TmRaise
        | TmOp (t, TmInt x, TmRaise) -> TmRaise

        | TmOp (OpPlus, TmInt x, TmInt y) -> TmInt (x+y)
        | TmOp (OpMinus, TmInt x, TmInt y) -> TmInt (x-y)
        | TmOp (OpMult, TmInt x, TmInt y) -> TmInt (x*y)
        | TmOp (OpDiv, TmInt x, TmInt 0) -> TmRaise
        | TmOp (OpDiv, TmInt x, TmInt y) when (y <> 0) -> TmInt (x/y)
        | TmOp (OpLT, TmInt x, TmInt y) -> TmBool (x<y)
        | TmOp (OpLE, TmInt x, TmInt y) -> TmBool (x<=y)
        | TmOp (OpEQ, TmInt x, TmInt y) -> TmBool (x=y)
        | TmOp (OpNEQ, TmInt x, TmInt y) -> TmBool (x<>y)
        | TmOp (OpGT, TmInt x, TmInt y) -> TmBool (x>y)
        | TmOp (OpGE, TmInt x, TmInt y) -> TmBool (x>=y)
        | TmOp ( t, TmInt x, t2) ->
            let t2' = step t2 in
            TmOp(t, TmInt x, t2')
        | TmOp ( t, t1, t2) ->
            let t1' = step t1 in
            TmOp(t, t1', t2)

        | TmHd (TmCons(TmRaise, e2)) -> TmRaise

        | TmCons(e1, e2) when not (isValue e1) -> 
            let e1' = step e1 in 
            TmCons(e1', e2)
        | TmCons(e1, e2) when (isValue e1) -> 
            let e2' = step e2 in 
            TmCons(e1, e2')

        | TmHd TmNil -> TmRaise
        | TmTl TmNil -> TmRaise
        | TmHd (TmCons (t1, t2)) -> t1
        | TmTl (TmCons (t1, t2)) -> t2

        | TmIsEmpty (TmRaise) -> TmRaise
        | TmIsEmpty (TmNil) -> TmBool true
        | TmIsEmpty (TmCons(t1,t2)) -> TmBool false

        | TmApply (TmRaise, e2) -> TmRaise
        | TmApply (e1, TmRaise) when isValue e1 -> TmRaise
        | TmApply (TmFn(x,e), e2) when isValue e2-> substitute (TmX x) e2 e 
        | TmApply (e1, e2) when isValue e1-> 
            let e2' = step e2 in
            TmApply(e1,e2')
        | TmApply (e1, e2) -> 
            let e1' = step e1 in
            TmApply(e1',e2)    

        | TmLet (x, TmRaise, e2) -> TmRaise
        | TmLet (x, e1, TmRaise) -> TmRaise
        | TmLet (x, e1, e2) when isValue e1-> substitute (TmX x) e1 e2
        | TmLet (x, e1, e2) ->
            let e1' = step e1 in
            TmLet(x, e1', e2)
            (*let rec f = (fn y => e1) in e2  *)
        | TmLetRec (f, TmRaise, e2) -> TmRaise
        | TmLetRec (f, TmFn(y, e1), TmRaise) -> TmRaise

        | TmLetRec(f, TmFn(y, e1), e2) ->
            substitute (TmX f) (TmFn(y, TmLetRec(f, TmFn(y, e1), e1))) e2
        
        | _ -> raise NoRuleApplies

    

    let rec eval t =
        try let t' = step t
            in eval t'
            with NoRuleApplies -> t

    (*--------------------------------------Testes---------------------------------------------*)
    (*Eval - operadores*)
    Console.WriteLine("OP: ")
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpPlus,TmInt 2, TmInt 3)))) = TmInt 5)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpPlus,(TmOp(OpPlus,TmInt 2, TmInt 3)), TmInt 3)))) = TmInt 8)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpEQ,TmInt 3, TmInt 3)))) = TmBool true)
        (*Raise*)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpPlus,TmRaise, TmInt 3)))) = TmRaise)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpPlus,TmInt 3, TmRaise)))) = TmRaise)
    (*Eval - apply*)
    Console.WriteLine("Apply: ")    
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmApply (TmFn("x",TmOp(OpMinus, (TmX "x"), (TmInt 3))), (TmInt 5)))) = TmInt 2)
        (*Raise*)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmApply (TmFn("x",TmOp(OpMinus, (TmX "x"), (TmInt 3))), TmRaise))) = TmRaise)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmApply (TmRaise,  (TmInt 3)))) = TmRaise)
    (*Eval - Let e Let rec*)
    Console.WriteLine("Let: ")    
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLet("x",TmInt 5,TmOp(OpDiv, TmInt 10, TmX "x")))) = TmInt 2)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLetRec("fat", TmFn("x", TmIf(TmOp(OpEQ, TmX "x", TmInt 0),TmInt 1,TmOp(OpMult, TmX "x", TmApply(TmX "fat", TmOp(OpMinus, TmX "x", TmInt 1)) ))),TmApply(TmX "fat", TmInt 5)))) = TmInt 120)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLetRec("func", TmFn("x", TmIf(TmOp(OpEQ, TmX "x", TmInt 1),TmInt 1,TmOp(OpPlus, TmX "x", TmApply(TmX "func", TmOp(OpMinus, TmX "x", TmInt 1)) ))),TmApply(TmX "func", TmInt 3)))) = TmInt 6)
        (*Raise*)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLet("x",TmRaise,TmOp(OpDiv, TmInt 10, TmX "x")))) = TmRaise)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLet("x",TmRaise,TmRaise))) = TmRaise)
    (*Eval - listas*)
    Console.WriteLine("List: ")             
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmHd(TmCons (TmInt 5, TmNil))))) = TmInt 5) 
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmTl(TmCons (TmInt 5, TmNil))))) = TmNil)
    Console.WriteLine("Esperado: true, {0}",
        isValue (TmCons(TmInt 5, TmCons(TmFn("s", TmInt 3), TmNil))))
        (*Raise*)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmHd(TmCons (TmRaise, TmNil))))) = TmRaise)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmHd(TmNil)))) = TmRaise)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmTl(TmNil)))) = TmRaise)  
    (*Substitute*)
    Console.WriteLine("Subst: ")
    Console.WriteLine("Esperado: true, {0}", 
        eval (substitute (TmX "x") (TmBool true) (TmIf( TmX "x",TmInt 1, TmInt 2))) =TmInt 1)
    Console.WriteLine("Esperado: true, {0}", 
        eval (substitute (TmX "func") (TmFn("x", TmOp(OpMinus, TmX "x", TmInt 1)))  (TmApply(TmX "func", TmInt 3))) =TmInt 2)
    Console.WriteLine("Esperado: true, {0}", 
        eval (substitute (TmX "x") (TmInt 1) (TmIf(TmOp(OpEQ, TmX "x", TmInt 0),TmInt 1, TmInt 2))) =TmInt 2)    
    Console.WriteLine("Esperado: true, {0}", 
        substitute (TmX "x") (TmInt 1) (TmOp(OpPlus, TmX "x", TmInt 2)) =TmOp(OpPlus, TmInt 1, TmInt 2))
    Console.WriteLine("Esperado: true, {0}", 
        substitute (TmX "x") (TmInt 1)  (TmX "x") =TmInt 1)
    Console.WriteLine("Esperado: true, {0}", 
        substitute (TmX "x") (TmInt 1)  (TmInt 3) = TmInt 3)

    while true do 1+1
        