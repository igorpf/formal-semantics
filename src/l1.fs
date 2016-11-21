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

    
    let rec isList x = 
        match x with 
        | TmNil -> true
        | TmCons(a, b) ->
            let b' = isList b in
                isValue a && b'
        | _ -> false     
    let rec step t = 
        match t with
        | TmIf ( TmBool true , t2 , t3 ) -> t2
        | TmIf ( TmBool false, t2 , t3 ) -> t3
        | TmIf ( t1 , t2 , t3 ) -> 
            let t1' = step t1 in
                TmIf ( t1' , t2 , t3 )
        (*Op rules*)
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

        | TmTry (t, e2) -> 
            let t' = step t in
            TmTry(t', e2)
        | TmTry (TmRaise, e2) -> e2

        | TmCons(e1, e2) when not (isValue e1) -> 
            let e1' = step e1 in 
            TmCons(e1', e2)
        | TmCons(e1, e2) when (isValue e1) -> 
            let e2' = step e2 in 
            TmCons(e1, e2')
        | TmHd (TmCons (t1, t2)) when isList(TmCons (t1, t2))-> t1
        | TmTl (TmCons (t1, t2)) when isList(TmCons (t1, t2)) -> t2
        | TmIsEmpty (TmNil) -> TmBool true
        | TmIsEmpty (TmCons(t1,t2)) when isList(TmCons (t1, t2)) -> TmBool false

        | TmApply (TmFn(x,e), e2) when isValue e2-> substitute (TmX x) e2 e 
        
        | TmApply (e1, e2) when isValue e1-> 
            let e2' = step e2 in
            TmApply(e1,e2')
        | TmApply (e1, e2) -> 
            let e1' = step e1 in
            TmApply(e1',e2)    
        | TmLet (x, e1, e2) when isValue e1-> substitute (TmX x) e1 e2
        | TmLet (x, e1, e2) ->
            let e1' = step e1 in
            TmLet(x, e1', e2)
            (*let rec f = (fn y => e1) in e2  *)
        | TmLetRec(f, TmFn(y, e1), e2) ->
            substitute (TmX f) (TmFn(y, TmLetRec(f, TmFn(y, e1), e1))) e2
        
        | _ -> raise NoRuleApplies

    

    let rec eval t =
        try let t' = step t
            in eval t'
            with NoRuleApplies -> t

    (*--------------------------------------Testes---------------------------------------------*)
    (*Eval - operadores*)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpPlus,TmInt 2, TmInt 3)))) = TmInt 5)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpPlus,(TmOp(OpPlus,TmInt 2, TmInt 3)), TmInt 3)))) = TmInt 8)
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmOp(OpEQ,TmInt 3, TmInt 3)))) = TmBool true)
    (*Eval - apply*)    
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmApply (TmFn("x",TmOp(OpMinus, (TmX "x"), (TmInt 3))), (TmInt 5)))) = TmInt 2)
    (*Eval - Let e Let rec*)    
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLet("x",TmInt 5,TmOp(OpDiv, TmInt 10, TmX "x")))) = TmInt 2)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLetRec("fat", TmFn("x", TmIf(TmOp(OpEQ, TmX "x", TmInt 0),TmInt 1,TmOp(OpMult, TmX "x", TmApply(TmX "fat", TmOp(OpMinus, TmX "x", TmInt 1)) ))),TmApply(TmX "fat", TmInt 5)))) = TmInt 120)
    Console.WriteLine("Esperado: true, {0}", 
        (eval (TmLetRec("func", TmFn("x", TmIf(TmOp(OpEQ, TmX "x", TmInt 1),TmInt 1,TmOp(OpPlus, TmX "x", TmApply(TmX "func", TmOp(OpMinus, TmX "x", TmInt 1)) ))),TmApply(TmX "func", TmInt 3)))) = TmInt 6)
    (*Eval - listas*)            
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmHd(TmCons (TmInt 5, TmNil))))) = TmInt 5) 
    Console.WriteLine("Esperado: true, {0}", 
        (eval ((TmTl(TmCons (TmInt 5, TmNil))))) = TmNil)
    Console.WriteLine("Esperado: true, {0}",
        isList (TmCons(TmInt 5, TmCons(TmFn("s", TmInt 3), TmNil))))
    (*Substitute*)
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