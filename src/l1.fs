module L1 = 
    type TmBool =
            | TmTrue    
            | TmFalse
    type TmInt = int
    type TmX = string
    type TmOp = 
        | TmPlus
        | TmMinus
        | TmMult
        | TmDiv
        | TmLT (*<*)
        | TmLE (*<=*)
        | TmEQ (*==*)
        | TmNEQ (*!=*)
        | TmGT (*>*)
        | TmGE (*>=*)
    type Term = 
        | TmBool
        | TmInt
        | TmOp of Term * Term
        | TmIf of Term * Term * Term
        | TmX (*Variável X*)
        | TmApply of Term * Term (*e1 e2*)
        | TmFn of TmX * Term (*fn X:T -> e*)
        
    