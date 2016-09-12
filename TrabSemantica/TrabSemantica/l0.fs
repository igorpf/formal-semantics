﻿open System


type Term = 
    | TmTrue 
    | TmFalse
    | TmIf of Term * Term * Term
    | TmZero
    | TmSucc of Term
    | TmPred of Term
    | TmIsZero of Term

exception NoRuleApplies

let rec isnumericval t = 
    match t with
    | TmZero -> true
    | TmSucc ( t1 ) -> isnumericval t1
    | _ -> false

let rec step t = 
    match t with
    | TmIf ( TmTrue , t2 , t3 ) -> t2
    | TmIf ( TmFalse , t2 , t3 ) -> t3
    | TmIf ( t1 , t2 , t3 ) -> 
        let t1' = step t1 in
             TmIf ( t1' , t2 , t3 )
    | TmSucc ( t1 ) -> 
        let t1' = step t1 in
            TmSucc ( t1' )
    | TmPred ( TmZero ) -> TmZero
    | TmPred ( TmSucc ( nv1 ) ) when ( isnumericval nv1 ) -> nv1
    | TmPred ( t1 ) -> 
    let t1' = step t1 in
        TmPred ( t1' )
    | TmIsZero ( TmZero ) -> TmTrue
    | TmIsZero ( TmSucc ( nv1 ) ) when ( isnumericval nv1 ) -> TmFalse
    | TmIsZero ( t1 ) -> 
        let t1' = step t1 in
        TmIsZero ( t1' )
    | _ -> raise NoRuleApplies

let rec eval t =
    try let t' = step t
        in eval t'
        with NoRuleApplies -> t

let c = eval ((TmSucc(TmSucc TmZero)))
Console.WriteLine("{0}", c)
