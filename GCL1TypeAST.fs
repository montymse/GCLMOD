(*Rasmus s164162, Solvi s174391, Muse s194615*)

// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module GCL1TypeAST

type AExpr = 
    | Num of float
    | Var of string 
    | PlusExpr of AExpr*AExpr
    | MinusExpr of AExpr*AExpr
    | TimesExpr of AExpr*AExpr
    | DivExpr of AExpr*AExpr
    | PowExpr of AExpr*AExpr
    | Neg of AExpr

type BExpr =
    | Bool of bool
    | And of BExpr*BExpr
    | Or of BExpr*BExpr
    | Sand of BExpr*BExpr
    | Sor of BExpr*BExpr
    | Not of BExpr
    | Eq of AExpr*AExpr
    | Neq of AExpr*AExpr
    | Gt of AExpr*AExpr
    | Gte of AExpr*AExpr
    | Lt of AExpr*AExpr
    | Lte of AExpr*AExpr

type Command = 
    | Ass of string*AExpr
    | Skip
    | Commands of Command*Command
    | If of GuardedCommand
    | Do of GuardedCommand
and GuardedCommand =
    | GC of BExpr*Command
    | GCs of GuardedCommand*GuardedCommand

