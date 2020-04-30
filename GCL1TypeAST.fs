(*Rasmus s164162, Solvi s174391, Muse s194615*)

// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module GCL1TypeAST

type AExpr = 
    | Var of string 
    | Num of float
    | TimesExpr of AExpr*AExpr
    | DivExpr of AExpr*AExpr
    | PlusExpr of AExpr*AExpr
    | MinusExpr of AExpr*AExpr
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
    | Gte of AExpr*AExpr
    | Gt of AExpr*AExpr
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

// Definitions of parentheses used for syntax 
let lp leftpar = if leftpar then "(" else ""
let rp rightpar = if rightpar then ")" else ""

// matching for aexpression for graph
type AExpr with 
    member this.Eval env = 
        match this with
        | Num(float) -> float
        | Var(string) -> match Map.tryFind string env with
                            | Some(v) -> v
                            | None -> 0.0
        | PlusExpr(e1,e2) -> (e1.Eval env) + (e2.Eval env)
        | MinusExpr(e1,e2) -> (e1.Eval env) - (e2.Eval env)
        | TimesExpr(e1,e2) -> (e1.Eval env) * (e2.Eval env) 
        | DivExpr(e1,e2) -> (e1.Eval env) / (e2.Eval env) 
        | PowExpr(e1,e2) -> System.Math.Pow((e1.Eval env), (e2.Eval env))
        | Neg(e1) -> - (e1.Eval env)

    member this.Atomic = 
        match this with
        | Num(_) |Var(_) |Neg(_)-> true 
        | PlusExpr(_) | MinusExpr(_) | TimesExpr(_) | DivExpr(_) | PowExpr(_) -> false
       
    member this.ToString =
        match this with
        | Num(f) -> string(f)
        | Var(s) -> s
        | PlusExpr(e1,e2) -> (lp (not e1.Atomic)) + e1.ToString + (rp (not e1.Atomic)) + "+" + (lp (not e2.Atomic)) +  e2.ToString  + (rp (not e2.Atomic))
        | MinusExpr(e1,e2) -> (lp (not e1.Atomic)) + e1.ToString + (rp (not e1.Atomic)) + "-" + (lp (not e2.Atomic)) +  e2.ToString  + (rp (not e2.Atomic))
        | TimesExpr(e1,e2) -> (lp (not e1.Atomic)) + e1.ToString + (rp (not e1.Atomic)) + "*" + (lp (not e2.Atomic)) +  e2.ToString  + (rp (not e2.Atomic))
        | DivExpr(e1,e2) -> (lp (not e1.Atomic)) + e1.ToString + (rp (not e1.Atomic)) + "/" + (lp (not e2.Atomic)) +  e2.ToString  + (rp (not e2.Atomic)) 
        | PowExpr(e1,e2) -> (lp (not e1.Atomic)) + e1.ToString + (rp (not e1.Atomic)) + "^" + (lp (not e2.Atomic)) +  e2.ToString  + (rp (not e2.Atomic)) 
        | Neg(e1) -> "-" + (lp (not e1.Atomic)) + e1.ToString + (rp (not e1.Atomic))

// matching for bexpression for graph
type BExpr with 
    member this.Eval env = 
        match this with 
        | Bool(b) -> b
        | And(b1,b2) -> 
            let v1 = (b1.Eval env) 
            let v2 = (b2.Eval env)
            v1 && v2
        | Or(b1,b2) -> 
            let v1 = (b1.Eval env) 
            let v2 = (b2.Eval env)
            v1 || v2
        | Sand(b1,b2) -> (b1.Eval env) && (b2.Eval env)
        | Sor(b1,b2) -> (b1.Eval env) || (b2.Eval env)
        | Not(b) -> not (b.Eval env)
        | Eq(a1,a2) -> (a1.Eval env) = (a2.Eval env)
        | Neq(a1,a2) -> (a1.Eval env) <> (a2.Eval env)
        | Gt(a1,a2) -> (a1.Eval env) > (a2.Eval env)
        | Gte(a1,a2) -> (a1.Eval env) >= (a2.Eval env)
        | Lt(a1,a2) -> (a1.Eval env) < (a2.Eval env)
        | Lte(a1,a2) -> (a1.Eval env) <= (a2.Eval env)

    member this.Atomic = 
        match this with
        | Bool(_) -> true
        | Not(_) | Eq(_) | Neq(_) | Lt(_) | Lte(_) | Gt(_) | Gte(_)-> false
        | Or(_) | Sor(_) | And(_) | Sand(_) ->   false
         
    member this.ToString = 
        match this with
        | Bool(b) -> string(b)
        | And(b1,b2) ->  (lp (not b1.Atomic)) + b1.ToString + (rp (not b2.Atomic)) + " & " + (lp (not b2.Atomic)) + b2.ToString + (rp (not b2.Atomic))
        | Or(b1,b2) ->   (lp (not b1.Atomic)) + b1.ToString + (rp (not b2.Atomic)) + " | " + (lp (not b2.Atomic)) + b2.ToString + (rp (not b2.Atomic))
        | Sand(b1,b2) -> (lp (not b1.Atomic)) + b1.ToString + (rp (not b2.Atomic)) + " && " + (lp (not b2.Atomic)) + b2.ToString + (rp (not b2.Atomic))
        | Sor(b1,b2) ->  (lp (not b1.Atomic)) + b1.ToString + (rp (not b2.Atomic)) + " || " + (lp (not b2.Atomic)) + b2.ToString + (rp (not b2.Atomic))
        | Not(b) -> "!"+ (lp (not b.Atomic)) + b.ToString + (rp (not b.Atomic))
        | Eq(a1,a2) ->   (lp (not a1.Atomic)) + a1.ToString + (rp (not a2.Atomic)) + " = " + (lp (not a2.Atomic)) + a2.ToString + (rp (not a2.Atomic))
        | Neq(a1,a2) ->  (lp (not a1.Atomic)) + a1.ToString + (rp (not a2.Atomic)) + " != " + (lp (not a2.Atomic)) + a2.ToString + (rp (not a2.Atomic))
        | Gt(a1,a2) ->   (lp (not a1.Atomic)) + a1.ToString + (rp (not a2.Atomic)) + " > " + (lp (not a2.Atomic)) + a2.ToString + (rp (not a2.Atomic))
        | Gte(a1,a2) ->  (lp (not a1.Atomic)) + a1.ToString + (rp (not a2.Atomic)) + " >= " + (lp (not a2.Atomic)) + a2.ToString + (rp (not a2.Atomic))
        | Lt(a1,a2) ->   (lp (not a1.Atomic)) + a1.ToString + (rp (not a2.Atomic)) + " < " + (lp (not a2.Atomic)) + a2.ToString + (rp (not a2.Atomic))
        | Lte(a1,a2) ->  (lp (not a1.Atomic)) + a1.ToString + (rp (not a2.Atomic)) + " <= " + (lp (not a2.Atomic)) + a2.ToString + (rp (not a2.Atomic))

// noexecute for GuardedCommands
let rec noexecute = function 
    | GC(b,_) -> "!(" + b.ToString + ")"
    | GCs(gc1,gc2) -> noexecute gc1 + " & " + noexecute gc2

// for printing transitions in .gv file
let transition first second label = "\tq"+first+" -> "+"q"+second+" [label = \""+label+"\"];\n"

// command part in GCLParser for graph.
let rec CommandGraph first second n = function
    | Skip -> transition first second "skip" , n
    | Ass(name,value) -> transition first second (name + " := " + value.ToString) , n
    | Commands(c1,c2) -> CommandGraph first (string(n)) (n+1) c1 |> fun (s1,n1) ->  CommandGraph (string(n)) second (n1) c2 |> fun (s2,n2) -> s1+s2, n2
    | If(gc) -> GuardedCommandGraph first second n gc
    | Do(gc) -> GuardedCommandGraph first first n gc |> fun (graph, n) -> graph + (transition first second  (noexecute gc) ), n
and GuardedCommandGraph first second n = function 
    | GC(b,c) -> let str,n' = CommandGraph (string(n)) second (n+1) c
                 (transition first (string(n)) b.ToString) + str, n'
    | GCs(gc1,gc2) -> let s1,n1 = (GuardedCommandGraph first second (n) gc1)
                      let s2,n2 = (GuardedCommandGraph first second (n1) gc2)
                      (s1+s2),n2

// header taken from http://www.formalmethods.dk/fm4fun/ (export graph). 
let header =   "digraph program_graph {rankdir=LR;
node [shape = circle]; q▷;
node [shape = doublecircle]; q◀; 
node [shape = circle]\n"

// footer taken from http://www.formalmethods.dk/fm4fun/ (export graph)
let footer = "}"

// definition of program which will be read by GCL.fsx. This will construct a program with
// a header, transitions and footer. The structure is: 
///////////////////HEADER////////////////////////
//              digraph program_graph {rankdir=LR;
//              node [shape = circle]; q▷;
//              node [shape = doublecircle]; q◀; 
//              node [shape = circle]
//////////////////TRANSITIONS////////////////////
//              q▷ -> qn [label = ""]; etc, where n is the states.
//////////////////FOOTER////////////////////
//              }

let ProgramGraph program = let s, _ = CommandGraph "▷" "◀" 1 program
                           header + s + footer

// Definition of GuardedCommand used for graph. 
type GuardedCommand with
    member this.TryFind env = 
        match this with
        | GC(b,c) -> if b.Eval env then Some(c) else None
        | GCs(gc1,gc2) -> match gc1.TryFind env with
                          | None -> gc2.TryFind env
                          | s -> s

// Lastly the command used for graph. 
type Command with 
    member this.Exec env = 
        match this with
        | Ass(s, aexpr) -> Map.add s (aexpr.Eval env) env
        | Skip -> env
        | Commands(c1,c2) -> c2.Exec (c1.Exec env) 
        | If(gc) -> match gc.TryFind env with
                    | Some(c) -> c.Exec env
                    | None -> failwith("Unable to construct path")
        | Do(gc) -> match gc.TryFind env with
                    | Some(c) -> this.Exec (c.Exec env)
                    | None -> env