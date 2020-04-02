(*Rasmus s164162, Solvi s174391, Muse s194615*)

// With this script we implements our Parser

// We import a couple of modules, including the generated lexer and parser

#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCL1TypeAST.fs"
open GCL1TypeAST
#load "GCL1Parser.fs"
open GCL1Parser
#load "GCL1Lexer.fs"
open GCL1Lexer

open System.IO


// The parser 
let parse input =

    let lexbuf = LexBuffer<char>.FromString input

    let res= GCL1Parser.start GCL1Lexer.tokenize lexbuf

    res
        
type Operation = 
    | Boolean of int * BExpr
    | Assign of int * (string * AExpr)
    | SkipCMD of int

// List of operations
type OperationList = (int * Operation) list

//State variable
let mutable state = 0   

///<summary>
/// The evaluator function
///</summary>

let rec evalC (e:Command) (start:int) (endState:int) =
    match e with
    | Ass(var,expr)    -> [start,Assign(endState,(var,expr))]
    | Skip(_)          -> [start,SkipCMD(endState)]
    | Commands(c1,c2)  ->  state <- state+1
                           let staticState = state
                           (evalC c1 start staticState) @ (evalC c2 staticState endState)
    | If(gc1)      -> evalGC gc1 start endState
    | Do(gc1)      -> (evalGC gc1 start start) @ (evalGCend gc1 start endState)

and evalGC (gc:GuardedCommand) (start:int) (endState:int) =
    match gc with
    | GCs (gc1,gc2)    -> (evalGC gc1 start endState) @ (evalGC gc2 start endState)
    | GC (b1,c)        -> state <- state + 1
                          [start,Boolean(state,b1)] @ (evalC c state endState)
and evalGCend (gc:GuardedCommand) (start:int) (endState:int) =
    match gc with
    | GCs (gc1,gc2)    ->  (evalGCend gc1 start endState) @ (evalGCend gc2 start endState)
    | GC (b1,_)        ->  state <- state + 1
                           [start,Boolean(endState,Not(b1))]


///<summary>
/// The Interpreter function
///</summary>

let rec Interpreter (currentNode:int) n (graph:OperationList) (varMap:Map<string,float>) (programSteps:int) =
    match graph with
    | (s,op)::_ when n <= 0 -> printfn "%s" ("Ran out of programSteps. Status: " + "\n" + (printVar (Map.toList varMap)))
    | (s,op)::tail when s <> currentNode -> Interpreter currentNode n tail varMap programSteps
    | (s,op)::tail when s = currentNode  -> match op with
                                            | Boolean(s,bx)         -> if GBExpr bx varMap then Interpreter s (n-1) graph varMap programSteps else Interpreter currentNode n tail varMap programSteps
                                            | Assign(s,(var,ex)) -> Interpreter s (n-1) graph (varMap.Add(var,(GAExpr ex varMap))) programSteps
                                            | SkipCMD(s)            -> Interpreter s (n-1) graph varMap programSteps
    | _                                  -> printfn "%s" ("Terminated in " + string(programSteps-n) + "\n" + (printVar (Map.toList varMap)))
and GAExpr (aexpr:AExpr) (varMap) =
    match aexpr with
    | Var (s)                   -> if varMap.ContainsKey s then Map.find s varMap else failwith (string(s)+" not_found")
    | Num (num)                 -> num
    | TimesExpr(ex1,ex2)        -> GAExpr ex1 varMap * GAExpr ex2 varMap
    | DivExpr(ex1,ex2)          -> (GAExpr ex1 varMap / GAExpr ex2 varMap)
    | PlusExpr(ex1,ex2)         -> (GAExpr ex1 varMap + GAExpr ex2 varMap)
    | MinusExpr(ex1,ex2)        -> (GAExpr ex1 varMap - GAExpr ex2 varMap)
    | PowExpr(ex1,ex2)          -> (GAExpr ex1 varMap ** GAExpr ex2 varMap)
    | Neg(ex1)                  -> -(GAExpr ex1 varMap)

and GBExpr (bexpr:BExpr) varMap =
    match bexpr with
    | Bool(b)               -> b
    | And (b1,b2)           -> let lhs = GBExpr b1 varMap
                               let rhs = GBExpr b2 varMap
                               lhs && rhs
    | Or (b1,b2)            -> let lhs = GBExpr b1 varMap
                               let rhs = GBExpr b2 varMap
                               lhs || rhs
    | Sand(b1,b2)           -> GBExpr b1 varMap && GBExpr b2 varMap
    | Sor (b1,b2)           -> GBExpr b1 varMap || GBExpr b2 varMap
    | Not (b1)              -> not(GBExpr b1 varMap)

    | Eq (e1,e2)            -> GAExpr e1 varMap = GAExpr e2 varMap
    | Neq (e1,e2)           -> GAExpr e1 varMap <> GAExpr e2 varMap
    | Gt (e1,e2)            -> GAExpr e1 varMap > GAExpr e2 varMap
    | Gte (e1,e2)            -> GAExpr e1 varMap >= GAExpr e2 varMap
    | Lt (e1,e2)            -> GAExpr e1 varMap < GAExpr e2 varMap
    | Lte (e1,e2)            -> GAExpr e1 varMap <= GAExpr e2 varMap
and printVar list =
    match list with
    | (var,value)::tail -> var + "=" + string(value) + "\n" + printVar tail
    | [] -> ""


///<summary>
/// The function GCL implements the parser and checks the syntax 
///</summary>
///<returns>
/// It either returns 'program rejected' if the syntax is incorrect otherwise 'program accepted'
///</returns>
///<remarks>
/// As input you can either type something into the console by using the Command Console.ReadLine()
/// or use the test file "TestFile.txt" by using the Command File.ReadAllText("TestFile.txt")
/// By default we use Console.ReadLine() 
///</remarks>
let GCL =  
    //GCL program read from file
    let input = File.ReadAllText("TestFile1.txt")
    //Initial node qi
    let qi = 0
    //Number of programSteps
    printfn"Choose number of programSteps:"
    let programSteps = int (Console.ReadLine())
    
    //Variable number (IF MORE VARIABLES NEED ADD HERE)  
    let varMap = Map.empty.Add("x",5.0)

    printf "\n----------------------------\nInput program:\n\n%s\n----------------------------\n" input

    try
       let programGCL  = parse (input)
       let oplist = (evalC programGCL 0 (-1))
       let pg = ProgramGraph (programGCL)
       let wr = new System.IO.StreamWriter("graph.gv")
       wr.Write pg
       wr.Close() 

       (Interpreter qi programSteps oplist varMap programSteps)

       printf "----------------------------\nProgram accepted!\n----------------------------\n"

    with err -> 
        printf "Program rejected!\n"