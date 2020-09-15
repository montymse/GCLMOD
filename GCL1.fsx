(*Muse s194615*)

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
open System.Text.RegularExpressions


(*------------------------THE PARSER-------------------------*)

let parse input =

    let lexbuf = LexBuffer<char>.FromString input

    let res= GCL1Parser.start GCL1Lexer.tokenize lexbuf

    res


(*---------------------THE SIGN ANALYSIS---------------------*)


///<summary>
/// Sign analyzer
/// </summary>

let rec signAnalyzer (x:(string*float) list) (result:(string*string) list)=
    match x with
    | (var,op)::xs when op>0.0 -> signAnalyzer xs ((var,"+")::result)
    | (var,op)::xs when op<0.0 -> signAnalyzer xs ((var,"-")::result)
    | (var,op)::xs when op=0.0 -> signAnalyzer xs ((var,"0")::result)
    | _ ->  result
 
///<summary>
/// Prints Sign analysis
/// </summary>

let rec Varlist (result:(string*string) list) n =
    match result with
     | (var,sign)::xs -> Varlist xs n@[var]
     | [] -> n
and Signlist (result:(string*string) list) m = 
    match result with
     | (var,sign)::xs -> Signlist xs m@[sign]
     | [] -> m
and printVars (result:(string*string) list) =
    let r= (Varlist result []) 
    for elm in r do 
        printf "%A " elm
    printfn""
and printSigns (result:(string*string) list) =
    let r= (Signlist result []) 
    for elm in r do 
        printf "%A " elm
and printSignLists (results:(int * (string *string) list) list) =
    match results with
    | _::(_,result)::xs -> printSigns result
                           printfn "\n"
                           printSignLists xs

    | [(_,result)]      -> printSigns result
                           printfn "\n"

    | []                -> printfn "\n"


(*----------------------THE EVALUATOR------------------------*)

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
    | Ass(var,expr)         -> [start,Assign(endState,(var,expr))]
    | Skip(_)               -> [start,SkipCMD(endState)]
    | Commands(c1,c2)       -> state <- state+1
                               let staticState = state
                               (evalC c1 start staticState) @ (evalC c2 staticState endState)
    | If(guarded)           ->  
                                evalGC guarded start endState
                               
    | Do(guarded)           ->  
                                (evalGC guarded start start) @ (evalGCend guarded start endState)

and evalGC (gc:GuardedCommand) (start:int) (endState:int) =
    match gc with
    | GCs (gc1,gc2)      -> (evalGC gc1 start endState) @ (evalGC gc2 start endState)
    | GC (b1,c)          -> state <- state + 1
                            [start,Boolean(state,b1)] @ (evalC c state endState)
and evalGCend (gc:GuardedCommand) (start:int) (endState:int) =
    match gc with
    | GCs (gc1,gc2)      -> (evalGCend gc1 start endState) @ (evalGCend gc2 start endState)
    | GC (b1,_)          -> state <- state + 1
                            [start,Boolean(endState,Not(b1))]


let rec GAExpr (aexpr:AExpr) (varMap:Map<string,float>) = 
    match aexpr with
    | Var (s)                   -> if varMap.ContainsKey s then Map.find s varMap else failwith (string(s)+" not_found")
    | Num (num)                 -> num
    | TimesExpr(ex1,ex2)        -> GAExpr ex1 varMap * GAExpr ex2 varMap
    | DivExpr(ex1,ex2)          -> (GAExpr ex1 varMap / GAExpr ex2 varMap)
    | PlusExpr(ex1,ex2)         -> (GAExpr ex1 varMap + GAExpr ex2 varMap)
    | MinusExpr(ex1,ex2)        -> (GAExpr ex1 varMap - GAExpr ex2 varMap)
    | PowExpr(ex1,ex2)          -> (GAExpr ex1 varMap ** GAExpr ex2 varMap)
    | Neg(ex1)                  -> -(GAExpr ex1 varMap)


let rec GBExpr (bexpr:BExpr) varMap =
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
    | Gte (e1,e2)           -> GAExpr e1 varMap >= GAExpr e2 varMap
    | Lte (e1,e2)            -> GAExpr e1 varMap < GAExpr e2 varMap
    | Lt (e1,e2)           -> GAExpr e1 varMap <= GAExpr e2 varMap

let rec printVarlist list =
    match list with
    | (var,value)::tail -> var + "=" + string(value) + "\n" + printVarlist tail
    | [] -> ""


(*---------------------THE INTERPRETER---------------------*)

///<summary>
/// The Interpreter function of the evaluation of the GCL
///</summary>

let rec Interpreter (currentNode:int) n (graph:OperationList) (varMap:Map<string,float>) (programSteps:int) (signs:(int * (string *string) list) list) (oplist) =
    match graph with
    | (s,op)::_ when n <= 0              -> 
                                         //   printVars (signAnalyzer (Map.toList varMap) [])
                                            
                                          //  printSignLists signs
                                            printfn "%s" ("Ran out of programSteps. Status: " + "\n" + (printVarlist (Map.toList varMap)))                           
    | (s,op)::tail when s <> currentNode -> 
                                           
                                            Interpreter currentNode n tail varMap programSteps (signs) oplist

    | (s,op)::tail when s = currentNode  ->     
                                            let st=[(programSteps,signAnalyzer (Map.toList varMap) [])]
                                            match op with
                                            | Boolean(s,bx)         -> if GBExpr bx varMap then 
                                                                         Interpreter s (n-1) oplist varMap programSteps (signs@st) oplist 
                                                                       else
                                                                         Interpreter currentNode n tail varMap programSteps (signs@st) oplist
                                            | Assign(s,(var,ex))    ->
                                                                       Interpreter s (n-1) oplist (varMap.Add(var,(GAExpr ex varMap))) programSteps (signs@st) oplist
                                            | SkipCMD(s)            -> Interpreter s (n-1) oplist varMap programSteps (signs@st) oplist
    | _                                  -> 
                                          //  printVars (signAnalyzer (Map.toList varMap) [])
                                          //  printSignLists signs
                                            printfn "%s" ("Terminated in " + string(programSteps-n) + "\n" + (printVarlist (Map.toList varMap)))
                                            

(*---------------------THE SIGN ANALYSIS PART 2---------------------*)

let rec signs (aexpr:AExpr) (varSigns:Map<string,string>) : string = 
    match aexpr with
    | Var (s)                   -> if varSigns.ContainsKey s then Map.find s varSigns else failwith (string(s)+" not_found")

    | Num (num)                 -> match num with   
                                    | x when x>0.0 -> "+"
                                    | x when x<0.0 -> "-"
                                    | _ -> "0"

    | TimesExpr(ex1,ex2)        ->  if (((signs ex1 varSigns)="0") || ((signs ex2 varSigns)="0")) then "0" 
                                    else if ((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="-") then "+"
                                    else if ((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="-") || ((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="+") then "-"
                                    else "+"

    | DivExpr(ex1,ex2)          ->  if ((signs ex1 varSigns)="0")  then "0" 
                                    else if (((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="-")) then "+"
                                    else if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="-") || ((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="+")) then "-"
                                    else "+"
                                    
    | PlusExpr(ex1,ex2)         ->  if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="+")) then "+"
                                    else if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="0")) then "+"
                                    else if (((signs ex1 varSigns)="0") && ((signs ex2 varSigns)="+")) then "+"
                                    else if (((signs ex1 varSigns)="0") && ((signs ex2 varSigns)="-")) then "-"
                                    else if (((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="0")) then "-"
                                    else if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="-")) then "-0+"
                                    else "-0+"  

    | MinusExpr(ex1,ex2)        ->  if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="+")) then "-0+"
                                    else if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="0")) then "+"
                                    else if (((signs ex1 varSigns)="0") && ((signs ex2 varSigns)="+")) then "-"
                                    else if (((signs ex1 varSigns)="0") && ((signs ex2 varSigns)="-")) then "+"
                                    else if (((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="0")) then "-"
                                    else if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="-")) then "+"
                                    else "+-0"

    | PowExpr(ex1,ex2)          ->  if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="+")) then "+"
                                    else if (((signs ex1 varSigns)="+") && ((signs ex2 varSigns)="-")) then "+"
                                    else if (((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="+")) then "-+"
                                    else if (((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="-")) then "-"
                                    else if (((signs ex1 varSigns)="0") && ((signs ex2 varSigns)="+")) then "0"
                                    else if (((signs ex1 varSigns)="-") && ((signs ex2 varSigns)="0")) then "+"
                                    else "+"

    
    | Neg(ex1)                  ->  if ((signs ex1 varSigns)="+") then "-" else "+"     //???????


(*---------------------THE SECURITY ANALYSIS---------------------*)

///<summary>
/// The function permutations generates of possible combinations of security flow 
/// for our given variables in the given GCL program
///</summary>

let rec combinationsgenerator x xs  =
    match xs with   
        | y::ys -> (x,y)::(y,x)::(combinationsgenerator x ys)
        | [] -> [(x,x)]

let rec permutations list result = 
    match list with
        | x::xs -> permutations xs (result@(combinationsgenerator x xs))
        | [] -> result

let rec notAllowedgenerator x xs  =
    match xs with   
        | (a,b)::ys -> (x,a)::(notAllowedgenerator x ys)
        | [] -> []

let rec notAllowed (varSeclist:(string *string) list) (securityLattice:string) result=
    let s=securityLattice.IndexOf("<")
    let securitylattice1=securityLattice.[s+1..String.length securityLattice]
    match varSeclist with 
        | (a,b)::xs when b=securitylattice1 -> notAllowed xs securityLattice result@notAllowedgenerator a xs
        | (a,b)::xs -> notAllowed xs securityLattice result
        | [] -> result
        
let rec Allowed (notAllowedlist:(string *string) list) (posibilitieslist:(string *string) list)=
    
    match notAllowedlist with   
        | (a,b)::xs -> Allowed xs (List.map (fun (elm1,elm2) ->  if (elm1,elm2)=(a,b) then ("","") else (elm1,elm2)) posibilitieslist)
        | [] -> posibilitieslist

let rec Difflist = function
    | ("","")::xs -> Difflist xs
    | x::xs -> x::(Difflist xs)
    | [] -> []
//Printfunction

let rec printlist list = 
        match list with
        | (var,var2)::tail when (var,var2)=("","") -> "" + printlist tail
        | (var,var2)::tail -> var + "->" + var2 + "   " + printlist tail
        | _ -> ""


let matchstring (strMatch:string) =
  let r = Regex("Var \".\"")
  let ms = r.Matches strMatch
  let mutable result=[]
  for i in 0..ms.Count-1 do
    result<-(string(ms.Item i)).Replace("\"","")::result
  result

let rec Assignmatch (x:string) (str:string list) =
    match str with
        | y::ys -> (y.Replace("Var ", ""),(x.Replace("Var ", "")))::(Assignmatch x ys)
        | [] -> []

let rec BoolActualGenerator (graphlist:OperationList) (e1: string) (e2: string)=
    
    match graphlist with
        | (num,op)::xs -> 
                          match op with                           
                            | Assign(_,(x,expr)) -> (Assignmatch x (matchstring e1))@(Assignmatch x (matchstring e2))@BoolActualGenerator xs e1 e2 
                            | Boolean(_,expr) -> BoolActualGenerator xs e1 e2
                            | SkipCMD(_) -> BoolActualGenerator xs e1 e2
        | [] -> []

let rec Actual (graph:OperationList) = 
    match graph with
        | (num,op)::xs -> match op with
                            | Assign(_,(x,expr)) -> (Assignmatch (x) (matchstring (string(expr))))@(Actual xs) 
                            | Boolean(_,expr) -> match expr with
                                                 | Eq (e1,e2) | Neq (e1,e2) | Gt (e1,e2) | Gte (e1,e2) | Lte (e1,e2) | Lt (e1,e2) -> 
                                                    (BoolActualGenerator graph (string(e1))  (string(e2)))@(Actual xs) 
                                                 | _ -> (Actual xs) 
                            | SkipCMD(_) -> (Actual xs) 
        | [] -> []

(*----------------------MODEL CHECKING---------------------*)
let rec Reach1 state (graphlist:OperationList) = 
    match graphlist with    
        | (x,op)::xs when x=state -> match op with
                                        | Assign(n,_) -> n::(Reach1 state xs)
                                        | Boolean(n,_) -> n::(Reach1 state xs)
                                        | SkipCMD(n) -> n::(Reach1 state xs)
        | (x,op)::xs -> (Reach1 state xs)
        | [] -> []

let ModelCheckGCL (graphlist: OperationList) =
    let mutable Visited = []
    let mutable ToExplore = [fst (graphlist.Head)]
    while (not ToExplore.IsEmpty) do 
        let s=ToExplore.Head
        //printfn "s: %A" s
        if (List.exists (fun elem -> elem = s) Visited) then 
            ToExplore <- ToExplore.Tail
        else 
            ToExplore <- ToExplore.Tail
            Visited   <- s::Visited
            let Reachstate=(Reach1 s graphlist)
            //printfn "%A" Reachstate
            if (s=(-1)) then printfn "Only 'Stuck state' is %A, which is the final state" s 
            if  Reachstate.IsEmpty && not (s=(-1))  then 
                printfn "Stuck state for state %A" s 
            else    
                for elm in Reachstate do 
                    ToExplore <- elm::ToExplore
               // printfn "Toexp: %A" ToExplore
    //printfn "Visited: %A" Visited

(*---------------------THE GCL PROGRAM---------------------*)

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
    let programSteps = 35
    
    //Variable number (IF MORE VARIABLES NEEDED ADD HERE)  
    let varMap = Map.empty.Add("x",1.0).Add("y",1.0)

    let varSigns = Map.empty.Add("x","+").Add("y","+")

    //Security Lattice (CHANGE IF NEEDED e.g. low<high or trusted<dubious)
    let securityLattice="public<private"
    //Security classification
    let varSec= Map.empty.Add("x","private").Add("y","public")

    printf "\n----------------------------\nInput program:\n\n%s\n----------------------------\n" input

    try
        //Allowed security status
        let sst= (Map.toList varSec)
        let noallowed=(notAllowed sst securityLattice [])
        let possib=(permutations (Varlist sst []) [])

        let allowed= Allowed noallowed possib

        //Parse program 

        let programGCL  = parse (input)
        let oplist = (evalC programGCL 0 (-1))
        let pg = ProgramGraph (programGCL)
        let wr = new System.IO.StreamWriter("graphdraw.gv")
        wr.Write pg
        wr.Close() 

       // for elm in oplist do
       //     printfn "%A" elm

        //MODEL CHECKING
        printfn"MODEL CHECKING"
        ModelCheckGCL oplist
        printfn"\n"

    
        //Actual security status
        let acc = List.distinct (Actual oplist)
        //Violations in security
        let violations=Difflist (Allowed allowed acc)
        //Result security
        let securegcl= if (List.isEmpty violations) then "secure" else "not secure"

        printf "----------------------------\n"

        (Interpreter qi programSteps oplist varMap programSteps [] oplist)

        printf "----------------------------\n"
        printfn"SECURITY ANALYSIS"
        printfn"Actual    : %A" (printlist acc)
        printfn"Allowed   : %A" (printlist allowed)
        printfn"Violations: %A" (printlist violations)
        printfn""
        printfn"Result    : %A" (securegcl)
        printfn""
        printf "----------------------------\nProgram accepted!\n----------------------------\n"

    with err -> 
        printf "Program rejected!\n"