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
                                            printVars (signAnalyzer (Map.toList varMap) [])
                                            printSignLists signs
                                            //printSignLists [(programSteps,signAnalyzer (Map.toList varMap) [])]

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
                                            | Assign(s,(var,ex))    -> Interpreter s (n-1) oplist (varMap.Add(var,(GAExpr ex varMap))) programSteps (signs@st) oplist
                                            | SkipCMD(s)            -> Interpreter s (n-1) oplist varMap programSteps (signs@st) oplist
    | _                                  -> 
                                            printVars (signAnalyzer (Map.toList varMap) [])
                                            printSignLists signs
                                            // printSignLists [(programSteps,signAnalyzer (Map.toList varMap) [])]
                                            //let str=(signAnalyzer (Map.toList varMap) 
                                            //(combinations [] List.length str)
                                            printfn "%s" ("Terminated in " + string(programSteps-n) + "\n" + (printVarlist (Map.toList varMap)))
                                            
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
        | y::ys -> (y.Replace("Var ", ""),(y.Replace("Var ", "")))::(y.Replace("Var ", ""),(x.Replace("Var ", "")))::(Assignmatch x ys)
        | [] -> [(x.Replace("Var ", ""),x.Replace("Var ", ""))]

let rec Boolmatch str str1 =
    match str with 
        | y::ys -> (Assignmatch y str1)@(Boolmatch ys str1)
        | [] -> []    


let ActualGenerator (graph:Operation) =                                 
    match graph with    
        | Assign(_,(x,expr)) ->  (Assignmatch (x) (matchstring (string(expr)))) 
                        
        | Boolean(_,expr) -> match expr with
                                | Eq (e1,e2) | Neq (e1,e2) | Gt (e1,e2) | Gte (e1,e2) | Lte (e1,e2) | Lt (e1,e2) -> 
                                       (Boolmatch (matchstring (string(e1))) (matchstring (string(e2))))    
                                | _ -> []
        | SkipCMD _ -> []

let rec searchBool (graph:OperationList) (num:int) =
      match graph with 
        | (i,Boolean(_))::xs when i=num   -> []
        | (i,Boolean(x,y))::xs when i<>num-> (Boolean(x,y))::(searchBool xs num)
        |  (a,b)::xs                      -> b::(searchBool xs num)
        | []                              -> []

let rec Listelm = function
    | (a,b)::tail -> a::b::(Listelm tail)
    | [] -> []
let rec matchListelm  elm list = 
    match list with
        | x::xs -> (elm,x)::(matchListelm elm xs)
        | [] -> []
let rec matchListList fromlist tolist =
    match (fromlist) with
        | x::xs -> (matchListelm x (Listelm tolist))@(matchListList xs tolist)
        | [] -> []

let rec evaloplist = function
    | x::xs -> (ActualGenerator  x)@(evaloplist xs)
    | [] -> []

let rec findnum num res = function
    | (i,op)::xs when num=i -> res@xs
    | (i,op)::xs -> findnum num (res@[(i,op)]) xs
    | [] -> res

let rec Actual (graph:OperationList) =
    match graph with
        | (num,op)::xs -> match op with
                           | Assign(_) ->  (ActualGenerator op)@(Actual xs)              
                           | Boolean(_) -> (ActualGenerator op)@(matchListList (Listelm (ActualGenerator ((op)))) (evaloplist (searchBool xs num)))@(Actual (findnum num [] xs))
                           | SkipCMD(_) -> (Actual xs)    

        | [] -> []



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
    //printf "Input a string: "
    //let input = (Console.ReadLine())
    let input = File.ReadAllText("TestFile1.txt")

    //Initial node qi
    let qi = 0
    //Number of programSteps
    printfn"Choose number of programSteps:"
    //let programSteps = int (Console.ReadLine())
    let programSteps = 20
    
    //Variable number (IF MORE VARIABLES NEEDED ADD HERE  )  
    let varMap = Map.empty.Add("x",2.0)
    printf "\n----------------------------\nInput program:\n\n%s\n----------------------------\n" input


    //Security Lattice (CHANGE IF NEEDED e.g. low<high or trusted<dubious)
    let securityLattice="public<private"
    //Security classification
    let varSec= Map.empty.Add("x","private").Add("y","public")

    //Allowed security status
    let sst= (Map.toList varSec)
    let noallowed=(notAllowed sst securityLattice [])
    let possib=(permutations (Varlist sst []) [])

    let allowed= Allowed noallowed possib

    try
       let programGCL  = parse (input)
       let oplist = (evalC programGCL 0 (-1))
       let pg = ProgramGraph (programGCL)
       let wr = new System.IO.StreamWriter("graph.gv")
       wr.Write pg
       wr.Close() 

       //Actual security status
       let acc = List.distinct (Actual oplist)
       
       //Violations in security
       let violations=Difflist (Allowed acc allowed)

    
       //Result security
       let securegcl= if (List.isEmpty violations) then "secure" else "not secure"

       (Interpreter qi programSteps oplist varMap programSteps [] oplist)

       printfn"Allowed   : %A" (printlist allowed)
       printfn"Actual    : %A" (printlist acc)
       printfn"Violations: %A" (printlist violations)
       printfn""
       printfn"Result    : %A" (securegcl)
       printfn""
       printf "----------------------------\nProgram accepted!\n----------------------------\n"
     
    with err -> 
        printf "Program rejected!\n"


         