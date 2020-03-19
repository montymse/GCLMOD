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

///<summary>
/// The function GCL implements the parser and checks the syntax 
///</summary>
///<returns>
/// It either returns 'program rejected' if the syntax is incorrect otherwise 'program accepted'
///</returns>
///<remarks>
/// As input you can either type something into the console by using the command Console.ReadLine()
/// or use the test file "TestFile.txt" by using the command File.ReadAllText("TestFile.txt")
/// By default we use Console.ReadLine() 
///</remarks>
/// 

let GCL =  

    let input = Console.ReadLine() 
    //let input = File.ReadAllText("TestFile1.txt")
    //printf "input:\n\n%s\n" input

    try
       let programGCL  = parse (input)
       let pg = ProgramGraph (programGCL)
       let wr = new System.IO.StreamWriter("graph.gv")
       wr.Write pg
       wr.Close() 

       printf "Program accepted!\n"

    with err -> 
        printf "Program rejected!\n"

        
