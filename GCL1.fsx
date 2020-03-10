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

let parse input =

    try
        let lexbuf = LexBuffer<char>.FromString input

        Some (GCL1Parser.start GCL1Lexer.tokenize lexbuf)

    with |Failure msg-> 
        None
        
    

let GCL =  
    let file = File.ReadAllText("input.txt")
    printf "Input:\n\n%A\n\n" file
    let programGCL : Command option = parse (file)
    match programGCL with 
        | None -> printf "Program rejected\n"
        | Some(p) -> 
            printf("Program accepted\n")