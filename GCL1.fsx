#r "/home/rasmus/programming/GCL1/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCL1Parser.fs"
open GCL1Parser
#load "GCL1Lexer.fs"
open GCL1Lexer

open System.IO

let parse input =

    try
        let lexbuf = LexBuffer<char>.FromString input

        Some(GCL1Parser.start GCL1Lexer.tokenize lexbuf)

    with |Failure msg->
        None
    

let GCL =  
    let file = File.ReadAllText("input.txt")
    printf "input:\n\n%s\n" file
    let program : Command option = parse (file)
    match program with 
        | None -> printf "ko\n"
        | Some(p) -> 
            printf("syntax ok\n")