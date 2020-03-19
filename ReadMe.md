https://github.com/rasmusmm/datamod

In order to generate the lexer run the following command:

mono FsLexYacc.10.0.0/build/fslex/net46/fslex.exe GCL1Lexer.fsl --unicode

In order to generate the parser run the following command:

mono FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe GCL1Parser.fsp --module GCL1Parser

For more detailed guide check: https://gitlab.gbar.dtu.dk/02141/mandatory-assignment/blob/master/getting-started-fs.md

There are four key files to inspect; GCL1Lexer.fsl, GCL1Parser.fsp, GCL1.fsx and GCL1TypeAST.fs. These control the lexing, parsing, running the script and the types.

To run simply write fsharpi GCL1.fsx in the terminal and then write your command in the console. 

The Program graph will then be written (and to be displayed) in the graph.gv file

