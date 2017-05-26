#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#load "Graphs.fs"
#load "CoolAst.fs"
#load "CoolType.fs"
#load "Parser.fs"

open Parser
open FParsec

run (regex "\"[^\n]*\"") "\"123123\n123\" \n"