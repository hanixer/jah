#load "Display.fs"
#load "Queue.fs"
#load "Tac.fs"

open Display
open Tac

let p5 = "
a <- int 33
bt b tilend
y <- b
label tilend
x <- + y a
return x"

let processStr s =
    readTac s |> eliminateDeadInStmts |> showStmts |> iDisplay

let transformToGraphAndBack s =
    readTac s |> stmtsToBlocks |> blocksToGraph |> graphToStmts |> showStmts |> iDisplay

let asrt n (input:string) (expected:string) =
    let input = input.Trim()
    let expected = expected.Trim()
    try
        let p = (processStr input).Trim()
        if p <> expected then
            printfn "FAILED #%d! \nExpected [\n%s] \n\nbut got [\n%s]\n\n" n expected p
    with exc ->
        printfn "FAILED #%d! Exception [%s] in [%s]" n exc.Message input
    

let testcases() =
    asrt 1 p5 p5
    asrt 2 "
a <- int 33
bt b tilend
y <- b
label tilend
x <- + y b
return x" 
        "
bt b tilend
y <- b
label tilend
x <- + y b
return x"

    asrt 3 "
a <- call in_int
bt b tilend
y <- b
label tilend
x <- + y b
return x"
        "
a <- call in_int
bt b tilend
y <- b
label tilend
x <- + y b
return x"

    asrt 4 "
i <- int 555
a <- call in_int
bt b tilend
y <- b
label tilend
x <- i
x <- + y b
return x"
        "
a <- call in_int
bt b tilend
y <- b
label tilend
x <- + y b
return x"

    asrt 5 "
a <- int 1
b <- int 2
bt cond use_b
x <- a
jmp end
label use_b
x <- b
label end
return x"
        "
a <- int 1
b <- int 2
bt cond use_b
x <- a
jmp end
label use_b
x <- b
label end
return x"
    asrt 6 "
label sta
a <- call in_int
bt boo loo
return b
label loo
b <- + a 1
jmp sta"
        "
label sta
a <- call in_int
bt boo loo
return b
label loo
b <- + a 1
jmp sta"
    
    asrt 777 "
comment start
label CellularAutomaton_evolve_0
t$1 <- default Int
t$2 <- call num_cells
t$3 <- default String
jmp CellularAutomaton_evolve_3
comment while-pred
label CellularAutomaton_evolve_3
t$6 <- t$1
t$7 <- t$2
t$5 <- < t$6 t$7
t$16 <- not t$5
bt t$16 CellularAutomaton_evolve_4
bt t$5 CellularAutomaton_evolve_5
comment while-join
label CellularAutomaton_evolve_4
t$4 <- default Object
population_map <- t$3
t$15 <- population_map
t$0 <- self
return t$0
comment while-body
label CellularAutomaton_evolve_5
t$11 <- t$1
t$10 <- call cell_at_next_evolution t$11
t$12 <- t$3
t$3 <- call concat t$10
t$9 <- t$3
t$13 <- t$1
t$14 <- int 1
t$1 <- + t$13 t$14
t$8 <- t$1
jmp CellularAutomaton_evolve_3"
        "
comment start
label CellularAutomaton_evolve_0
t$1 <- default Int
t$2 <- call num_cells
t$3 <- default String
jmp CellularAutomaton_evolve_3
comment while-pred
label CellularAutomaton_evolve_3
t$6 <- t$1
t$7 <- t$2
t$5 <- < t$6 t$7
t$16 <- not t$5
bt t$16 CellularAutomaton_evolve_4
bt t$5 CellularAutomaton_evolve_5
comment while-join
label CellularAutomaton_evolve_4
population_map <- t$3
t$0 <- self
return t$0
comment while-body
label CellularAutomaton_evolve_5
t$11 <- t$1
t$10 <- call cell_at_next_evolution t$11
t$12 <- t$3
t$3 <- call concat t$10
t$13 <- t$1
t$14 <- int 1
t$1 <- + t$13 t$14
jmp CellularAutomaton_evolve_3"

let p6 = "
comment start
label Main_main_0
t$1 <- int 1
t$2 <- call in_int
t$7 <- t$1
t$8 <- int 3
t$3 <- + t$7 t$8
t$11 <- t$1
t$12 <- t$2
t$10 <- < t$11 t$12
t$18 <- not t$10
bt t$18 Main_main_4
bt t$10 Main_main_3
comment then branch
label Main_main_3
t$14 <- t$3
t$15 <- int 6
t$13 <- + t$14 t$15
jmp Main_main_6
comment else branch
label Main_main_4
t$16 <- t$1
jmp Main_main_8
comment if-join
label Main_main_5
t$17 <- t$2
jmp Main_main_10
comment fcall-pre to out_int
label Main_main_6
_dead_ <- call out_int t$13
jmp Main_main_5
comment fcall-pre to out_int
label Main_main_8
_dead_ <- call out_int t$16
jmp Main_main_5
comment fcall-pre to out_int
label Main_main_10
t$0 <- call out_int t$17
return t$0
"

let p7 = "
a <- int 3
b <- int 4
bt foo bran
ans <- a
jmp end
label bran
ans <- b
label bran2
_dea <- call out_int ans
label bran3
_dea11111111 <- call out_int ans
label end
return ans
"

let p8= "
bt b nope
a <- 1
label end
return a
label nope
a <- 2
jmp end"

//asrt 223 p7 p7
//readTac p7 |> stmtsToBlocks
transformToGraphAndBack p7