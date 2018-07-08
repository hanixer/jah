type Term = 
    | Var of string
    | Fn of string * Term list

type Fol = R of string * Term list
