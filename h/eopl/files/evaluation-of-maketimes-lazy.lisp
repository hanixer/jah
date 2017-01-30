let makerec = proc (f)
let d = proc (x) (f (x x))
in (f (d d))
in let maketimes4 = proc (f)
proc (x)
if zero?(x)
then 0
else -((f -(x,1)), -4)
in let times4 = (makerec maketimes4)
in (times4 3)

(times4 3)
--------------------------------------------------------------------------------
((makerec maketimes4) 3)
--------------------------------------------------------------------------------
(([proc (f) let d = proc (x) (f (x x)) in (f (d d))] maketimes4) 3)
--------------------------------------------------------------------------------
(let d = proc (x) (maketimes4 (x x)) in (maketimes4 (d d)) 3) replace d
--------------------------------------------------------------------------------
((maketimes4 (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x)))) 3) replace first maketimes4
--------------------------------------------------------------------------------
((proc (f) proc (x) if zero?(x) then 0 else -((f -(x,1)), -4) (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x)))) 3) apply proc taking `f`
--------------------------------------------------------------------------------
(proc (x) if zero?(x) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(x,1)), -4) 3)  apply proc(x)
--------------------------------------------------------------------------------
(if zero?(3) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(3,1)), -4) eval if
--------------------------------------------------------------------------------
-(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(3,1)), -4) apply proc (x)
--------------------------------------------------------------------------------
-((((maketimes4 (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))))  -(3,1)), -4) substitue maketimes4
--------------------------------------------------------------------------------
-(((proc (f) proc (x) if zero?(x) then 0 else -((f -(x,1)), -4) (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))))  -(3,1)), -4) apply proc(f)
--------------------------------------------------------------------------------
-((proc (x) if zero?(x) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(x,1)), -4)  -(3,1)), -4) apply proc(x)
--------------------------------------------------------------------------------
-(if zero?(-(3,1)) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(-(3,1),1)), -4), -4) eval if
--------------------------------------------------------------------------------
-(-((((maketimes4 (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))))) -(-(3,1),1)), -4), -4) substitue maketimes4
--------------------------------------------------------------------------------
-(-((((proc (f) proc (x) if zero?(x) then 0 else -((f -(x,1)), -4) (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))))) -(-(3,1),1)), -4), -4) apply proc(f)
--------------------------------------------------------------------------------
-(-((((proc (x) if zero?(x) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(x,1)), -4))) -(-(3,1),1)), -4), -4) apply proc(x)
--------------------------------------------------------------------------------
-(-((proc (x) if zero?(x) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(x,1)), -4) -(-(3,1),1)), -4), -4) apply proc(x)
--------------------------------------------------------------------------------
-(-(if zero?(-(-(3,1),1)) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(-(-(3,1),1),1)), -4), -4), -4) eval if
--------------------------------------------------------------------------------
-(-(-(proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x)) -(-(-(3,1),1),1), -4), -4), -4) apply proc(x)
--------------------------------------------------------------------------------
-(-(-((maketimes4 (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x)))) -(-(-(3,1),1),1), -4), -4), -4) substitue maketimes4
--------------------------------------------------------------------------------
-(-(-((proc (f) proc (x) if zero?(x) then 0 else -((f -(x,1)), -4) (proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x)))) -(-(-(3,1),1),1), -4), -4), -4) apply proc(f)
--------------------------------------------------------------------------------
-(-(-(proc (x) if zero?(x) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(x,1)), -4) -(-(-(3,1),1),1), -4), -4), -4) apply proc(x)
--------------------------------------------------------------------------------
-(-(-(if zero?(-(-(-(3,1),1),1)) then 0 else -(((proc (x) (maketimes4 (x x)) proc (x) (maketimes4 (x x))) -(-(-(-(3,1),1),1),1)), -4), -4), -4), -4) eval if
--------------------------------------------------------------------------------
-(-(-(0, -4), -4), -4), -4)
