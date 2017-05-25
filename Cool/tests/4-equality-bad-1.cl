class Main inherits IO {
  main() : Object { 
    let x : Int <- 1
    in x + 1
    -- self@Main.out_int(2) 
  } ;
  f(x:Int, b:Int) : Object {
    x = self
  };
} ; 

class A {};