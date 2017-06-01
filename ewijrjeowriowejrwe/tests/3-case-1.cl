class Main inherits IO {
  main() : Object { 
    let x : Int <- 1
    in x + 1
    -- self@Main.out_int(2) 
  } ;
  f() : Object {
    case var of
	 a : A => out_string("Class type is now A\n");
	 o : Object => out_string("Oooops\n");
      esac
  };
} ; 

class A {};