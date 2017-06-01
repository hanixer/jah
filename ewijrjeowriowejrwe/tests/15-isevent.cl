class Main inherits IO {
  main() : Object { {
    out_string("HUEHURHUEHRUHEUHRIUEHURE");
    self.out_string("Hello!\n");
    if is_even(x) then
        out_string("x is even.\n")
    else
        out_string("x is not even.\n")
    fi;
  }} ;

  is_even(num : Int) : Bool {
      (let x : Int <- num in
            if x < 0 then is_even(~x) else
            if 0 = x then true else
	    if 1 = x then false else
	          is_even(x - 2)
	    fi fi fi
      )
   };

   x : Int <- 2000;
} ; 
