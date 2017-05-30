class Main inherits IO {
  main() : Object { {
    out_int(x);
    if x < 10 then
      out_string("x less 10\n")
    else
      out_string("x >= 10\n")
    fi;
    while x < 100 loop
    {
      out_int(x);
      out_string("backthere\n");
      x <- x + 1;
    }
    pool;
  } } ;
  x : Int;
} ; 
