class Main inherits IO {
  a : A <- new A;
  x : Int;
  main() : Object {{
    x <- in_int();
    self.out_string("The input is: ");
    out_int(x);
  }} ;
} ; 

class A inherits IO {
  n : Int <- {
    out_string("breaking changes\n");
    2;
  };
  a : Int <- {
    out_string("aaaaaaaaaaaaa changes\n");
    2;
  };
};