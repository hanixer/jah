class Main inherits IO {
  main() : Object { {
    let x : Int <- 5,
        xw : IntWraper <- new IntWraper in {
      out_int(x);
      f(x);
      out_int(x);
      out_int(xw.get());
      g(xw);
      out_int(xw.get());
    };
  } } ;

  f(x : Int) : Object {{
    x <- x + 1;
    out_int(x);
  }};

  g(x : IntWraper) : Object {{
    x.incr();
    out_int(x.get());
  }};

} ; 

class Frayer {
  handle(wr : IntWraper) : Int {
    wr.incr()
  };
};

class IntWraper {
  x : Int <- 5;
  incr() : Int {x<-x+1};
  get() : Int {x};
};