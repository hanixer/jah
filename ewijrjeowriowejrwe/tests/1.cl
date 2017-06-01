class Main inherits IO {
  main() : Object { {
    1;
  } } ;
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