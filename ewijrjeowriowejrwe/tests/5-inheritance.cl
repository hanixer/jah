class Main inherits IO {
  main() : Object { 
    self.out_string("Hello, world.\n") 
  } ;
} ; 

class A {
  firstly() : String {"Gogogo"};
  backward() : Int {555};
};

class B inherits A {
  backward() : Int {654};
};

class C inherits B {
  firstly() : String {"And this is overloaded"};
  classCOwnMethod() : String {"This is my method"};
};