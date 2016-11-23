import java.io.IOException;

public class Main {
    public static int f() {
        return 1;
    }
    public static void main(String[] args) {
        try {
	    String s = Util.readFile("f2.cpp");
	    Lexer.printTokensToFile(s, "out");
	} catch (IOException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}
    }
}
