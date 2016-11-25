import java.util.Collection;

public class SimpleArithmeticParser {
    Lexer lexer;
    Collection<Token> tokens;
    
    public SimpleArithmeticParser(String source) {
	lexer = new Lexer(source);
	tokens = lexer.getTokens();
	
    }
    
    
}
