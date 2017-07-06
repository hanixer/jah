package frontend.lis.parsers;

import java.util.EnumSet;

import frontend.Scanner;
import frontend.Token;
import frontend.TokenType;
import frontend.lis.LisParser;
import frontend.lis.LisTokenType;

import static frontend.lis.LisTokenType.*;

public class DeclarationParser extends LisParser {
    private static EnumSet<LisTokenType> DECL_START_TOKS = 
	    EnumSet.of(VAR);
    
    protected static EnumSet<LisTokenType> DECL_SYNC_SET;
    
    static {
	DECL_SYNC_SET = EnumSet.of(LINE, SEMICOLON);
	DECL_SYNC_SET.addAll(DECL_START_TOKS);
    }
    

    public DeclarationParser(Scanner scanner) {
	super(scanner);
    }

    public DeclarationParser(LisParser lisParser) {
	super(lisParser);
    }

    public boolean isDeclarationToken(Token currentToken) {
	return DECL_START_TOKS.contains(currentToken.getType());
    }

    public void parse(TokenType tokenType) throws Exception {
	skipSemi();
	
	if (tokenType == VAR) {
	    new VariableDeclaration(this).parse(tokenType);
	}
	
	skipSemi();
    }

    
}
