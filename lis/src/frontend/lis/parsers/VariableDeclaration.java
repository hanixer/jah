package frontend.lis.parsers;

import frontend.Scanner;
import frontend.TokenType;
import frontend.lis.ErrorType;
import frontend.lis.LisParser;
import frontend.lis.LisTokenType;
import intermediate.ICodeNode;
import intermediate.SymTabEntry;
import intermediate.TypeSpec;
import intermediate.symtabimpl.Predefined;
import intermediate.symtabimpl.SymTabKeyImpl;

public class VariableDeclaration extends DeclarationParser {

    public VariableDeclaration(Scanner scanner) {
	super(scanner);
	// TODO Auto-generated constructor stub
    }

    public VariableDeclaration(LisParser lisParser) {
	super(lisParser);
    }

    public void parse(TokenType tokenType) throws Exception {
	nextToken();
	
	if (currentToken().getType() != LisTokenType.IDENTIFIER) {
	    flag(currentToken(), ErrorType.MISSING_ID_VAR);
	    synchronize(DECL_SYNC_SET);
	    return;
	}
	
	String name = currentToken().getText();
	nextToken();
	
	if (symTabStack.lookupLocal(name) != null) {
	    flag(currentToken(), ErrorType.NAME_REDEFINITION);
	    synchronize(DECL_SYNC_SET);
	    return;
	}
	
	SymTabEntry nameTabEntry = symTabStack.enterLocal(name);
	
	consumeExpected(LisTokenType.COLON, ErrorType.MISSING_COLON);

	if (currentToken().getType() != LisTokenType.IDENTIFIER) {
	    flag(currentToken(), ErrorType.MISSING_TYPENAME);
	    synchronize(DECL_SYNC_SET);
	    return;
	}
	
	String typeName = currentToken().getText();
	nextToken();
	
	SymTabEntry typeTabEntry = symTabStack.lookup(typeName);
	TypeSpec type;
	if (typeTabEntry == null) {
	    flag(currentToken(), ErrorType.UNDEFINED_TYPE);
	    type = Predefined.undefinedType;
	} else {
	    type = typeTabEntry.getTypeSpec(); 
	}
	
	nameTabEntry.setTypeSpec(type);
	
	if (currentToken().getType() == LisTokenType.ASSIGN) {
	    nextToken();
	    
	    ICodeNode initExpr = new ExpressionParser(scanner).parseExpression();
	    nameTabEntry.setAttribute(SymTabKeyImpl.INIT_EXPRESSION, initExpr);
	}
    }
}
