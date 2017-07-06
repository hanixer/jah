package frontend.lis;

public enum ErrorType {
    WRONG_CHAR("Wrong char literal"),
    INVALID_STRING_LITERAL("Invalid string literal, it must end with '\"'"),
    INVALID_INTEGER("Invalid integer literal"),
    INVALID_REAL("Invalid real literal"),
    INVALID_SPECIAL_SYMB("Invalid special symbol"),
    INVALID_COMMENT("Comment must be closed"),
    NONEXPECTED_TOKEN("Was expected"),
    MISSING_LPAREN("'(' is missing"),
    MISSING_RPAREN("')' is missing"),
    MISSING_LBRACKET("'{' is missing"),
    MISSING_RBRACKET("'}' is missing"),
    MISSING_STMT_SEPAR("';' or new line is missing after statement"),
    MISSING_STMT("Statement (list) is missing"),
    MISSING_WHILE("'while' is missing in 'do' statement"), 
    MISSING_CONDITION("Condition is missing"),
    MISSING_ID_VAR("Missing identifier after 'var'"), 
    NAME_REDEFINITION("Redefinition of name"), MISSING_COLON, MISSING_TYPENAME, UNDEFINED_TYPE,
    ;
    
    private String text;
    
    ErrorType() {
	text = "";
    }

    ErrorType(String text) {
	this.text = text;
    }
    
    public String getText() {
        return text;
    }
}
