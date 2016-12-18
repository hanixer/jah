package frontend.clike;

public enum ErrorType {
    WRONG_CHAR("Wrong char literal"),
    INVALID_STRING_LITERAL("Invalid string literal, it must end with '\"'"),
    INVALID_INTEGER("Invalid integer literal"),
    INVALID_REAL("Invalid real literal"),
    INVALID_SPECIAL_SYMB("Invalid special symbol"),
    INVALID_COMMENT("Comment must be closed"),
    NONEXPECTED_TOKEN("Was expected"),
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
