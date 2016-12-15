package frontend.clike;

public enum ErrorType {
    WRONG_CHAR("Wrong char literal"),
    INVALID_STRING_LITERAL("Invalid string literal, it must end with '\"'"),
    INVALID_INTEGER("Invalid integer literal"),
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
