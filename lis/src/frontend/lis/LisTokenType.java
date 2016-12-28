package frontend.lis;

import frontend.TokenType;

public enum LisTokenType implements TokenType {
    // Literals
    IDENTIFIER, CHAR, STRING, INTEGER, REAL,
    
    ERROR,
    LINE,
    COMMENT,
    EOF,
    
    L_BRACKET, R_BRACKET, L_SQUARE_BRACKET, R_SQUARE_BRACKET, HASH, HASH_HASH, L_PAREN, R_PAREN, SEMICOLON, COLON, QUESTION, COLON_COLON, DOT, DOT_STAR, ELIPSIS, PLUS, MINUS, STAR, SLASH, PERCENT, HAT, AMPERSAND, BAR, TILDE, EXCLAM, ASSIGN, LESS_THEN, GREATER_THEN, PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN, PERCENT_ASSIGN, HAT_ASSIGN, AMPERSAND_ASSIGN, BAR_ASSIGN, SHIFT_LEFT, SHIFT_RIGHT, SHIFT_RIGHT_ASSIGN, SHIFT_LEFT_ASSIGN, EQUAL, NOT_EQUAL, LESS_EQ, GREATER_EQ, AMPER_AMPER, BAR_BAR, PLUS_PLUS, MINUS_MINUS, COMMA, ARROW_STAR, ARROW,
    
    // Keywords
    IF("if"),
    ELSE("else"),
    FOR("for"),
    DO("do"),
    WHILE("while"),
    TRUE("true"),
    FALSE("false"),
    VAR("var"),
    RETURN("return"),
    ;
    
    String text;
    private static LisTokenType kwFirst = IF;
    private static LisTokenType kwLast = RETURN;
    
    private LisTokenType() {
	this.text = "";
    }
    
    private LisTokenType(String text) {
	this.text = text;
    }
    
    public static LisTokenType getIdentifierOrKeyword(String text) {
	for (int i = kwFirst.ordinal(); i <= kwLast.ordinal(); ++i) {
	    if (values()[i].text.equals(text)) {
		return values()[i];
	    }
	}
	return IDENTIFIER;
    }
}
