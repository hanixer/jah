package frontend.lis.tokens;

import static frontend.lis.LisTokenType.*;
import static frontend.lis.ErrorType.*;

import frontend.Source;
import frontend.Token;
import frontend.lis.LisTokenType;

public class SpecialSymbolToken extends Token {

    public SpecialSymbolToken(Source source) throws Exception {
	super(source);
    }

    @Override
    protected void extract() throws Exception {
	char ch = currentChar();

	switch (ch) {
	case '{':
	    makeTokenAndAdvance(L_BRACKET);
	    break;
	case '}':
	    makeTokenAndAdvance(R_BRACKET);
	    break;
	case '[':
	    makeTokenAndAdvance(L_SQUARE_BRACKET);
	    break;
	case ']':
	    makeTokenAndAdvance(R_SQUARE_BRACKET);
	    break;
	case '(':
	    makeTokenAndAdvance(L_PAREN);
	    break;
	case ')':
	    makeTokenAndAdvance(R_PAREN);
	    break;
	case ';':
	    makeTokenAndAdvance(SEMICOLON);
	    break;
	case ':':
	    makeTokenAndAdvance(SEMICOLON);
	    break;
	case '?':
	    makeTokenAndAdvance(QUESTION);
	    break;
	case '.':
	    nextChar();
	    if (currentChar() == '.') {
		nextChar();
		if (currentChar() == '.') {
		    makeTokenAndAdvance(ELIPSIS);
		} else {
		    type = ERROR;
		    value = INVALID_SPECIAL_SYMB;
		}
	    } else {
		makeTokenAndAdvance(DOT);
	    }
	    break;
	case '+':
	    readSingleDoubleAssign(PLUS_PLUS, PLUS_ASSIGN, PLUS);
	    break;
	case '-':
	    readSingleDoubleAssign(MINUS_MINUS, MINUS_ASSIGN, MINUS);
	    break;
	case '*':
	    readSingleAssign(STAR_ASSIGN, STAR);
	    break;
	case '/':
	    readAfterSlash();
	    break;
	case '%':
	    readSingleAssign(PERCENT_ASSIGN, PERCENT);
	    break;
	case '^':
	    readSingleAssign(HAT_ASSIGN, HAT);
	    break;
	case '&':
	    readSingleDoubleAssign(AMPER_AMPER, AMPERSAND, AMPERSAND_ASSIGN);
	    break;
	case '|':
	    readSingleDoubleAssign(BAR_BAR, BAR_ASSIGN, BAR);
	    break;
	case '~':
	    makeTokenAndAdvance(TILDE);
	    break;
	case '!':
	    readSingleAssign(NOT_EQUAL, EXCLAM);
	    break;
	case '=':
	    readSingleAssign(EQUAL, ASSIGN);
	    break;
	case ',':
	    makeTokenAndAdvance(COMMA);
	    break;
	case '<':
	    readAngleBracket(SHIFT_LEFT, SHIFT_LEFT_ASSIGN, LESS_THEN, LESS_EQ);
	    break;
	case '>':
	    readAngleBracket(SHIFT_RIGHT, SHIFT_RIGHT_ASSIGN, GREATER_THEN, GREATER_EQ);
	    break;
	case Source.EOL:
	    makeTokenAndAdvance(LINE);
	    break;
	default:
	}
    }

    public void readSingleDoubleAssign(LisTokenType doubleOp, LisTokenType assignOp, LisTokenType singleOp)
	    throws Exception {
	char ch = currentChar();
	nextChar();
	if (currentChar() == ch) {
	    makeTokenAndAdvance(doubleOp);
	} else if (currentChar() == '=') {
	    makeTokenAndAdvance(assignOp);
	} else {
	    type = singleOp;
	}
    }

    public void readAngleBracket(LisTokenType doubleOp, LisTokenType doubleAssign, LisTokenType singleOp,
	    LisTokenType singleAsign) throws Exception {
	char ch = currentChar();
	nextChar();
	if (currentChar() == ch) {
	    if (nextChar() == '=') {
		makeTokenAndAdvance(doubleAssign);
	    } else {
		type = doubleOp;
	    }
	} else if (currentChar() == '=') {
	    makeTokenAndAdvance(doubleAssign);
	} else {
	    type = singleOp;
	}
    }

    private void readAfterSlash() throws Exception {
	char ch = nextChar(); 
	    if (ch == '*' || ch == '/') {
		scanComment();
	    }
	    else if (ch == '=') {
		makeTokenAndAdvance(SLASH_ASSIGN);
	    }

	if (type == null) {
	    type = SLASH;
	}
	}

    private void scanComment() throws Exception {
	if (currentChar() == '*') {
	    boolean isCommentEndFound = false;
	    char ch = nextChar();
	    while (true) {
		if (ch == '*' && peekChar() == '/') {
		    isCommentEndFound = true;
		    break;
		} else if (ch == Source.EOF) {
		    break;
		}

		ch = nextChar();
	    }

	    if (isCommentEndFound) {
		type = COMMENT;
	    }
	    else {
		type = ERROR;
		value = INVALID_COMMENT;
	    }
	} else {
	    while (currentChar() != Source.EOL) {
		nextChar();
	    }

	    type = COMMENT;
	}	
    }

    private void readSingleAssign(LisTokenType starAssign, LisTokenType star) throws Exception {
	if (nextChar() == '=') {
	    makeTokenAndAdvance(starAssign);
	} else {
	    type = star;
	}
    }

    private void makeTokenAndAdvance(LisTokenType tokType) throws Exception {
	type = tokType;
	nextChar();
    }
}
