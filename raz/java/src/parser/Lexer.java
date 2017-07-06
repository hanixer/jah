package parser;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;

//import Token.TokenKind;

/**
 * What's not supported: - user defined literals - trigraph sequences
 */
public class Lexer {
    final char[] source;
    final int sourceLen;
    int currPos;
    int savedPos;
    int tokStartPos;
    boolean isAtNewLine;
    boolean isPreprocessingDirective;
    boolean isIncludeDirective;
    Token token;

    public Lexer(String src) {
	source = src.toCharArray();
	sourceLen = source.length;
	currPos = -1;
	isAtNewLine = true;
	isPreprocessingDirective = false;
	isIncludeDirective = false;
	advance();
    }

    public ArrayList<Token> getTokens() {
	ArrayList<Token> result = new ArrayList<>();
	do {
	    getToken();
	    result.add(token);
	} while (token.kind != TokenKind.EOF);

	return result;
    }

    Token getToken() {
	token = null;

	while (token == null && !isAtEnd()) {
	    scan();
	}

	if (token == null) {
	    if (isPreprocessingDirective) {
		isPreprocessingDirective = false;
		token = new Token(TokenKind.EOD);
	    } else
		token = new Token(TokenKind.EOF);
	}

	return token;
    }

    private boolean isBeforeEnd() {
	return currPos < sourceLen;
    }

    private boolean isPosValidAndCharEquals(char c) {
	return currPos < sourceLen && source[currPos] == c;
    }

    private void scan() {
	tokStartPos = currPos;
	char c = peekChar();
	switch (c) {
	case '{':
	    makeTokenAndAdvance(TokenKind.L_BRACKET);
	    break;
	case '}':
	    makeTokenAndAdvance(TokenKind.R_BRACKET);
	    break;
	case '[':
	    makeTokenAndAdvance(TokenKind.L_SQUARE_BRACKET);
	    break;
	case ']':
	    makeTokenAndAdvance(TokenKind.R_SQUARE_BRACKET);
	    break;
	case '#':
	    scanAfterHash();
	    break;
	case '(':
	    makeTokenAndAdvance(TokenKind.L_PAREN);
	    break;
	case ')':
	    makeTokenAndAdvance(TokenKind.R_PAREN);
	    break;
	case ';':
	    makeTokenAndAdvance(TokenKind.SEMICOLON);
	    break;
	case ':':
	    scanColon();
	    break;
	case '?':
	    makeTokenAndAdvance(TokenKind.QUESTION);
	    break;
	case '.':
	    scanAfterDot();
	    break;
	case '+':
	    scanAfterPlus();
	    break;
	case '-':
	    scanAfterMinus();
	    break;
	case '*':
	    scanSingleOpOrWithAssign(TokenKind.STAR_ASSIGN, TokenKind.STAR);
	    break;
	case '/':
	    scanAfterSlash();
	    break;
	case '%':
	    scanSingleOpOrWithAssign(TokenKind.PERCENT_ASSIGN, TokenKind.PERCENT);
	    break;
	case '^':
	    scanSingleOpOrWithAssign(TokenKind.HAT_ASSIGN, TokenKind.HAT);
	    break;
	case '&':
	    scanAmperOrBar();
	    break;
	case '|':
	    scanAmperOrBar();
	    break;
	case '~':
	    makeTokenAndAdvance(TokenKind.TILDE);
	    break;
	case '!':
	    scanSingleOpOrWithAssign(TokenKind.NOT_EQUAL, TokenKind.EXCLAM);
	    break;
	case '=':
	    scanSingleOpOrWithAssign(TokenKind.EQUAL, TokenKind.ASSIGN);
	    break;
	case ',':
	    makeTokenAndAdvance(TokenKind.COMMA);
	    break;
	case 'u':
	    advance();
	    if (isBeforeEnd()) {
		if (isCurrChar('8')) {
		    advance();
		    if (isBeforeEnd()) {
			scanAfterStringPrefix();
		    }
		} else
		    scanAfterCharOrStringPrefix();
	    } else
		scanIdentifier();
	    break;
	case 'U':
	    advance();
	    if (isBeforeEnd())
		scanAfterCharOrStringPrefix();
	    else
		scanIdentifier();
	    break;
	case 'L':
	    advance();
	    if (isBeforeEnd())
		scanAfterCharOrStringPrefix();
	    else
		scanIdentifier();
	    break;
	case 'R':
	    advance();
	    if (isBeforeEnd() && isCurrChar('"'))
		scanRawStringLiteral();
	    else
		scanIdentifier();
	    break;
	case '\'':
	    scanCharacterLiteral();
	    break;
	case '"':
	    if (isIncludeDirective) {
		isIncludeDirective = false;
		scanInclude();
	    } else
		scanStringLiteral();
	    break;
	case '\r':
	case '\n':
	    if (isPreprocessingDirective) {
		isPreprocessingDirective = false;
		makeToken(TokenKind.EOD);
	    } else {
		skipWhitespaces();
	    }
	    break;
	case '<':
	    if (isIncludeDirective) {
		isIncludeDirective = false;
		scanInclude();
	    } else
		scanAngleBracket();
	    break;
	case '>':
	    scanAngleBracket();
	    break;
	default:
	    if (Character.isWhitespace(c))
		skipWhitespaces();
	    else if (isIdentifierInitial(c))
		scanIdentifier();
	    else if (Character.isDigit(c))
		scanNumber();
	}
    }

    private void makeNumberToken() {
	String text = new String(source, tokStartPos, currPos - tokStartPos);
	token = new Token(TokenKind.NUMBER, text, tokStartPos, currPos - 1);
    }

    private void scanSingleOpOrWithAssign(TokenKind kindAssign, TokenKind kindSingle) {
	advance();
	if (isPosValidAndCharEquals('='))
	    makeTokenAndAdvance(kindAssign);
	else
	    makeToken(kindSingle);
    }

    private void scanAmperOrBar() {
	char ch = peekChar();

	TokenKind assign = null, duplicate = null, single = null;

	if (ch == '|') {
	    assign = TokenKind.BAR_ASSIGN;
	    duplicate = TokenKind.BAR_BAR;
	    single = TokenKind.BAR;

	} else if (ch == '&') {
	    assign = TokenKind.AMPERSAND_ASSIGN;
	    duplicate = TokenKind.AMPER_AMPER;
	    single = TokenKind.AMPERSAND;
	} else
	    assert (false);

	advance();
	if (isPosValidAndCharEquals('='))
	    makeTokenAndAdvance(assign);
	else if (isPosValidAndCharEquals(ch))
	    makeTokenAndAdvance(duplicate);
	else
	    makeToken(single);
    }

    private void scanAfterSlash() {
	advance();
	if (isBeforeEnd()) {
	    if (isCurrChar('*') || isCurrChar('/'))
		scanComment();
	    else if (isCurrChar('='))
		makeTokenAndAdvance(TokenKind.SLASH_ASSIGN);
	}

	if (token == null)
	    makeToken(TokenKind.SLASH);
    }

    private void scanAfterMinus() {
	advance();
	if (isBeforeEnd()) {
	    if (isCurrChar('-'))
		makeTokenAndAdvance(TokenKind.MINUS_MINUS);
	    else if (isCurrChar('='))
		makeTokenAndAdvance(TokenKind.MINUS_ASSIGN);
	    else if (isCurrChar('>')) {
		advance();
		if (isPosValidAndCharEquals('*'))
		    makeTokenAndAdvance(TokenKind.ARROW_STAR);
		else
		    makeToken(TokenKind.ARROW);
	    }
	}

	if (token == null)
	    makeToken(TokenKind.MINUS);
    }

    private void scanAfterPlus() {
	advance();
	if (isPosValidAndCharEquals('+'))
	    makeTokenAndAdvance(TokenKind.PLUS_PLUS);
	else if (isPosValidAndCharEquals('='))
	    makeTokenAndAdvance(TokenKind.PLUS_ASSIGN);
	else
	    makeToken(TokenKind.PLUS);
    }

    private void scanAfterDot() {
	advance();
	if (isPosValidAndCharEquals('.')) {
	    advance();
	    if (isPosValidAndCharEquals('.')) {
		makeTokenAndAdvance(TokenKind.ELIPSIS);
	    } else
		errorToken();
	} else if (isBeforeEnd() && Character.isDigit(peekChar())) {
	    consumeFractionalNumberEnd();
	    makeNumberToken();
	} else
	    makeToken(TokenKind.DOT);
    }

    private void scanColon() {
	advance();
	if (isPosValidAndCharEquals(':'))
	    makeTokenAndAdvance(TokenKind.COLON_COLON);
	else
	    makeToken(TokenKind.COLON);
    }

    private void scanAfterHash() {
	advance();
	if (isBeforeEnd() && isCurrChar('#')) {
	    advance();
	    makeTokenAndAdvance(TokenKind.HASH_HASH);
	} else {
	    if (isAtNewLine)
		isPreprocessingDirective = true;
	    makeToken(TokenKind.HASH);
	}
    }

    private void scanAngleBracket() {
	assert (isCurrChar('<') || isCurrChar('>'));
	TokenKind shift = null, compar = null, shiftAssign = null, orEqual = null;
	char ch = peekChar();

	if (isCurrChar('<')) {
	    shift = TokenKind.SHIFT_LEFT;
	    shiftAssign = TokenKind.SHIFT_LEFT_ASSIGN;
	    compar = TokenKind.LESS_THEN;
	    orEqual = TokenKind.LESS_EQ;
	} else if (isCurrChar('>')) {
	    shift = TokenKind.SHIFT_RIGHT;
	    shiftAssign = TokenKind.SHIFT_RIGHT_ASSIGN;
	    compar = TokenKind.GREATER_THEN;
	    orEqual = TokenKind.GREATER_EQ;
	} else {
	    assert (false);
	}

	advance();

	if (isBeforeEnd()) {
	    if (isCurrChar(ch)) {
		advance();
		if (isPosValidAndCharEquals('=')) {
		    makeTokenAndAdvance(shiftAssign);
		} else
		    makeToken(shift);
	    } else if (isCurrChar('='))
		makeTokenAndAdvance(orEqual);
	}

	if (token == null)
	    makeTokenAndAdvance(compar);
    }

    private void makeTokenAndAdvance(TokenKind kind) {
	makeToken(kind);
	advance();
    }

    private void scanInclude() {
	char endChar = '\0';
	TokenKind kind = TokenKind.ERROR;

	if (isCurrChar('<')) {
	    endChar = '>';
	    kind = TokenKind.ANGLE_INCLUDE;
	} else if (isCurrChar('"')) {
	    endChar = '"';
	    kind = TokenKind.QUOTE_INCLUDE;
	} else
	    assert (false);

	advance();

	while (isBeforeEnd() && !isCurrChar(endChar))
	    advance();

	if (isBeforeEnd() && isCurrChar(endChar)) {
	    advance();
	    makeTokenWithText(kind);
	} else
	    errorToken();
    }

    private void scanAfterCharOrStringPrefix() {
	if (isCurrChar('\'')) {
	    scanCharacterLiteral();
	} else if (isCurrChar('"')) {
	    scanStringLiteral();
	} else if (isCurrChar('R')) {
	    advance();
	    if (isBeforeEnd() && isCurrChar('"')) {
		scanRawStringLiteral();
	    } else {
		scanIdentifier();
	    }
	}
    }

    private void scanAfterStringPrefix() {
	if (isCurrChar('"')) {
	    scanStringLiteral();
	} else if (isCurrChar('R')) {
	    advance();
	    if (isBeforeEnd() && isCurrChar('"')) {
		scanRawStringLiteral();
	    } else {
		scanIdentifier();
	    }
	}
    }

    private boolean isCurrChar(char c) {
	return source[currPos] == c;
    }

    private void restorePos() {
	currPos = savedPos;
    }

    private void savePos() {
	savedPos = currPos;
    }

    private boolean isAtEnd() {
	return currPos >= sourceLen;
    }

    private void scanComment() {
	if (peekChar() == '*') {
	    boolean isCommentEndFound = false;

	    advance();
	    while (isBeforeEnd()) {
		if (peekChar() == '*' && (currPos + 1 < sourceLen && source[currPos + 1] == '/')) {
		    currPos += 2;
		    isCommentEndFound = true;
		    break;
		}

		advance();
	    }

	    if (isCommentEndFound)
		makeToken(TokenKind.COMMENT);
	    else
		errorToken();
	} else {
	    while (isBeforeEnd() && peekChar() != '\n') {
		advance();
	    }

	    makeToken(TokenKind.COMMENT);
	}

    }

    private void advance() {
	currPos++;
	if (currPos < sourceLen && source[currPos] == '\\') {
	    int pos = currPos + 1;
	    if (pos < sourceLen) {
		if (source[pos] == '\n')
		    currPos += 2;
		else if (source[pos] == '\r') {
		    currPos += 2;
		    if (currPos < sourceLen && source[currPos] == '\n')
			currPos++;
		}
	    }
	}
    }

    private void scanRawStringLiteral() {
	consumeUpToDoubleQuote();

	int prefixBegPos = currPos;
	int prefixLen = 0;

	// consume prefix and (
	while (isBeforeEnd()) {
	    if (peekChar() == '(') {
		prefixLen = currPos - prefixBegPos;
		advance();
		break;
	    } else if (!isRawStringPrefixCharacter(peekChar())) {
		advance();
		errorToken();
		return;
	    } else
		advance();
	}

	if (isAtEnd()) {
	    errorToken();
	    return;
	}

	boolean isStringCorrect = false;
	boolean isMatchingPrefix = false;
	int prefixMatchedCount = 0;
	while (isBeforeEnd()) {
	    if (peekChar() == ')') {
		isMatchingPrefix = true;
		prefixMatchedCount = 0;
	    } else if (isMatchingPrefix) {
		if (prefixMatchedCount == prefixLen && peekChar() == '"') {
		    advance();
		    isStringCorrect = true;
		    break;
		} else if (source[prefixBegPos + prefixMatchedCount] == peekChar())
		    prefixMatchedCount++;
		else {
		    isMatchingPrefix = false;
		    prefixMatchedCount = 0;
		}
	    }

	    advance();
	}

	if (isStringCorrect)
	    makeTokenWithText(TokenKind.STRING);
	else
	    errorToken();
    }

    boolean isRawStringPrefixCharacter(char c) {
	switch (c) {
	case ')':
	case '\\':
	case '\t':
	case 0x0B: // vertical tab
	case '\f':
	case '\n':
	    return false;
	default:
	    return true;
	}
    }

    private void scanStringLiteral() {
	consumeUpToDoubleQuote();

	while (isBeforeEnd() && peekChar() != '"') {
	    if (peekChar() == '\\') {
		advance();
		if (!consumeCharEscape()) {
		    errorToken();
		    return;
		}
	    } else if (peekChar() == '\n') {
		errorToken();
		return;
	    } else
		advance();
	}

	if (isBeforeEnd() && peekChar() == '"')
	    advance();
	else {
	    errorToken();
	    return;
	}

	makeTokenWithText(TokenKind.STRING);
    }

    private void consumeUpToDoubleQuote() {
	for (; isBeforeEnd(); advance()) {
	    if (peekChar() == '"') {
		advance();
		break;
	    }
	}
    }

    private void errorToken() {
	token = new Token(TokenKind.ERROR, tokStartPos, currPos);
    }

    private void scanCharacterLiteral() {
	advance();

	if (isAtEnd()) {
	    errorToken();
	    return;
	}

	if (peekChar() == '\\') {
	    advance();
	    if (!consumeCharEscape()) {
		errorToken();
		return;
	    }
	} else if (peekChar() == '\n' || peekChar() == '\'') {
	    errorToken();
	    return;
	} else
	    advance();

	if (isBeforeEnd() && peekChar() == '\'') {
	    advance();
	    makeTokenWithText(TokenKind.CHAR);
	} else
	    errorToken();
    }

    private void makeTokenWithText(TokenKind k) {
	String text = new String(source, tokStartPos, currPos - tokStartPos);
	token = new Token(k, text, tokStartPos, currPos - 1);
    }

    private boolean consumeCharEscape() {
	boolean success = true;
	if (isBeforeEnd()) {
	    char c = peekChar();
	    if (isSingleEscapeCharacter(c))
		advance();
	    else if (c == 'x') {
		advance();
		if (isAtEnd() || !isHexadecimalDigit(peekChar()))
		    success = false;

		while (isBeforeEnd() && isHexadecimalDigit(peekChar()))
		    advance();
	    } else if (c == 'u' || c == 'U') {
		advance();
		int numberHexToRead = c == 'u' ? 4 : 8;
		int i = 0;
		for (; i < numberHexToRead; ++i) {
		    if (isAtEnd() || !isHexadecimalDigit(peekChar())) {
			success = false;
			break;
		    }
		    advance();
		}

		if (i < numberHexToRead)
		    success = false;
	    } else if (isOctalDigit(c)) {
		while (isBeforeEnd() && isOctalDigit(peekChar()))
		    advance();
	    } else
		success = false;
	}
	return success;
    }

    private char peekChar() {
	return source[currPos];
    }

    private void scanNumber() {
	if (isCurrChar('0')) {
	    advance();
	    if (isBeforeEnd()) {
		char c2 = peekChar();
		if (c2 == 'x' || c2 == 'X')
		    scanHexadecimal();
		else if ('0' <= c2 && c2 <= '7')
		    scanOctal();
		else if (c2 == '.') {
		    consumeFractionalNumberEnd();
		    makeNumberToken();
		}
	    }
	}

	if (token == null)
	    scanDecimal();
    }

    private void scanDecimal() {

	char c;

	while (isBeforeEnd()) {
	    c = peekChar();
	    if (Character.isDigit(c))
		advance();
	    else if (isIntegerSuffix(c)) {
		consumeIntegerSuffix(c);
		break;
	    } else if (c == '.') {
		consumeFractionalNumberEnd();
		break;
	    } else
		break;

	}

	makeNumberToken();
    }

    private void consumeFractionalNumberEnd() {
	advance();
	while (isBeforeEnd() && Character.isDigit(peekChar()))
	    advance();

	if (isBeforeEnd() && (peekChar() == 'e' || peekChar() == 'E'))
	    advance();

	if (isBeforeEnd() && (peekChar() == '+' || peekChar() == '-'))
	    advance();

	while (isBeforeEnd() && Character.isDigit(peekChar()))
	    advance();

	if (isBeforeEnd() && (peekChar() == 'f' || peekChar() == 'F' || peekChar() == 'l' || peekChar() == 'L'))
	    advance();
    }

    private void consumeIntegerSuffix(char c) {
	if (c == 'u' || c == 'U') {
	    advance();
	    if (isBeforeEnd()) {
		char c2 = peekChar();
		if (c2 == 'l' || c2 == 'L') {
		    advance();
		    if (isBeforeEnd()) {
			char c3 = peekChar();
			if (c3 == c2)
			    advance();
		    }
		}
	    }
	} else if (c == 'l' || c == 'L') {
	    advance();
	    if (isBeforeEnd()) {
		char c2 = peekChar();
		if (c2 == c) {
		    advance();
		    if (isBeforeEnd()) {
			char c3 = peekChar();
			if (c3 == 'u' || c3 == 'U')
			    advance();
		    }
		} else if (c2 == 'u' || c2 == 'U')
		    advance();
	    }
	}
    }

    private void makeToken(TokenKind k) {
	token = new Token(k, tokStartPos, currPos - 1);
    }

    private void scanOctal() {
	advance();

	while (isBeforeEnd() && isOctalDigit(peekChar()))
	    advance();

	if (isBeforeEnd() && isIntegerSuffix(peekChar()))
	    consumeIntegerSuffix(peekChar());

	makeNumberToken();
    }

    private boolean isOctalDigit(char c) {
	return '0' <= c && c <= '7';
    }

    private void scanHexadecimal() {

	currPos += 2;

	while (isBeforeEnd() && isHexadecimalDigit(peekChar()))
	    advance();

	if (isBeforeEnd() && isIntegerSuffix(peekChar()))
	    consumeIntegerSuffix(peekChar());

	makeNumberToken();
    }

    private void skipWhitespaces() {
	while (isBeforeEnd() && Character.isWhitespace(peekChar())) {
	    if (isCurrChar('\n'))
		isAtNewLine = true;

	    advance();
	}
    }

    private void scanIdentifier() {

	advance();

	while (isBeforeEnd() && isIdentifierCharacter(peekChar()))
	    advance();

	String text = new String(source, tokStartPos, currPos - tokStartPos);
	if (isPreprocessingDirective && text.compareTo("include") == 0 && !isIncludeDirective)
	    isIncludeDirective = true;

	token = new Token(TokenKind.IDENTIFIER, text, tokStartPos, currPos - 1);
    }

    boolean isIdentifierInitial(char c) {
	return c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
    }

    boolean isIdentifierCharacter(char c) {
	return c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || Character.isDigit(c);
    }

    boolean isIntegerSuffix(char c) {
	return c == 'u' || c == 'U' || c == 'l' || c == 'L';
    }

    boolean isHexadecimalDigit(char c) {
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
    }

    boolean isSingleEscapeCharacter(char c) {
	switch (c) {
	case '\'':
	case '"':
	case '?':
	case '\\':
	case 'a':
	case 'b':
	case 'f':
	case 'n':
	case 'r':
	case 't':
	case 'v':
	    return true;
	default:
	    return false;
	}
    }

    public static int[] massivi = new int[1028];

    static int getSomething() {
	return massivi[2];
    }

    public static void main(String[] args) {
	while (true) {
	    String s = Util.readLine();
	    printTokens(s);
	}
    }

    public static void printTokens(String s) {
	Lexer l = new Lexer(s);
	ArrayList<Token> a = l.getTokens();

	for (Token token : a) {
	    System.out.println(token.toString());
	}
    }

    public static void printTokensToFile(String s, String file) {
	Lexer l = new Lexer(s);
	ArrayList<Token> a = l.getTokens();

	try {
	    PrintWriter writer = new PrintWriter(file);
	    for (Token token : a) {
		writer.println(token.toString());
	    }
	    writer.close();
	} catch (FileNotFoundException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}
    }
}
