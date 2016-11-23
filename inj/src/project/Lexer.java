package project;

import java.util.ArrayList;

import project.Token.TokenKind;

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
	boolean isPreviousPoundOnNewLine;
	Token token;

	Lexer(String src) {
		source = src.toCharArray();
		sourceLen = source.length;
		currPos = -1;
		isAtNewLine = true;
		isPreviousPoundOnNewLine = false;
		advance();
	}

	ArrayList<Token> getTokens() {
		ArrayList<Token> result = new ArrayList<>();

		do {
			getToken();

			if (token.kind == Token.TokenKind.EOF)
				break;

			result.add(token);
		} while (isBeforeEnd());

		return result;
	}

	private boolean isBeforeEnd() {
		return currPos < sourceLen;
	}

	Token getToken() {
		token = null;

		while (token == null && !isAtEnd()) {
			scan();
		}
		
		if (token == null)
			token = new Token(TokenKind.EOF);

		return token;
	}

	private void scan() {
		tokStartPos = currPos;
		char c = peekChar();
		switch (c) {
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
			scanStringLiteral();
			break;
		case '#':
			advance();

			isPreviousPoundOnNewLine = false;

			if (isBeforeEnd() && isCurrChar('#')) {
				advance();
				makeToken(tokStartPos, TokenKind.DOUBLE_POUND);
			} else {
				if (isAtNewLine)
					isPreviousPoundOnNewLine = true;

				makeToken(tokStartPos, TokenKind.POUND);
			}
			break;
		case '/':
			advance();
			if (isBeforeEnd() && (isCurrChar('*') || isCurrChar('/')))
				scanComment();
			else {
				makeToken(tokStartPos, TokenKind.DIVIDE);
				advance();
			}
			break;
		case '\n':
			if (isPreviousPoundOnNewLine) {
				isPreviousPoundOnNewLine = false;
				makeToken(tokStartPos, TokenKind.EOD);
			} else {
				skipWhitespaces();
			}
			break;
		default:
			if (Character.isWhitespace(c))
				skipWhitespaces();
			else if (isIdentifierInitial(c))
				scanIdentifier();
			else if (isComment())
				scanComment();
			else if (Character.isDigit(c))
				scanNumber();
			else if (c == '.') {
				savePos();
				advance();
				if (isBeforeEnd() && Character.isDigit(peekChar())) {
					consumeFractionalNumberEnd();
					makeToken(tokStartPos, TokenKind.NUMBER);
				} else {
					restorePos();
				}
			}
		}
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

		int startPos = currPos;
		advance();
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
				makeToken(startPos, TokenKind.COMMENT);
			else
				errorToken(startPos);
		} else {
			while (isBeforeEnd() && peekChar() != '\n') {
				advance();
			}

			makeToken(startPos, TokenKind.COMMENT);
		}

	}

	private void advance() {
		currPos++;
		if (currPos + 1 < sourceLen && source[currPos] == '\\' && source[currPos + 1] == '\n')
			currPos += 2;
	}

	private boolean isComment() {
		return peekChar() == '/' && currPos + 1 < sourceLen
				&& (source[currPos + 1] == '*' || source[currPos + 1] == '/');
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
				errorToken(tokStartPos);
				return;
			} else
				advance();
		}

		if (isAtEnd()) {
			errorToken(tokStartPos);
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
			makeToken(tokStartPos, TokenKind.STRING);
		else
			errorToken(tokStartPos);
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
					errorToken(tokStartPos);
					return;
				}
			} else if (peekChar() == '\n') {
				errorToken(tokStartPos);
				return;
			} else
				advance();
		}

		if (isBeforeEnd() && peekChar() == '"')
			advance();
		else {
			errorToken(tokStartPos);
			return;
		}

		makeToken(tokStartPos, TokenKind.STRING);
	}

	private void consumeUpToDoubleQuote() {
		for (; isBeforeEnd(); advance()) {
			if (peekChar() == '"') {
				advance();
				break;
			}
		}
	}

	private void errorToken(int startPos) {
		token = new Token(TokenKind.ERROR, startPos, currPos);
	}

	private void scanCharacterLiteral() {
		advance();

		if (isAtEnd()) {
			errorToken(tokStartPos);
			return;
		}

		if (peekChar() == '\\') {
			advance();
			if (!consumeCharEscape()) {
				errorToken(tokStartPos);
				return;
			}
		} else if (peekChar() == '\n' || peekChar() == '\'') {
			errorToken(tokStartPos);
			return;
		} else
			advance();

		if (isBeforeEnd() && peekChar() == '\'') {
			advance();
			makeToken(tokStartPos, TokenKind.CHAR);
		} else
			errorToken(tokStartPos);
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

		int startPos = currPos;
		char c = peekChar();

		if (c == '0') {
			if (currPos + 1 < sourceLen) {
				char c2 = source[currPos + 1];
				if (c2 == 'x' || c2 == 'X')
					scanHexadecimal();
				else if ('0' <= c2 && c2 <= '7')
					scanOctal();
				else if (c2 == '.') {
					advance();
					consumeFractionalNumberEnd();
					makeToken(startPos, TokenKind.NUMBER);
				}
			}
		} else
			scanDecimal();

	}

	private void scanDecimal() {
		int startPos = currPos;
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

		makeToken(startPos, TokenKind.NUMBER);
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

	private void makeToken(int startPos, TokenKind k) {
		token = new Token(k, new String(source, startPos, currPos - startPos));
	}

	private void scanOctal() {
		int startPos = currPos;
		advance();

		while (isBeforeEnd() && isOctalDigit(peekChar()))
			advance();

		if (isBeforeEnd() && isIntegerSuffix(peekChar()))
			consumeIntegerSuffix(peekChar());

		makeToken(startPos, TokenKind.NUMBER);
	}

	private boolean isOctalDigit(char c) {
		return '0' <= c && c <= '7';
	}

	private void scanHexadecimal() {
		int startPos = currPos;
		currPos += 2;

		while (isBeforeEnd() && isHexadecimalDigit(peekChar()))
			advance();

		if (isBeforeEnd() && isIntegerSuffix(peekChar()))
			consumeIntegerSuffix(peekChar());

		makeToken(startPos, TokenKind.NUMBER);
	}

	private void skipWhitespaces() {
		while (isBeforeEnd() && Character.isWhitespace(peekChar())) {
			if (isCurrChar('\n'))
				isAtNewLine = true;

			advance();
		}
	}

	private void scanIdentifier() {
		int startPos = currPos;
		advance();

		while (isBeforeEnd() && isIdentifierCharacter(peekChar()))
			advance();

		token = new Token(TokenKind.IDENTIFIER, new String(source, startPos, currPos - startPos));
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

	boolean isCharacterLiteralInitial(char c) {
		if (c == '\'')
			return true;
		else if ((c == 'u' || c == 'U' || c == 'L') && (currPos + 1 < sourceLen && source[currPos + 1] == '\''))
			return true;
		else
			return false;
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
			Lexer l = new Lexer(s);
			ArrayList<Token> a = l.getTokens();

			for (Token token : a) {
				System.out.println(token.toString());
			}
		}
	}
}
