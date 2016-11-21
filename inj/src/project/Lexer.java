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

	Lexer(String src) {
		source = src.toCharArray();
		sourceLen = source.length;
	}

	ArrayList<Token> getTokens() {
		ArrayList<Token> result = new ArrayList<>();

		do {
			Token tk = getToken();

			if (tk.kind == Token.TokenKind.EOF)
				break;

			result.add(tk);
		} while (currPos < sourceLen);

		return result;
	}

	Token getToken() {
		Token tk = null;

		skipWhitespaces();

		if (currPos >= sourceLen)
			return new Token(TokenKind.EOF);

		int startPos = currPos;
		char c = source[currPos];
		if (c == '#') {
			currPos++;
			if (currPos < sourceLen && source[currPos] == '#') {
				currPos++;
				tk = makeToken(startPos, TokenKind.DOUBLE_POUND);
			} else {
				tk = makeToken(startPos, TokenKind.POUND);
			}
		} else if (c == '.') {

		}
		if (isCharacterLiteralInitial(c))
			tk = scanCharacterLiteral();
		else if (isStringLiteralInitial(c))
			tk = scanStringLiteral();
		else if (isRawStringLiteralInitial(c))
			tk = scanRawStringLiteral();
		else if (isIdentifierInitial(c))
			tk = scanIdentifier();
		else if (isComment())
			tk = scanComment();
		else if (Character.isDigit(c))
			tk = scanNumber();
		else if (c == '.') {
			if (currPos + 1 < sourceLen && Character.isDigit(source[currPos + 1])) {
				consumeFractionalNumberEnd();
				tk = makeToken(startPos, TokenKind.NUMBER);
			}
		} else if (c == '\'') {

		}

		return tk;
	}

	private Token scanComment() {
		Token tk = null;
		int startPos = currPos;
		currPos++;
		if (source[currPos] == '*') {
			boolean isCommentEndFound = false;
			
			currPos++;
			while (currPos < sourceLen) {
				if (source[currPos] == '*' && (currPos + 1 < sourceLen && source[currPos + 1] == '/')) {
					currPos += 2;
					isCommentEndFound = true;
					break;
				}
				
				currPos++;
			}
			
			if (isCommentEndFound)
				tk = makeToken(startPos, TokenKind.COMMENT);
			else
				tk = errorToken(startPos);
		} else {
			while (currPos < sourceLen && source[currPos] != '\n') {
				currPos++;
			}
			
			tk = makeToken(startPos, TokenKind.COMMENT);
		}
		
		return tk;
	}

	private boolean isComment() {
		return source[currPos] == '/' &&
				currPos + 1 < sourceLen &&
				(source[currPos + 1] == '*' || source[currPos + 1] == '/');
	}

	private Token scanRawStringLiteral() {
		int startPos = currPos;

		consumeUpToDoubleQuote();

		int prefixBegPos = currPos;
		int prefixLen = 0;

		// consume prefix and (
		while (currPos < sourceLen) {
			if (source[currPos] == '(') {
				prefixLen = currPos - prefixBegPos;
				currPos++;				
				break;
			} else if (!isRawStringPrefixCharacter(source[currPos])) {
				currPos++;
				return errorToken(startPos);
			} else
				currPos++;
		}

		if (currPos >= sourceLen)
			return errorToken(startPos);

		boolean isStringCorrect = false;
		boolean isMatchingPrefix = false;
		int prefixMatchedCount = 0;
		while (currPos < sourceLen) {
			if (source[currPos] == ')') {
				isMatchingPrefix = true;
				prefixMatchedCount = 0;
			} else if (isMatchingPrefix) {
				if (prefixMatchedCount == prefixLen && source[currPos] == '"') {
					currPos++;
					isStringCorrect = true;
					break;
				} else if (source[prefixBegPos + prefixMatchedCount] == source[currPos])
					prefixMatchedCount++;
				else {
					isMatchingPrefix = false;
					prefixMatchedCount = 0;
				}
			}

			currPos++;
		}

		if (isStringCorrect)
			return makeToken(startPos, TokenKind.STRING);
		else
			return errorToken(startPos);
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

	private Token scanStringLiteral() {
		int startPos = currPos;

		consumeUpToDoubleQuote();

		while (currPos < sourceLen && source[currPos] != '"') {
			if (source[currPos] == '\\') {
				currPos++;
				if (!consumeCharEscape()) {
					return errorToken(startPos);
				}
			} else if (source[currPos] == '\n')
				return errorToken(startPos);
			else
				currPos++;
		}

		if (currPos < sourceLen && source[currPos] == '"')
			currPos++;
		else
			return errorToken(startPos);

		return makeToken(startPos, TokenKind.STRING);
	}

	private boolean isRawStringLiteralInitial(char c) {
		if (c == 'R' && (currPos + 1 < sourceLen && source[currPos + 1] == '"'))
			return true;

		int offset = stringEncodingPrefixOffset();
		int pos1 = currPos + offset;
		int pos2 = currPos + offset + 1;
		if (offset > 0 && (pos1 < sourceLen && source[pos1] == 'R') && (pos2 < sourceLen && source[pos2] == '"'))
			return true;

		return false;
	}

	private void consumeUpToDoubleQuote() {
		for (; currPos < sourceLen; ++currPos) {
			if (source[currPos] == '"') {
				currPos++;
				break;
			}
		}
	}

	private Token errorToken(int startPos) {
		return new Token(TokenKind.ERROR, startPos, currPos);
	}

	private int stringEncodingPrefixOffset() {
		int pos = currPos;
		if (pos < sourceLen) {
			if (source[pos] == 'u') {
				pos++;
				if (pos < sourceLen && source[pos] == '8') {
					pos++;
				}
			} else if (source[pos] == 'U' || source[pos] == 'L')
				pos++;
		}

		return pos - currPos;
	}

	private boolean isStringLiteralInitial(char c) {
		if (c == '"')
			return true;

		int offset = stringEncodingPrefixOffset();
		if (offset > 0 && (currPos + offset < sourceLen && source[currPos + offset] == '"'))
			return true;

		return false;
	}

	private Token scanCharacterLiteral() {
		int startPos = currPos;
		if (source[currPos] == '\'')
			currPos += 1;
		else
			currPos += 2;

		if (currPos >= sourceLen)
			return errorToken(startPos);

		if (source[currPos] == '\\') {
			currPos++;
			if (!consumeCharEscape())
				return errorToken(startPos);
		} else if (source[currPos] == '\n' || source[currPos] == '\'')
			return errorToken(startPos);
		else
			currPos++;

		if (currPos < sourceLen && source[currPos] == '\'')
			currPos++;
		else
			return errorToken(startPos);

		return makeToken(startPos, TokenKind.CHAR);
	}

	private boolean consumeCharEscape() {
		boolean success = true;
		if (currPos < sourceLen) {
			char c = source[currPos];
			if (isSingleEscapeCharacter(c))
				currPos++;
			else if (c == 'x') {
				currPos++;
				if (currPos >= sourceLen || !isHexadecimalDigit(source[currPos]))
					success = false;

				while (currPos < sourceLen && isHexadecimalDigit(source[currPos]))
					currPos++;
			} else if (c == 'u' || c == 'U') {
				currPos++;
				int numberHexToRead = c == 'u' ? 4 : 8;
				int i = 0;
				for (; i < numberHexToRead; ++i) {
					if (currPos >= sourceLen || !isHexadecimalDigit(source[currPos])) {
						success = false;
						break;
					}
					currPos++;
				}

				if (i < numberHexToRead)
					success = false;
			} else if (isOctalDigit(c)) {
				while (currPos < sourceLen && isOctalDigit(source[currPos]))
					currPos++;
			} else
				success = false;
		}
		return success;
	}

	private Token scanNumber() {
		Token tk = null;
		int startPos = currPos;
		char c = source[currPos];

		if (c == '0') {
			if (currPos + 1 < sourceLen) {
				char c2 = source[currPos + 1];
				if (c2 == 'x' || c2 == 'X')
					tk = scanHexadecimal();
				else if ('0' <= c2 && c2 <= '7')
					tk = scanOctal();
				else if (c2 == '.') {
					currPos++;
					consumeFractionalNumberEnd();
					tk = makeToken(startPos, TokenKind.NUMBER);
				}
			}
		} else
			tk = scanDecimal();

		return tk;
	}

	private Token scanDecimal() {
		int startPos = currPos;
		char c;

		while (currPos < sourceLen) {
			c = source[currPos];
			if (Character.isDigit(c))
				currPos++;
			else if (isIntegerSuffix(c)) {
				consumeIntegerSuffix(c);
				break;
			} else if (c == '.') {
				consumeFractionalNumberEnd();
				break;
			} else
				break;

		}

		return makeToken(startPos, TokenKind.NUMBER);
	}

	private void consumeFractionalNumberEnd() {
		currPos++;
		while (currPos < sourceLen && Character.isDigit(source[currPos]))
			currPos++;

		if (currPos < sourceLen && (source[currPos] == 'e' || source[currPos] == 'E'))
			currPos++;

		if (currPos < sourceLen && (source[currPos] == '+' || source[currPos] == '-'))
			currPos++;

		while (currPos < sourceLen && Character.isDigit(source[currPos]))
			currPos++;

		if (currPos < sourceLen && (source[currPos] == 'f' || source[currPos] == 'F' || source[currPos] == 'l'
				|| source[currPos] == 'L'))
			currPos++;
	}

	private void consumeIntegerSuffix(char c) {
		if (c == 'u' || c == 'U') {
			currPos++;
			if (currPos < sourceLen) {
				char c2 = source[currPos];
				if (c2 == 'l' || c2 == 'L') {
					currPos++;
					if (currPos < sourceLen) {
						char c3 = source[currPos];
						if (c3 == c2)
							currPos++;
					}
				}
			}
		} else if (c == 'l' || c == 'L') {
			currPos++;
			if (currPos < sourceLen) {
				char c2 = source[currPos];
				if (c2 == c) {
					currPos++;
					if (currPos < sourceLen) {
						char c3 = source[currPos];
						if (c3 == 'u' || c3 == 'U')
							currPos++;
					}
				} else if (c2 == 'u' || c2 == 'U')
					currPos++;
			}
		}
	}

	private Token makeToken(int startPos, TokenKind k) {
		return new Token(k, new String(source, startPos, currPos - startPos));
	}

	private Token scanOctal() {
		int startPos = currPos;
		currPos++;

		while (currPos < sourceLen && isOctalDigit(source[currPos]))
			currPos++;

		if (currPos < sourceLen && isIntegerSuffix(source[currPos]))
			consumeIntegerSuffix(source[currPos]);

		return makeToken(startPos, TokenKind.NUMBER);
	}

	private boolean isOctalDigit(char c) {
		return '0' <= c && c <= '7';
	}

	private Token scanHexadecimal() {
		int startPos = currPos;
		currPos += 2;

		while (currPos < sourceLen && isHexadecimalDigit(source[currPos]))
			currPos++;

		if (currPos < sourceLen && isIntegerSuffix(source[currPos]))
			consumeIntegerSuffix(source[currPos]);

		return makeToken(startPos, TokenKind.NUMBER);
	}

	private void skipWhitespaces() {
		while (currPos < sourceLen && Character.isWhitespace(source[currPos]))
			currPos++;
	}

	private Token scanIdentifier() {
		int startPos = currPos;
		currPos++;

		while (currPos < sourceLen && isIdentifierCharacter(source[currPos]))
			currPos++;

		return new Token(TokenKind.IDENTIFIER, new String(source, startPos, currPos - startPos));
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
