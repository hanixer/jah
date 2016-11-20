package project;

import java.util.ArrayList;

import java.util.Iterator;
import java.util.Random;

import project.Token.TokenKind;
/**
 * What's not supported:
 * 		- user defined literals
 * 		- trigraph sequences 
 *
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

		char c = source[currPos];
		if (isCharacterLiteralInitial(c))
			tk = scanCharacterLiteral();
		else if (isIdentifierInitial(c))
			tk = scanIdentifier();
		else if (Character.isDigit(c))
			tk = scanNumber();
		else if (c == '.') {
			if (currPos + 1 < sourceLen && Character.isDigit(source[currPos + 1])) {
				int startPos = currPos;
				consumeFractionalNumberEnd();
				tk = makeToken(startPos, TokenKind.NUMBER);
			}
		} else if (c == '\'') {

		}

		return tk;
	}

	private Token scanCharacterLiteral() {
		int startPos = currPos;
		if (source[currPos] == '\'')
			currPos += 1;
		else
			currPos += 2;
		
		if (currPos >= sourceLen)
			return new Token(TokenKind.ERROR, startPos, currPos);

		if (source[currPos] == '\\') {
			currPos++;
			if (currPos < sourceLen) {
				char c = source[currPos];
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
					currPos++;
					break;
				default:
					if (c == 'x') {
						currPos++;
						
						if (currPos >= sourceLen || !isHexadecimalDigit(source[currPos]))
							return new Token(TokenKind.ERROR, startPos, currPos);
						
						while (currPos < sourceLen && isHexadecimalDigit(source[currPos]))
							currPos++;
					} else if (c == 'u' || c == 'U') {
						currPos++;
						int numberHexToRead = c == 'u' ? 4 : 8;
						int i = 0;
						for (;i < numberHexToRead; ++i) {
							if (currPos >= sourceLen || !isHexadecimalDigit(source[currPos]))
								return new Token(TokenKind.ERROR, startPos, currPos);
							currPos++;
						}
						
						if (i < numberHexToRead)
							return new Token(TokenKind.ERROR, startPos, currPos);
					} else if (isOctalDigit(c)) {
						while (currPos < sourceLen && isOctalDigit(source[currPos]))
							currPos++;
					} else 
						return new Token(TokenKind.ERROR, startPos, currPos);						
				}
			}
		} else if (source[currPos] == '\n' || source[currPos] == '\'')
			return new Token(TokenKind.ERROR, startPos, currPos);
		else 
			currPos++;
		
		if (currPos < sourceLen && source[currPos] == '\'')
			currPos++;
		else
			return new Token(TokenKind.ERROR, startPos, currPos);			

		return makeToken(startPos, TokenKind.CHAR);
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
