package parser;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import parser.Lexer;
import parser.Token;
import parser.TokenKind;

public class LexerTest {
	Lexer lexer;

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() {
		lexer = new Lexer("123 234");
		Object[] output = lexer.getTokens().stream().map(t -> t.text).toArray();
		assertArrayEquals(Arrays.asList("123", "234", "").toArray(), output);
	}

	@Test
	public void testFloating() {
		lexer = new Lexer(" 1.0f 2.1023123f 1.0e+123 ");
		Object[] output = lexer.getTokens().stream().map(t -> t.text).toArray();
		assertArrayEquals(Arrays.asList("1.0f", "2.1023123f", "1.0e+123", "").toArray(), output);
	}

	@Test
	public void testOtherNumbers() {
		assertOneTokenNumber("1.01f");
		assertOneTokenNumber(".01f");
		assertOneTokenNumber("123ULL");
		assertOneTokenNumber("123llu");
		assertOneTokenNumber("123Lu");
		assertOneTokenNumber("0xDeaDbEef");
		assertOneTokenNumber("0123u");
		assertOneTokenNumber("1.21378e+123f");
		assertOneTokenNumber("1.21378e-123L");
		assertOneTokenNumber("1.21378e123L");
	}

	@Test
	public void testCharacters() {
		assertOneCharacter("'c'");
		assertOneCharacter("u'c'");
		assertOneCharacter("U'c'");
		assertOneCharacter("L'c'");
		assertOneCharacter("'\\a'");
		assertOneCharacter("'\\12'");
		assertOneCharacter("'\\xA2'");
		assertOneCharacter("'\\n'");
		assertOneCharacter("'\\udead'");
		assertOneCharacter("'\\UdeadBEAF'");
	}

	@Test
	public void testString() {
		assertOneTokenString("\"abc\"");
		assertOneTokenString("u\"abc\"");
		assertOneTokenString("u8\"abc\"");
		assertOneTokenString("U\"abc\"");
		assertOneTokenString("L\"abc\"");
		assertOneTokenString("L\"\\x12afaf\"");
		assertOneTokenString("\"\\u11aa\"");
		assertOneTokenString("\"\\012356\"");
		assertOneTokenString("\"\"");

		assertTokenError("\"abc\n\"");
		assertTokenError("\"a\\u111 bc\"");
		assertTokenError("\"a\\ bc\"");
	}

	@Test
	public void testRawString() {
		assertOneTokenString("R\"()\"");
		assertOneTokenString("R\"(abc)\"");
		assertOneTokenString("uR\"(abc)\"");
		assertOneTokenString("u8R\"(abc)\"");
		assertOneTokenString("UR\"(abc)\"");
		assertOneTokenString("LR\"(abc)\"");
		assertOneTokenString("u8R\"**(abc)**\"");
		assertOneTokenString("R\"1(()()()()wjiofeoewjfewf\n\nwjeiorjwer\\)1\"");

		assertTokenError("R\"\t(abc)\t");
		assertTokenError("R\"\t(ab");
		assertTokenError("R\"**(ab)*\"");
		assertTokenError("R\"abc\"");
	}

	@Test
	public void testBackslashNewline() {
		assertOneCharacter("'\\\nc'");
		assertOneTokenString("u8R\\\n\"*(ab)*\"");
	}

	@Test
	public void testPreprocessingTokens() {
		input("#include some\n");
		pound();
		identifier("include");
		identifier("some");
		eod();
		eof();

		input("#include some\ngap");
		pound();
		identifier("include");
		identifier("some");
		eod();
		identifier("gap");
		eof();

		input("#include <iostream>");
		pound();
		identifier("include");
		angleInclude("<iostream>");
		eod();
		eof();
	}

	@Test
	public void testShiftOperations() {
		input("<<");
		op(TokenKind.SHIFT_LEFT);

		input(">>");
		op(TokenKind.SHIFT_RIGHT);

		input("1<<2*3");
		number("1");
		op(TokenKind.SHIFT_LEFT);
		number("2");
		op(TokenKind.STAR);
		number("3");
		eof();
	}

	@Test
	public void testCR() {
		input("1 + \r\n 2");
		number("1");
		op(TokenKind.PLUS);
		number("2");
		eof();
	}

	public void op(TokenKind opKind) {
		Token t = lexer.getToken();
		assertEquals(opKind, t.kind);
	}

	public void number(String numStr) {
		Token t = lexer.getToken();
		assertEquals(TokenKind.NUMBER, t.kind);
		assertEquals(numStr, t.text);
	}

	private void angleInclude(String s) {
		Token token = lexer.getToken();
		assertEquals(TokenKind.ANGLE_INCLUDE, token.kind);
		assertEquals(s, token.text);
	}

	private void eod() {
		assertEquals(TokenKind.EOD, lexer.getToken().kind);
	}

	private void eof() {
		assertEquals(TokenKind.EOF, lexer.getToken().kind);
	}

	private void identifier(String s2) {
		Token token = lexer.getToken();
		assertEquals(TokenKind.IDENTIFIER, token.kind);
		assertEquals(s2, token.text);
	}

	private void pound() {
		assertEquals(TokenKind.HASH, lexer.getToken().kind);
	}

	private void input(String s) {
		lexer = new Lexer(s);
	}

	private void assertOneCharacter(String s) {
		input(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.CHAR, tk.kind);
		assertEquals(s, tk.text);
	}

	private void assertOneTokenNumber(String s) {
		input(s);
		number(s);
	}

	private void assertOneTokenString(String s) {
		input(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.STRING, tk.kind);
		assertEquals(s, tk.text);
	}

	private void assertTokenError(String s) {
		input(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.ERROR, tk.kind);
	}

}
