package project;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import project.Token.TokenKind;

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
		assertArrayEquals(Arrays.asList("123", "234").toArray(), output);		
	}
	
	@Test
	public void testFloating() {
		lexer = new Lexer(" 1.0f 2.1023123f 1.0e+123 ");
		Object[] output = lexer.getTokens().stream().map(t -> t.text).toArray();
		assertArrayEquals(Arrays.asList("1.0f", "2.1023123f", "1.0e+123").toArray(), output);		
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

	private void assertOneCharacter(String s) {
		lexer = new Lexer(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.CHAR, tk.kind);
		assertEquals(s, tk.text);
	}

	private void assertOneTokenNumber(String s) {
		lexer = new Lexer(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.NUMBER, tk.kind);
		assertEquals(s, tk.text);
	}
	private void assertOneTokenString(String s) {
		lexer = new Lexer(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.STRING, tk.kind);
		assertEquals(s, tk.text);
	}
	private void assertTokenError(String s) {
		lexer = new Lexer(s);
		Token tk = lexer.getToken();
		assertEquals(TokenKind.ERROR, tk.kind);
	}

}
