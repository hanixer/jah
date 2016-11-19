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
	public void testOtherFloating() {
		assertOneToken("1.01f");
	}

	private void assertOneToken(String s) {
		lexer = new Lexer(s);
		Token tk = lexer.getToken();
		assertTrue(tk.kind == TokenKind.NUMBER && tk.text == s);
	}

}
