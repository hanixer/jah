package parser;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ParserTest {

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testExpression() {
	expr("1");
	expr("a");
	expr("'a'");
	expr("'a'");
	expr("\"a\"");
	expr("a++");
	expr("--B");
	expr("~a");
	expr("*a");
	expr("1 << 2 >> 3 + 5 * 6 == 5 || b * b == 20 > 20 && 21 <= a++ && 2 | 3 && 3 & 5");
	expr("1 + 2 == 3 ? 5 : 6");
	expr("a = b");
	expr("r += 5");
	expr("a = b = c");
	expr("5 + 2, 2 + 5, 3 + 3");
	expr("sizeof a");
    }

    public void expr(String s) {
	Parser p = new Parser(s);
	assertNotNull(p.expression());
    }

}
