package frontend;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.StringReader;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SourceTest {

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void test() throws Exception {
	String s = "ab\ncd";
	BufferedReader r = new BufferedReader(new StringReader(s));
	Source src = new Source(r);

	assertEquals('a', src.currentChar());
	assertEquals('b', src.peekChar());
	assertEquals('b', src.nextChar());
	assertEquals(Source.EOL, src.peekChar());
	assertEquals(Source.EOL, src.nextChar());
	assertEquals(Source.EOL, src.currentChar());
	assertEquals('c', src.nextChar());
	assertEquals('d', src.peekChar());
	assertEquals('d', src.nextChar());
	assertEquals(Source.EOL, src.nextChar());
	assertEquals(Source.EOL, src.currentChar());
	assertEquals(Source.EOL, src.peekChar());
	assertEquals(Source.EOF, src.nextChar());
    }

    @Test
    public void testNext() throws Exception {
	String s = "ab\ncd";
	BufferedReader r = new BufferedReader(new StringReader(s));
	Source src = new Source(r);

	assertEquals(Source.EOL, src.nextChar());
    }

    @Test
    public void testEmpty() throws Exception {
	String s = "";
	BufferedReader r = new BufferedReader(new StringReader(s));
	Source src = new Source(r);

	assertEquals(Source.EOF, src.currentChar());
    }

}
