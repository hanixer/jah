

import static org.junit.Assert.*;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SASPTest {

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void test() {
	assertOutput("1", 1);
	assertOutput("1 + 2", 3);
	assertOutput("( (2 + 3 ) + 1 )  * 4", 24);
	assertOutput("1+2*3", 7);
	assertOutput("(1+2)*3", 9);
    }

    public void assertOutput(String input, int expected) {
	SimpleArithStringParser p = new SimpleArithStringParser(input);
	int result = p.parse();
	assertEquals(expected, result);
    }

}
