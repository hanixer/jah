import java.util.ArrayList;
import java.util.Iterator;

public class Main2 {
    
    static Integer f(Integer x) {
	x = 10;
	return null;
    }

    public static void main(String[] args) {
	Integer n = new Integer(2);
	Integer jioj = f(n);
	ArrayList<Integer> a = new ArrayList<>();
	a.add(1);
	a.add(2);
	Iterator<Integer> it = a.iterator();
	System.out.println(it.next());
	System.out.println(it.next());
	//System.out.println(it.next());
	SimpleArithmeticParser p = new SimpleArithmeticParser("1 + 2");
	SimpleArithmeticParser.Node nd = p.parse();
	nd = null;
    }
}
