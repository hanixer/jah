
public class SimpleArithStringParser {
	char[] s;
	int p;

	public SimpleArithStringParser(String input) {
		this.s = input.toCharArray();
		p = 0;
	}

	public Integer parse() {
		return expr();
	}

	boolean character(char c) {
		int pos = p;
		pos = skipWs(pos);

		if (correct(pos) && s[pos] == c) {
			pos++;
		} else
			return false;

		pos = skipWs(pos);
		p = pos;

		return true;
	}

	Integer number() {
		int pos = p;
		Integer i = null;

		pos = skipWs(pos);

		if (correct(pos) && Character.isDigit(s[pos])) {
			int start = pos;
			pos++;
			while (correct(pos) && Character.isDigit(s[pos]))
				pos++;
			i = new Integer(new String(s, start, pos - start));
		} else {
			return null;
		}

		pos = skipWs(pos);

		p = pos;
		return i;
	}

	Integer factor() {
		if (character('(')) {
			Integer e = expr();
			if (e != null && character(')')) {
				return e;
			}
		}

		return number();
	}

	Integer term() {
		Integer f = factor();
		if (f != null) {
			if (character('*')) {
				Integer t = term();
				if (t != null) {
					return new Integer(f * t);
				}
			} else
				return f;
		}

		return null;
	}

	Integer expr() {
		Integer t = term();
		if (t != null) {
			if (character('+')) {
				Integer e = expr();
				if (e != null) {
					return new Integer(t + e);
				}
			} else
				return t;
		}

		return null;
	}

	public int skipWs(int pos) {
		while (correct(pos) && Character.isWhitespace(s[pos]))
			pos++;
		return pos;
	}

	public boolean correct(int pos) {
		return pos < s.length;
	}
}
