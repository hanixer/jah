package intermediate;

import java.util.ArrayList;

public interface SymTabEntry {
    String getName();
    SymTab getSymTab();
    void appendLineNumber(int lineNumber);
    ArrayList<Integer> getLineNumbers();
    void setAttribute(SymTabKey key, Object attribute);
    Object getAttribute(SymTabKey key);
}
