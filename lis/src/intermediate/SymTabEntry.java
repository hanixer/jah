package intermediate;

import java.util.ArrayList;

public interface SymTabEntry {
    String getName();
    SymTab getSymTab();
    void appendLineNumber(int lineNumber);
    ArrayList<Integer> getLineNumbers();
    void setAttribute(SymTabKey key, Object attribute);
    Object getAttribute(SymTabKey key);
    void setDefinition(Definition definition);
    Definition getDefinition();
    void setTypeSpec(TypeSpec typeSpec);
    TypeSpec getTypeSpec();
}
