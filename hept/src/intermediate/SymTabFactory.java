package intermediate;

import intermediate.symtabimpl.SymTabEntryImpl;
import intermediate.symtabimpl.SymTabImpl;
import intermediate.symtabimpl.SymTabStackImpl;

public class SymTabFactory {
    
    public static SymTabStack createSymTabStack() {
	return new SymTabStackImpl();
    }
    
    public static SymTab createSymTab(int nestingLevel) {
	return new SymTabImpl(nestingLevel);
    }
    
    public static SymTabEntry createEntry(SymTab symTab, String name) {
	SymTabEntry entry = new SymTabEntryImpl(symTab, name);
	return entry;
    }
}
