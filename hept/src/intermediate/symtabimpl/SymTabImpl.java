package intermediate.symtabimpl;

import java.util.ArrayList;
import java.util.HashMap;

import intermediate.SymTab;
import intermediate.SymTabEntry;
import intermediate.SymTabFactory;

public class SymTabImpl extends HashMap<String, SymTabEntry> implements SymTab {
    private static final long serialVersionUID = -8573477290991158554L;
    private int nestingLevel;
    
    public SymTabImpl(int nestingLevel) {
	this.nestingLevel = nestingLevel;
    }

    @Override
    public int getNestingLevel() {
	return nestingLevel;
    }

    @Override
    public SymTabEntry enter(String name) {
	SymTabEntry entry = SymTabFactory.createEntry(this, name);
	put(name, entry);
	return entry;
    }

    @Override
    public SymTabEntry lookup(String name) {
	return get(name);
    }

    @Override
    public ArrayList<SymTabEntry> sortedEntries() {
	ArrayList<SymTabEntry> entries = new ArrayList<>();
	for (SymTabEntry entry : values()) {
	    entries.add(entry);
	}
	
	return entries;
    }

}
