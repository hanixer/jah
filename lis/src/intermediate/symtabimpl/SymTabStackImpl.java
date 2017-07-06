package intermediate.symtabimpl;

import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Stack;

import intermediate.SymTab;
import intermediate.SymTabEntry;
import intermediate.SymTabFactory;
import intermediate.SymTabStack;

public class SymTabStackImpl extends ArrayList<SymTab> implements SymTabStack {
    private static final long serialVersionUID = 7662493049696936778L;
    
    private int nestingLevel;

    public SymTabStackImpl() {
	add(SymTabFactory.createSymTab(nestingLevel));
    }

    @Override
    public int getCurrentNestingLevel() {
	return nestingLevel;
    }

    @Override
    public SymTab getLocalSymTab() {
	return get(nestingLevel);
    }

    @Override
    public SymTabEntry enterLocal(String name) {
	return get(nestingLevel).enter(name);
    }

    @Override
    public SymTabEntry lookupLocal(String name) {
	return get(nestingLevel).lookup(name);
    }

    @Override
    public SymTabEntry lookup(String name) {
	for (int i = nestingLevel; i >= 0; --i) {
	    SymTabEntry entry =  get(i).lookup(name);
	    if (entry != null) {
		return entry;
	    }
	}
	
	return null;
    }

    @Override
    public SymTab push() {
	SymTab symTab = SymTabFactory.createSymTab(++nestingLevel);
	add(symTab);
	
	return symTab;
    }

    @Override
    public SymTab push(SymTab symTab) {
	++nestingLevel;
	add(symTab);
	return symTab;
    }

    @Override
    public SymTab pop() {
	SymTab symTab = get(nestingLevel);
	remove(nestingLevel);
	nestingLevel--;
	
	return symTab;
    }

}
