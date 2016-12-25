package intermediate.symtabimpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import intermediate.SymTab;
import intermediate.SymTabEntry;
import intermediate.SymTabKey;

public class SymTabEntryImpl implements SymTabEntry {
    private SymTab symTab;
    private String name;
    private ArrayList<Integer> lineNumbers = new ArrayList<Integer>();
    private Map<SymTabKey, Object> attributes = new HashMap<SymTabKey, Object>();

    public SymTabEntryImpl(SymTab symTab, String name) {
	this.symTab = symTab;
	this.name = name;
    }

    @Override
    public String getName() {
	return name;
    }

    @Override
    public SymTab getSymTab() {
	return symTab;
    }

    @Override
    public void appendLineNumber(int lineNumber) {
	lineNumbers.add(lineNumber);
    }

    @Override
    public ArrayList<Integer> getLineNumbers() {
	return lineNumbers;
    }

    @Override
    public void setAttribute(SymTabKey key, Object attribute) {
	attributes.put(key, attribute);
    }

    @Override
    public Object getAttribute(SymTabKey key) {
	return attributes.get(key);
    }

}
