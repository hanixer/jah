package lis;

import java.util.ArrayList;

import intermediate.SymTabEntry;
import intermediate.SymTabStack;

public class CrossReferencer {

    public CrossReferencer() {
    }

    public void print(SymTabStack stack) {
	ArrayList<SymTabEntry> entries = stack.getLocalSymTab().sortedEntries();
	
	for (SymTabEntry entry : entries) {
	    System.out.print(String.format("%-20s", entry.getName()));
	    for (int line : entry.getLineNumbers()) {
		System.out.print(String.format("%03d ", line));
	    }
	    System.out.println();
	}
    }
}
