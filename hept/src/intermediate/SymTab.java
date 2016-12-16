package intermediate;

import java.util.ArrayList;

public interface SymTab {
    int getNestingLevel();
    SymTabEntry enter(String name);
    SymTabEntry lookup(String name);
    ArrayList<SymTabEntry> sortedEntries();
}
