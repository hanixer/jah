package intermediate;

public interface SymTabStack {
    int getCurrentNestingLevel();
    SymTab getLocalSymTab();
    SymTabEntry enterLocal(String name);
    SymTabEntry lookupLocal(String name);
    SymTabEntry lookup(String name);
    
    SymTab push();
    SymTab push(SymTab symTab);
    SymTab pop();
}
