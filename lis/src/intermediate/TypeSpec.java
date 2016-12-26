package intermediate;

public interface TypeSpec {
    public TypeForm getForm();
    public SymTabEntry getIdentifier();
    public void setAttribute(TypeKey key, Object value);
    public Object getAttribute(TypeKey key);
    public boolean isString();
    public TypeSpec baseType();
    void setIdentifier(SymTabEntry symTabEntry);
}
