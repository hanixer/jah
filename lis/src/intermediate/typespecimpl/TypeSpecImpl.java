package intermediate.typespecimpl;

import java.util.HashMap;

import intermediate.SymTab;
import intermediate.SymTabEntry;
import intermediate.TypeForm;
import intermediate.TypeKey;
import intermediate.TypeSpec;

public class TypeSpecImpl extends HashMap<TypeKey, Object> implements TypeSpec {
    private static final long serialVersionUID = -4221123053078268887L;
    private TypeForm typeForm;
    private SymTabEntry symTabEntry;

    public TypeSpecImpl(TypeForm typeForm) {
	this.typeForm = typeForm;
    }

    @Override
    public TypeForm getForm() {
	return typeForm;
    }

    @Override
    public void setIdentifier(SymTabEntry symTabEntry) {
	this.symTabEntry = symTabEntry;
    }

    @Override
    public SymTabEntry getIdentifier() {
	return symTabEntry;
    }

    @Override
    public void setAttribute(TypeKey key, Object value) {
	put(key, value);
    }

    @Override
    public Object getAttribute(TypeKey key) {
	return get(key);
    }

    @Override
    public boolean isString() {
	return false;
    }

    @Override
    public TypeSpec baseType() {
	// TODO Auto-generated method stub
	return null;
    }

}
