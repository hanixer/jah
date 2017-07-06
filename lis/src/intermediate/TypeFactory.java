package intermediate;

import intermediate.typespecimpl.TypeSpecImpl;

public class TypeFactory {
    public static TypeSpec createType(TypeForm typeForm) {
	return new TypeSpecImpl(typeForm);
    }
}
