package intermediate.symtabimpl;

import intermediate.Definition;
import intermediate.SymTabEntry;
import intermediate.SymTabStack;
import intermediate.TypeFactory;
import intermediate.TypeForm;
import intermediate.TypeSpec;

public class Predefined {
    public static TypeSpec integerType;
    public static TypeSpec doubleType;
    public static TypeSpec booleanType;
    public static TypeSpec charType;
    public static TypeSpec undefinedType;

    public static SymTabEntry integerId;
    public static SymTabEntry doubleId;
    public static SymTabEntry booleanId;
    public static SymTabEntry charId;
    public static SymTabEntry trueId;
    public static SymTabEntry falseId;
    
    public static void initialize(SymTabStack stack) {
	integerId = stack.enterLocal("Int");
	integerType = TypeFactory.createType(TypeForm.SCALAR);
	integerType.setIdentifier(integerId);
	integerId.setDefinition(Definition.TYPE);
	integerId.setTypeSpec(integerType);

	doubleId = stack.enterLocal("Double");
	doubleType = TypeFactory.createType(TypeForm.SCALAR);
	doubleType.setIdentifier(doubleId);
	doubleId.setDefinition(Definition.TYPE);
	doubleId.setTypeSpec(doubleType);

	booleanId = stack.enterLocal("Boolean");
	booleanType = TypeFactory.createType(TypeForm.SCALAR);
	booleanType.setIdentifier(booleanId);
	booleanId.setDefinition(Definition.TYPE);
	booleanId.setTypeSpec(booleanType);

	charId = stack.enterLocal("Char");
	charType = TypeFactory.createType(TypeForm.SCALAR);
	charType.setIdentifier(charId);
	charId.setDefinition(Definition.TYPE);
	charId.setTypeSpec(charType);
	
	undefinedType = TypeFactory.createType(TypeForm.SCALAR);
    }
}
