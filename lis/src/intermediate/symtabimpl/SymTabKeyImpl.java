package intermediate.symtabimpl;

import intermediate.SymTabKey;

public enum SymTabKeyImpl implements SymTabKey {
    CONSTANT_VALUE,
    
    FUNCTION_CODE, FUNCTION_SYMTAB, FUNCTION_ICODE,
    FUNCTION_PARAMS, FUNCTION_FUNCTIONS,
    
    DATA_VALUE, 
    INIT_EXPRESSION,
}
