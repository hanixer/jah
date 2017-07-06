package intermediate.icodeimpl;

import intermediate.ICodeNodeType;

public enum ICodeNodeTypeImpl implements ICodeNodeType {
    PROGRAM, FUNCTION,
    
    // Statements
    COMPOUND, LOOP, TEST, CALL, PARAMETERS,
    IF, SELECT, SELECT_BRANCH, SELECT_CONSTANTS, NO_OP, EXPRESSION_STMT,
    
    // Operators
    POSTFIX, PREFIX, BINARY, CONDITIONAL, ASSIGNMENT, COMMA,
    
    // Operands
    VARIABLE, INTEGER_CONSTANT, REAL_CONSTANT, CHAR_CONSTANT, STRING_CONSTANT,
    ;
    
    
    
}
