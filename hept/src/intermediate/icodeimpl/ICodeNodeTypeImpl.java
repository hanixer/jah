package intermediate.icodeimpl;

import intermediate.ICodeNodeType;

public enum ICodeNodeTypeImpl implements ICodeNodeType {
    PROGRAM, FUNCTION,
    
    // Statements
    COMPOUND, LOOP, TEST, CALL, PARAMETERS,
    IF, SELECT, SELECT_BRANCH, SELECT_CONSTANTS, NO_OP,
    
    // Expressions
    PRIMARY, IDEXPR, POSTFIX, UNARY, BINARY, CONDITIONAL, ASSIGNMENT, COMMA,
}
