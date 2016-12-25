package backend;

import backend.compiler.CodeGenerator;
import backend.interpreter.Executor;

public class BackendFactory {
    public static Backend createBackend(String operation) throws Exception {
	if (operation.equalsIgnoreCase("compile")) {
	    return new CodeGenerator();
	} else if (operation.equalsIgnoreCase("execute")) {
	    return new Executor();
	} else {
	    throw new Exception("Invalid operation");
	}
    }
}
