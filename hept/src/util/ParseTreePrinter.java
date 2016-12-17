package util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.Map.Entry;

import intermediate.ICode;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.ICodeNodeKey;
import intermediate.icodeimpl.ICodeNodeImpl;
import static intermediate.icodeimpl.ICodeNodeTypeImpl.*;
import static intermediate.icodeimpl.ICodeNodeKeyImpl.*;

public class ParseTreePrinter {
    private PrintStream stream;

    public ParseTreePrinter(PrintStream stream) {
	this.stream = stream;
    }

    public void print(ICode iCode) {
	print(iCode.getRootNode(), 0);
    }
    public void print(ICodeNode rawNode) {
	print(rawNode, 0);
    }
    
    public void print(ICodeNode rawNode, int level) {
	ICodeNodeImpl node = (ICodeNodeImpl) rawNode;  
	for (int i = 0; i < level; ++i) {
	    stream.print(" ");
	}
	
	stream.print("<" + node);

	for (Entry<ICodeNodeKey, Object> entry : node.entrySet()) {
	    stream.print(" " + entry.getKey().toString().toLowerCase() + 
		    "=" + "\"" + entry.getValue() + "\"");
	}
	
	stream.println(">");
	
	for (ICodeNode child : node.getChildren()) {
	    print(child, level + 1);
	}
	
	
	for (int i = 0; i < level; ++i) {
	    stream.print(" ");
	}
	stream.println("</" + node + ">");
    }
    
    public static void main(String[] args) throws FileNotFoundException {
	ICode iCode = ICodeFactory.createICode();
	ICodeNode n1 = ICodeFactory.createCodeNode(IF);
	n1.setAttribute(LINE, 20);
	ICodeNode n2 = ICodeFactory.createCodeNode(IDEXPR);
	n2.setAttribute(LINE, 30);
	n1.addChild(n2);
	
	iCode.setRootNode(n1);
	
	ParseTreePrinter printer = new ParseTreePrinter(System.out);
	printer.print(iCode);
    }
}
