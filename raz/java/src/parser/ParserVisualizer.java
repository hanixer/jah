package parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.stage.Stage;

public class ParserVisualizer extends Application {
    TreeView<String> tree;
    TextArea textArea;
    String initialExpression = "1,2,3";
    String filename = "some.cpp";

    public static void main(String[] args) {
	launch(args);
    }

    TreeItem<String> createTreeItem(SyntaxNode n) {
	if (n == null)
	    return null;
	TreeItem<String> ti = new TreeItem<>(n.toString());

	if (n.token != null) {
	    ti = new TreeItem<>(n.token.toString());
	} else {

	    for (SyntaxNode child : n.childs) {
		if (child != null) {
		    TreeItem<String> tiChild = createTreeItem(child);
		    ti.getChildren().add(tiChild);
		}
	    }
	    ti.setExpanded(true);
	}

	return ti;
    }

    @Override
    public void start(Stage primaryStage) {
	primaryStage.setTitle("Tree View Sample");

	String contents = readFile();

	tree = new TreeView<>(null);
	textArea = new TextArea(contents);

	updateStatement();

	Button button = new Button("Refresh");
	Button exprButton = new Button("Expression");
	Button stmtButton = new Button("Statement");
	FlowPane stackPane = new FlowPane();
	stackPane.getChildren().add(exprButton);
	stackPane.getChildren().add(stmtButton);

	BorderPane root = new BorderPane();
	root.setCenter(tree);
	root.setTop(textArea);
	root.setLeft(stackPane);

	button.setOnAction(new EventHandler<ActionEvent>() {
	    @Override
	    public void handle(ActionEvent event) {
		updateTree();
	    }
	});

	exprButton.setOnAction(new EventHandler<ActionEvent>() {
	    @Override
	    public void handle(ActionEvent event) {
		updateExpression();
	    }
	});

	stmtButton.setOnAction(new EventHandler<ActionEvent>() {
	    @Override
	    public void handle(ActionEvent event) {
		updateStatement();
	    }
	});

	primaryStage.setScene(new Scene(root, 1500, 850));
	primaryStage.show();
    }

    public String readFile() {
	String contents = "";
	try {
	    contents = new String(Files.readAllBytes(Paths.get(filename)));
	} catch (IOException e) {
	    e.printStackTrace();
	}
	return contents;
    }

    private void updateTree() {
	Parser p = new Parser(textArea.getText());
	SyntaxNode node = p.expression();
	tree.setRoot(createTreeItem(node));
    }

    private void updateExpression() {
	Parser p = new Parser(textArea.getText());
	SyntaxNode node = p.expression();
	tree.setRoot(createTreeItem(node));
    }

    private void updateStatement() {
	Parser p = new Parser(textArea.getText());
	SyntaxNode node = p.statement();
	tree.setRoot(createTreeItem(node));
    }
}