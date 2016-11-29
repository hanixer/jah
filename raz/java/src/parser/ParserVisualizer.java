package parser;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class ParserVisualizer extends Application {
	TreeView<String> tree;
	TextField textField;
	String initialExpression = "a=b=c=5";
	
	public static void main(String[] args) {
		launch(args);
	}

	TreeItem<String> createTreeItem(Parser.Node n) {
		TreeItem<String> ti = new TreeItem<>(n.tag);

		if (n.token != null) {
			ti = new TreeItem<>(n.token.toString());
		} else {

			for (Parser.Node child : n.childs) {
				TreeItem<String> tiChild = createTreeItem(child);
				ti.getChildren().add(tiChild);
			}
			ti.setExpanded(true);
		}

		return ti;
	}
	
	@Override
	public void start(Stage primaryStage) {
		primaryStage.setTitle("Tree View Sample");

		tree = new TreeView<>(null);
		textField = new TextField(initialExpression);
		
		updateTree();
		
		Button button = new Button("Refresh");
		BorderPane root = new BorderPane();
		root.setCenter(tree);
		root.setTop(textField);
		root.setLeft(button);
		
		button.setOnAction(new EventHandler<ActionEvent>() {
			
			@Override
			public void handle(ActionEvent event) {
				updateTree();
			}
		});
		
		primaryStage.setScene(new Scene(root, 1500, 850));
		primaryStage.show();
	}

	private void updateTree() {
		Parser p = new Parser(textField.getText());
		Parser.Node node = p.assignmentExpression();
		tree.setRoot(createTreeItem(node));
	}
}