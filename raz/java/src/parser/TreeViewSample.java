package parser;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

public class TreeViewSample extends Application {
	TreeView<String> tree;
	TextField textField;
	
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
		Parser p = new Parser("a+++ 2");
		Parser.Node node = p.additiveExpression();

		primaryStage.setTitle("Tree View Sample");

		TreeItem<String> rootItem = createTreeItem(node);

		Button btn = new Button("ERERE");

		tree = new TreeView<>(rootItem);
		textField = new TextField("1 + 2 * 3");
		
		updateTree();
		
		Button button = new Button("Refresh");
		BorderPane root = new BorderPane();
		root.setCenter(tree);
		root.setTop(textField);
		root.setLeft(button);
		
		button.setOnAction(new EventHandler<ActionEvent>() {
			
			@Override
			public void handle(ActionEvent event) {
				// TODO Auto-generated method stub
				String s = textField.getText();
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