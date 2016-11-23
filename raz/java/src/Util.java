import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Util {
    public static String readLine() {
	BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
	String result = "";
	try {
	    result = br.readLine();
	} catch (IOException e) {
	    System.err.println("Console read exception: " + e.getMessage());
	}

	return result;
    }

    public static String readFile(String path) throws IOException {
	byte[] encoded = Files.readAllBytes(Paths.get(path));
	return new String(encoded);
    }
}
