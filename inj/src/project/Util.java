package project;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

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
}
