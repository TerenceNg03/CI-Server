package ci;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import org.json.simple.JSONObject;

/**
 * A class for the App.
 */
public class App {

  /**
   * Placeholder function for writing json to file.
   *
   * @param jsonObj the json object
   */
  public static void writeToFile(File file, JSONObject jsonObj) {
    try {
      FileWriter fileWriter = new FileWriter(file);
      fileWriter.write(jsonObj.toJSONString());
      fileWriter.close();

    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * The main class of the app.
   *
   * @param args arguments for the program
   * @throws IOException if the input fails
   */
  @SuppressWarnings("checkstyle:MagicNumber")
  public static void main(String[] args) throws IOException {
    String dirWithBuild = args[0];

    MavenHandler mavenHandler = MavenHandler.getInstance(dirWithBuild);
    System.out.println("Result of compilation: " + mavenHandler.compileProgram());
    System.out.println(" " + mavenHandler.getCompileLog());
    System.out.println("Result of tests: " + mavenHandler.runTests());
    System.out.println(" " + mavenHandler.getTestLog());

    //        File file = new File(".\\exampleOutput.json");
    //        JSONObject jsonObject = new JSONObject();
    //        jsonObject.put("compile", "true");
    //        writeToFile(file, jsonObject);
  }
}
