package ci;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import org.json.simple.JSONObject;

/** A class for the App. */
public class App {
    /**
     * The main class of the app.
     *
     * @param args arguments for the program
     * @throws IOException if the input fails
     */
    public static void main(String[] args) throws IOException {
        String dirWithBuild = args[0];

        MavenHandler mavenHandler = MavenHandler.getInstance(dirWithBuild);
        String compileStatus = mavenHandler.compileProgram() ? "success" : "failure";
        String testStatus = mavenHandler.runTests() ? "success" : "failure";

        CIResultWriter ciResultWriter = new CIResultWriter();
        ciResultWriter.addResult("compile", compileStatus, mavenHandler.getCompileLog());
        ciResultWriter.addResult("test", testStatus, mavenHandler.getTestLog());
        ciResultWriter.uploadFile();

    }
}
