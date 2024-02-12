package ci;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.json.simple.JSONObject;

/** A class for the App. */
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

    private static String makeTemporaryGitDirectory(String cloneUrl, String commitName)
            throws IOException, GitAPIException {
        File localPath = Files.createTempDirectory("tmpDirPrefix").toFile();

        Git git =
                Git.cloneRepository()
                        .setURI(cloneUrl)
                        .setDirectory(localPath)
                        .setCloneAllBranches(true)
                        .call();

        git.checkout().setCreateBranch(true).setName(commitName).call();

        return localPath.getAbsolutePath();
    }

    /**
     * The main class of the app.
     *
     * @param args arguments for the program
     * @throws IOException if the input fails
     */
    public static void main(String[] args) throws IOException, GitAPIException {
        String cloneUrl = args[0];
        String commitName = args[1];

        String dirWithBuild = makeTemporaryGitDirectory(cloneUrl, commitName);

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
