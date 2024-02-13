package ci;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import org.apache.commons.io.FileUtils;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;

/** A class for the App. */
public class App {

    /**
     * A function to stop the execution with a certain code. If the exit status is 1 the compilation
     * failed and the tests couldn't build. If the exit status is 2 the tests failed. If the exit
     * status is 0 everything is OK.
     *
     * @param compileStatus the status of the compilation
     * @param testStatus the status of the test
     */
    public static void exitSystem(final boolean compileStatus, final boolean testStatus) {
        if (!compileStatus) {
            System.exit(1);
        } else if (!testStatus) {
            System.exit(2);
        } else {
            System.exit(0);
        }
    }

    private static String makeTemporaryGitDirectory(String cloneUrl, String commitName)
            throws IOException, GitAPIException {
        File localPath = Files.createTempDirectory("CI").toFile();

        Git git =
                Git.cloneRepository()
                        .setURI(cloneUrl)
                        .setDirectory(localPath)
                        .setCloneAllBranches(true)
                        .call();

        git.checkout().setCreateBranch(true).setName(commitName).setStartPoint(commitName).call();

        git.close();

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
        boolean compileStatus = mavenHandler.compileProgram();
        boolean testStatus = mavenHandler.runTests();

        System.out.println(mavenHandler.getTestLog());

        FileUtils.deleteDirectory(new File(dirWithBuild));

        exitSystem(compileStatus, testStatus);
    }
}
