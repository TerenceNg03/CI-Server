package ci;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/** MavenHandler class responsible for compilation and test of repository. */
public class MavenHandler {
    private static volatile MavenHandler instance = null;
    private final String dir;
    private final String mvnCmd;
    private String testLog;
    private String compileLog;

    private MavenHandler(final String dir) {
        this.dir = dir;
        this.mvnCmd =
                System.getProperty("os.name").toLowerCase().contains("windows") ? "mvn.cmd" : "mvn";
    }

    /**
     * Ensures that the class only has one instance.
     *
     * @param dir the path of the files
     * @return an instance of the MavenHandler class
     */
    public static MavenHandler getInstance(final String dir) {
        if (instance == null) {
            synchronized (MavenHandler.class) {
                if (instance == null) {
                    instance = new MavenHandler(dir);
                }
            }
        }
        return instance;
    }

    /**
     * Compiles the repo with command mvn compile.
     *
     * @return boolean if the compilation were successful or not
     * @throws IOException if the command input fails
     */
    public boolean compileProgram() throws IOException {
        Process process = Runtime.getRuntime().exec(new String[] {mvnCmd, "-f", dir, "compile"});

        compileLog = getOutput(process);

        return process.exitValue() == 0;
    }

    /**
     * Tests the repo with command mvn test.
     *
     * @return boolean if the tests were successful or not
     * @throws IOException if the command input fails
     */
    public boolean runTests() throws IOException {
        Process process = Runtime.getRuntime().exec(new String[] {mvnCmd, "-f", dir, "test"});

        testLog = getOutput(process);

        return process.exitValue() == 0;
    }

    /**
     * Runs the process and returns the logs.
     *
     * @param process the process
     * @return the logs
     */
    private String getOutput(Process process) {
        StringBuilder sb = new StringBuilder();
        new Thread(
                        () -> {
                            BufferedReader output =
                                    new BufferedReader(
                                            new InputStreamReader(process.getInputStream()));
                            String line;
                            try {
                                while ((line = output.readLine()) != null) {
                                    sb.append(line).append("\n");
                                }
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        })
                .start();
        try {
            process.waitFor();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return sb.toString();
    }

    /**
     * Gets the test log.
     *
     * @return testLog
     */
    public String getTestLog() {
        return testLog;
    }

    /**
     * Gets the compilation log.
     *
     * @return compileLog
     */
    public String getCompileLog() {
        return compileLog;
    }
}
