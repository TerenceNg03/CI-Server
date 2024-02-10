package ci;

import java.io.*;
import org.json.simple.JSONObject;

/** Hello world! */
public class App {

    /**
     * Placeholder function for writing json to file
     *
     * @param jsonObj
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
     * Builds and tests the repo with command mvn package
     *
     * @param dir the directory in which mvn package will execute
     * @return String[], storing the build status at index 0 and build log at index 1
     * @throws IOException
     */
    public String[] runBuild(String dir) throws IOException {
        String[] result = new String[2];
        final String mavenCmd =
                System.getProperty("os.name").toLowerCase().contains("windows") ? "mvn.cmd" : "mvn";
        String[] command = {mavenCmd, "-f", dir, "package"};
        Process process = Runtime.getRuntime().exec(command);
        StringBuilder sb = new StringBuilder();
        new Thread(
                        () -> {
                            BufferedReader input =
                                    new BufferedReader(
                                            new InputStreamReader(process.getInputStream()));
                            String line;
                            try {
                                while ((line = input.readLine()) != null) {
                                    sb.append(line).append("\n");
                                }
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        })
                .start();
        try {
            process.waitFor();
            String message = sb.toString();
            if (process.exitValue() != 0) {
                result[0] = "failure";
                result[1] = message;
            } else {
                result[0] = "success";
                result[1] = message;
            }
        } catch (InterruptedException e) {

            e.printStackTrace();
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        App app = new App();
        String dirWithBuild = "./";

        MavenHandler mavenHandler = MavenHandler.getInstance(dirWithBuild);
        System.out.println("Result of compilation " + mavenHandler.compileProgram());
        System.out.println("Result of tests: " + mavenHandler.runTests()); //mvn exev:java


        //        File file = new File(".\\exampleOutput.json");
        //        JSONObject jsonObject = new JSONObject();
        //        jsonObject.put("compile", "true");
        //        writeToFile(file, jsonObject);
    }
}
