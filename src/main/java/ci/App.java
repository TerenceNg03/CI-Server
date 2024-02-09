package ci;

import java.io.*;
import org.json.simple.JSONObject;


/** Hello world! */
public class App {

    /**
     * @param file
     */
    public static void readInput(File file) {}

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
                result[0] = "Fail";
                result[1] = message;
            } else {
                result[0] = "Success";
                result[1] = message;
            }
        } catch (InterruptedException e) {

            e.printStackTrace();
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        // String input = args[1];
        // System.out.println(input);
        App app = new App();
        String dirWithBuild = "./";
        String[] log = app.runBuild(dirWithBuild);
        System.out.println("Result of build: " + log[0] + "\n " + log[1]);
        File file = new File(".\\exampleOutput.json");
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("compile", "true");
        writeToFile(file, jsonObject);
        System.out.println(jsonObject);
    }
}
