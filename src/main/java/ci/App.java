package ci;

import java.io.*;

/** Hello world! */
public class App {


    public String[] runBuild() throws Exception {
        String[] result = new String[2];
        final String mavenCmd = System.getProperty("os.name").toLowerCase().contains("windows") ? "mvn.cmd" : "mvn";
        String[] command = {mavenCmd, "package"};
        Process process = Runtime.getRuntime().exec(command);
        StringBuilder sb = new StringBuilder();
        new Thread(() -> {
            BufferedReader input = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line = null;
            try {
                while ((line = input.readLine()) != null) {
                    sb.append(line).append("\n");
                }
            }catch (IOException e ) {
                System.out.println("Faulty command: " + e.getMessage());
                e.printStackTrace();
            }
        }).start();
        try {
            process.waitFor();
            if (process.exitValue() != 0) {
                String message = sb.toString();
                //System.out.println("Build failed \n" + process.exitValue()+ " " + message);
                result[0] = "Fail";
                result[1] = message;
            }
            else {
                String message = sb.toString();
                //System.out.println("Build was successful \n" + message);
                result[0] = "Success";
                result[1] = message;
            }
        } catch (InterruptedException e ) {

            e.printStackTrace();
        }
        return result;
    }

    public static void main(String[] args) throws Exception {
        System.out.println("Hello World!");
        App app = new App();
        String[] log = app.runBuild();
        System.out.println("Result of build: " + log[0] + "\n " + log[1]);
    }
}
