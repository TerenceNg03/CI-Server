package ci;

import java.io.*;

/** Hello world! */
public class App {


    public void runBuild() throws Exception {
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
                System.out.println("Build failed \n" + process.exitValue()+ " " + message);
            }
            else {
                String message = sb.toString();
                System.out.println("Build was successful \n" + message);
            }
        } catch (InterruptedException e ) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }

    }

    public static void main(String[] args) throws Exception {
        System.out.println("Hello World!");
        App app = new App();
        app.runBuild();
    }
}
