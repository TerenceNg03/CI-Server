package ci;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import org.json.simple.JSONObject;

/** Hello world! */
public class App {

    /**
     *
     * @param file
     */
    public static void readInput(File file) {
    }

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

    public static void main(String[] args) {
        //String input = args[1];
        //System.out.println(input);
        File file = new File(".\\exampleOutput.json");
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("compile", "true");
        writeToFile(file, jsonObject);
    }
}
