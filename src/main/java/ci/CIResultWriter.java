package ci;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import org.json.simple.JSONObject;

/** Class to write the continuous integration result. */
public class CIResultWriter {
  private JSONObject jsonObject;
  private FileWriter fileWriter;

  /** Constructor that will write to a file in a certain path. */
  public CIResultWriter(final String path, final String filename) {
    File file = new File(path + filename);
    jsonObject = new JSONObject();
    try {
      fileWriter = new FileWriter(file);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /** Default constructor puts the output as output/output.json. */
  public CIResultWriter() {
   this("output\\", "output.json");
  }

  /**
   * Method that adds result to the json object, containing name, status and logs.
   *
   * @param featureName name of the CI feature
   * @param status status of the CI feature
   * @param logs logs of the CI feature
   */
  public void addResult(final String featureName, final String status, final String logs) {
    JSONObject result = new JSONObject();

    result.put("status", status);
    result.put("logs", logs);

    jsonObject.put(featureName, result);
  }

  /**
   * Method that uploads the json content to the file.
   *
   * @return if the upload was successful
   */
  public boolean uploadFile() {
    try {
      fileWriter.write(jsonObject.toJSONString());
      fileWriter.close();
      return true;
    } catch (IOException e) {
      e.printStackTrace();
    }
    return false;
  }
}
