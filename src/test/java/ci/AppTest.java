package ci;

import static org.junit.Assert.*;

import java.io.IOException;
import org.junit.Test;

/** Unit test for simple App. */
public class AppTest extends App {
    /** Rigorous Test :-) */
    @Test
    public void shouldAnswerWithTrue() {
        // App.main(null);
        assertTrue(true);
    }

    @Test
    public void runBuildShouldFailIfFaultyDirectory() throws IOException {
        String[] result = runBuild("...");
        assertTrue(result[0].equals("Fail"));
    }

    // @Test
    public void runBuildAlwaysFailTest() {
        assertTrue(false);
    }
}
