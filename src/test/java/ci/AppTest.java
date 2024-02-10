package ci;

import static org.junit.Assert.*;

import java.io.IOException;
import org.junit.Test;

/** Unit test for simple App. */
public class AppTest {
    /** Rigorous Test :-) */
    @Test
    public void shouldAnswerWithTrue() {
        assertTrue(true);
    }

    @Test
    public void runBuildShouldFailIfFaultyDirectory() throws IOException {
        MavenHandler mavenHandler = MavenHandler.getInstance("...");

        assertFalse(mavenHandler.compileProgram());
    }

    @Test
    public void runTestShouldFailIfFaultyDirectory() throws IOException {
        MavenHandler mavenHandler = MavenHandler.getInstance("...");

        assertFalse(mavenHandler.runTests());
    }

    // @Test
    public void runBuildAlwaysFailTest() {
        fail();
    }
}
