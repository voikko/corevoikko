package org.puimula.libvoikko;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class VoikkoTest {

  private Voikko voikko;

  @Before
  public void setUp() {
    voikko = new Voikko("fi");
  }
  
  @After
  public void tearDown() {
    voikko.terminate();
  }
  
  @Test
  public void initAndTerminate() {
    // do nothing, just check that setUp and tearDown complete succesfully
  }
  
  @Test
  public void spell() {
    assertTrue(voikko.spell("määrä"));
    assertFalse(voikko.spell("määä"));
  }
}
