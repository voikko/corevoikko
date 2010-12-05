/**
 * Java interface to libvoikko, library of Finnish language tools.
 * This module can be used to perform various natural language analysis
 * tasks.
 * In order to use this package you must have the native libvoikko
 * library available in a location where it can be found by the library
 * loader of the operating system.
 *
 * Simple example demonstrating the use of this library for spell checking:
<pre>
import org.puimula.libvoikko.Voikko;

public class VoikkoEsimerkki {

    public static void main(String[] args) {
        Voikko voikko = new Voikko("fi");
        System.out.println("Onko kissa oikein? " + voikko.spell("kissa"));
        System.out.println("Onko kisssa oikein? " + voikko.spell("kisssa"));
        System.out.println();
        System.out.println("Korjausehdotukset sanalle kisssa:");
        for (String suggestion : voikko.suggest("kisssa")) {
            System.out.println(suggestion);
        }
        System.out.println();
    }
}
</pre>
 */
package org.puimula.libvoikko;
