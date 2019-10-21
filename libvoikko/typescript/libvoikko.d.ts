export = Libvoikko;

declare class Libvoikko {
  constructor();
  /**
   * Creates a new Voikko instance.
   *
   * @param language BCP 47 language tag to be used
   * @param path Extra path that will be checked first when looking for linguistic resources
   */
  init(language: string, path?: string): Voikko;
}

declare interface GrammarError {
  /** Error code describing the type of error. */
  errorCode: number;
  /** Start of the error segment within the paragraph */
  startPos: number;
  /** Length of the error segment */
  errorLen: number;
  /** List of suggested replacements for the marked error */
  suggestions: Array<string>;
  /** Human readable short description for the error. */
  shortDescription: string;
}

declare interface Analysis {
  /**
   * Base form of the given word.
   *
   * Examples:
   *   Word: kissalla -> BASEFORM: kissa
   */
  BASEFORM?: string;
  /**
   * Finnish language specific attribute.
   *
   * Sanan sanaluokka. Attribuutti on käytössä libvoikon sisällä.
   * Attribuutin mahdolliset arvot ovat seuraavat:
   *  - nimisana (yleisnimi)
   *  - laatusana
   *  - nimisana_laatusana (sama kuin erilliset analyysit nimisanana ja laatusanana)
   *  - teonsana
   *  - seikkasana
   *  - asemosana
   *  - suhdesana
   *  - huudahdussana
   *  - sidesana
   *  - etunimi
   *  - sukunimi
   *  - paikannimi
   *  - nimi (muu erisnimi kuin etu-, suku- tai paikannimi)
   *  - kieltosana
   *  - lyhenne
   *  - lukusana
   *  - etuliite
   */
  CLASS?: string;
  /**
   * Word is comparable (adjective). Suggested values for this attribute are
   * "positive", "comparative" and "superlative".
   *
   * Examples:
   *   Word: sininen   -> COMPARISON: positive
   *   Word: sinisempi -> COMPARISON: comparative
   *   Word: sinisin   -> COMPARISON: superlative
   */
  COMPARISON?: string;
  /**
   * Analyzers that are implemented using finite state transducers can provide
   * the raw transducer output using this attribute.
   *
   * Examples:
   *   Word: kissalla -> FSTOUTPUT: [Ln][Xp]kissa[X][Xs]505527[X]kissa[Sade][Ny]lla
   */
  FSTOUTPUT?: string;
  /**
   * Finnish language specific attribute.
   *
   * Sanaan liittyy fokuspartikkeli -kin tai -kAAn.
   *
   * Esimerkkejä:
   *   Sana: kissakin  -> FOCUS: kin
   *   Sana: kissakaan -> FOCUS: kaan
   */
  FOCUS?: string;
  /**
   * Finnish language specific attribute.
   *
   * Sanaan liittyy kysymysliite -ko tai -kö. Attribuutin ainoa sallittu
   * arvo on "true". Jos sanaan ei liity kysymysliitettä, attribuuttia ei ole.
   */
  KYSYMYSLIITE?: string;
  /**
   * Mood of a verb. Suggested values for this attribute are
   * "indicative", "conditional", "imperative" and "potential".
   *
   * Examples:
   *   Word: juoksen -> MOOD: indicative
   *   Word: juoksisin -> MOOD: conditional
   */
  MOOD?: string;
  /**
   * For all verbs this attribute indicates whether the verb is in
   * a connegative form. Suggested values: "false", "true", "both"
   *
   * Examples:
   *   Word: sallitaan -> NEGATIVE: false
   *   Word: sallita (as in "ei sallita") -> NEGATIVE: true
   *   Word: maalaa (also "ei maalaa") -> NEGATIVE: both
   */
  NEGATIVE?: string;
  /**
   * Grammatical number of the word. Suggested values for this attribute
   * are "singular", "dual", "trial" and "plural".
   *
   * Examples:
   *   Word: kissa -> NUMBER: singular
   *   Word: kissat -> NUMBER: plural
   */
  NUMBER?: string;
  /**
   * Word is a participle of some sort. Suggested values for this attribute
   * are "present_active", "present_passive", "past_active", "past_passive",
   * "agent" and "negation" (add more as needed).
   *
   * Examples:
   *   Word: juokseva    -> PARTICIPLE: present_active
   *   Word: juostava    -> PARTICIPLE: present_passive
   *   Word: juossut     -> PARTICIPLE: past_active
   *   Word: juostu      -> PARTICIPLE: past_passive
   *   Word: juoksema    -> PARTICIPLE: agent
   *   Word: juoksematon -> PARTICIPLE: negation
   */
  PARTICIPLE?: string;
  /**
   * For verbs in active voice this attribute represents the person
   * (first, second or third). The person for passive voice can be
   * considered as the fourth voice if appropriate for the language.
   * Suggested values for this attribute are "1", "2", "3" and "4".
   *
   * Examples:
   *   Word: juoksen -> PERSON: 1
   *   Word: juokset -> PERSON: 2
   */
  PERSON?: string;
  /**
   * Word contains information about possessor. For now this is used to
   * indicate the use of possessive suffix in Finnish nouns.
   *
   * Examples:
   *   Word: kissani  -> POSSESSIVE: 1s
   *   Word: kissasi  -> POSSESSIVE: 2s
   *   Word: kissamme -> POSSESSIVE: 1p
   *   Word: kissanne -> POSSESSIVE: 2p
   *   Word: kissansa -> POSSESSIVE: 3
   */
  POSSESSIVE?: string;
  /**
   * Finnish language specific attribute.
   *
   * Nominin sijamuoto. Attribuutti on käytössä libvoikon sisällä.
   * Attribuutin mahdolliset arvot ovat seuraavat:
   *  - nimento
   *  - omanto
   *  - osanto
   *  - olento
   *  - tulento
   *  - kohdanto
   *  - sisaolento
   *  - sisaeronto
   *  - sisatulento
   *  - ulkoolento
   *  - ulkoeronto
   *  - ulkotulento
   *  - vajanto
   *  - seuranto
   *  - keinonto
   *  - kerrontosti (esim. "nopeasti")
   */
  SIJAMUOTO?: string;
  /**
   * This attribute describes morpheme boundaries, character case and
   * hyphenation restrictions for the word. The following characters are
   * used in the values of this attribute:
   *
   * = Start of a new morpheme. This must also be present at the start
   *   of a word.
   *
   * - Hyphen. Word can be split in text processors after this character
   *   without inserting an extra hyphen. If the hyphen is at morpheme
   *   boundary, the boundary symbol = must be placed after the hyphen.
   *
   * p Letter that is written in lower case in the standard form.
   *
   * q Letter that is written in lower case in the standard form.
   *   Hyphenation is forbidden before this letter.
   *
   * i Letter that is written in upper case in the standard form.
   *
   * j Letter that is written in upper case in the standard form.
   *   Hyphenation is forbidden before this letter.
   *
   * Examples:
   *   Word: Matti-niminen -> STRUCTURE: =ipppp-=ppppppp
   *   Word: DNA-näyte ->     STRUCTURE: =jjj-=ppppp
   *   Word: autokauppa ->    STRUCTURE: =pppp=pppppp
   */
  STRUCTURE?: string;
  /**
   * Tense and aspect of a verb. Suggested values for this attribute are
   * "past_imperfective", "present_simple", (add more as needed).
   *
   * Examples:
   *   Word: juoksen -> TENSE: present_simple
   *   Word: juoksin -> TENSE: past_imperfective
   */
  TENSE?: string;
  /**
   * Finnish language specific attribute.
   *
   * Sanan osien perusmuodot. Attribuutti ei ole käytössä libvoikon
   * sisällä. Attribuutin arvona on sanan perusmuoto, jossa yhdyssanan
   * osat ja päätteet on erotettu toisistaan +-merkillä. Lisäksi kunkin
   * yhdyssanan osan perusmuoto on osan perässä suluissa. Mikäli yhdyssanan
   * osat itsessään ovat jaettavissa osiin, osat voidaan sulkujen sisällä
   * olevassa perusmuodossa erotella merkeillä = tai |.
   *
   * Esimerkkejä:
   *   Sana: köydenvetoa ->     WORDBASES: +köyde(köysi)+n+veto(veto)
   *   Sana: Alkio-opistossa -> WORDBASES: +alkio(Alkio)+-+opisto(opisto)
   *                                       +alkio(alkio)+-+opisto(opisto)
   *
   * Johdinpäätteiden perusmuodot ovat suluissa siten, että päätteen edessä
   * on +-merkki:
   *   Sana: kansalliseepos ->  WORDBASES: +kansa(kansa)+llis(+llinen)+eepos(eepos)
   */
  WORDBASES?: string;
  /**
   * Finnish language specific attribute.
   *
   * Viittaukset sanan osiin Joukahaisessa. Attribuutti ei ole käytössä
   * libvoikon sisällä. Attribuutin arvona on sanan perusmuoto, jossa
   * yhdyssanan osat ja päätteet on erotettu toisistaan +-merkillä.
   * Lisäksi kunkin yhdyssanan osan tietue-id on osan perässä suluissa.
   *
   * Esimerkkejä:
   *   Sana: köydenvetoa ->     WORDBASES: +köyde(w506953)+n+veto(w517284)
   *                                       +köyde(w506953)+n+veto(w523540)
   *                                       +köyde(w506953)+n+veto(w525160)
   *   Sana: Alkio-opistossa -> WORDBASES: +alkio(w518215)+-+opisto(w510148)
   *                                       +alkio(w500068)+-+opisto(w510148)
   */
  WORDIDS?: string;
}

declare interface Token {
  type: TokenType;
  text: string;
}

type TokenType = 'WORD' | 'WHITESPACE' | 'PUNCTUATION' | 'NONE' | 'UNKNOWN';

declare interface Sentence {
  text: string;
  nextStartType: NextStartType;
}

type NextStartType = 'PROBABLE' | 'NONE' | 'POSSIBLE' | 'NO_START';

type SuggestionStrategy = 'OCR' | 'TYPO';

declare interface Voikko {
  /**
   * Releases the resources allocated by libvoikko for this instance.
   */
  terminate: () => void;

  /**
   * Check the spelling of given word. Return true if the word is correct,
   * false if it is incorrect.
   */
  spell: (word: string) => boolean;

  /**
   * a list of suggested spellings for given (misspelled) word. If the given
   * word is correct, the list contains only the word itself
   */
  suggest: (word: string) => Array<string>;

  /**
   * Check the given text for grammar errors and return a list of
   * GrammarError objects representing the errors that were found.
   * Unlike the C based API this method accepts multiple paragraphs
   * separated by newline characters.
   */
  grammarErrors: (text: string, language?: string) => Array<GrammarError>;

  /**
   * Analyze the morphology of given word and return the list of analysis results.
   */
  analyze: (word: string) => Array<Analysis>;

  /**
   * Split the given natural language text into a list of Token objects.
   */
  tokens: (text: string) => Array<Token>;

  /**
   * Split the given natural language text into a list of Sentence objects.
   */
  sentences: (text: string) => Array<Sentence>;

  /**
   * Return a character pattern that describes the hyphenation of given word.
   *
   * ' ' = no hyphenation at this character,
   * '-' = hyphenation point (character at this position is preserved in the hyphenated form),
   * '=' = hyphentation point (character at this position is replaced by the hyphen.)
   */
  getHyphenationPattern: (word: string) => string;

  /**
   * Returns the given word in fully hyphenated form.
   */
  hyphenate: (
    word: string,
    separator?: string,
    allowContextChanges?: boolean,
  ) => string;

  /**
   * Ignore dot at the end of the word (needed for use in some word processors).
   * If this option is set and input word ends with a dot, spell checking and
   * hyphenation functions try to analyze the word without the dot if no
   * results can be obtained for the original form. Also with this option,
   * string tokenizer will consider trailing dot of a word to be a part of
   * that word. Default: false
   */
  setIgnoreDot: (value: boolean) => void;

  /**
   * Ignore words containing numbers. Default: false
   */
  setIgnoreNumbers: (value: boolean) => void;

  /**
   * Accept words that are written completely in uppercase letters without
   * checking them at all. Default: false
   */
  setIgnoreUppercase: (value: boolean) => void;

  /**
   * Accept words even when the first letter is in uppercase
   * (start of sentence etc.) Default: true
   */
  setAcceptFirstUppercase: (value: boolean) => void;

  /**
   * Accept words even when all of the letters are in uppercase.
   * Note that this is not the same as setIgnoreUppercase: with this option
   * the word is still checked, only case differences are ignored. Default: true
   */
  setAcceptAllUppercase: (value: boolean) => void;

  /**
   * (Spell checking only): Ignore non-words such as URLs and email addresses.
   * Default: true
   */
  setIgnoreNonwords: (value: boolean) => void;

  /**
   * (Spell checking only): Allow some extra hyphens in words. This option
   * relaxes hyphen checking rules to work around some unresolved issues in
   * the underlying morphology, but it may cause some incorrect words to
   * be accepted. The exact behaviour (if any) of this option is not specified.
   * Default: false
   */
  setAcceptExtraHyphens: (value: boolean) => void;

  /**
   * (Spell checking only): Accept missing hyphens at the start and end of
   * the word. Some application programs do not consider hyphens to be word
   * characters. This is reasonable assumption for many languages but not
   * for Finnish. If the application cannot be fixed to use proper tokenization
   * algorithm for Finnish, this option may be used to tell libvoikko to work
   * around this defect. Default: false
   */
  setAcceptMissingHyphens: (value: boolean) => void;

  /**
   * (Grammar checking only): Accept incomplete sentences that could occur in
   * titles or headings. Set this option to true if your application is not
   * able to differentiate titles from normal text paragraphs, or if you know
   * that you are checking title text. Default: false
   */
  setAcceptTitlesInGc: (value: boolean) => void;

  /**
   * (Grammar checking only): Accept incomplete sentences at the end of the
   * paragraph. These may exist when text is still being written. Default: false
   */
  setAcceptUnfinishedParagraphsInGc: (value: boolean) => void;

  /**
   * (Grammar checking only): Accept paragraphs if they would be valid within
   * bulleted lists. Default: false
   */
  setAcceptBulletedListsInGc: (value: boolean) => void;

  /**
   * Do not insert hyphenation positions that are considered to be ugly but
   * correct Default: false
   */
  setNoUglyHyphenation: (value: boolean) => void;

  /**
   * (Hyphenation only): Hyphenate unknown words. Default: true
   */
  setHyphenateUnknownWords: (value: boolean) => void;

  /**
   * The minimum length for words that may be hyphenated. This limit is also
   * enforced on individual parts of compound words. Default: 2
   */
  setMinHyphenatedWordLength: (value: boolean) => void;

  /**
   * Set the suggestion strategy to be used when generating spelling
   * suggestions. Default: TYPO
   */
  setSuggestionStrategy: (suggestionStrategy: SuggestionStrategy) => void;
}
