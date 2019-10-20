export = Libvoikko;

declare class Libvoikko {
  constructor();
  init(language: string): Voikko;
}

declare interface GrammarError {
  errorCode: number;
  startPos: number;
  errorLen: number;
  suggestions: Array<string>;
  shortDescription: string;
}

declare interface Analysis {
  BASEFORM: string;
  CLASS: string;
  FSTOUTPUT: string;
  NUMBER: string;
  SIJAMUOTO: string;
  STRUCTURE: string;
  WORDBASES: string;
}

declare interface Token {
  type: TokenType;
  text: string;
}

type TokenType = 'WORD' | 'WHITESPACE' | 'PUNCTUATION';

declare interface Sentence {
  text: string;
  nextStartType: NextStartType;
}

type NextStartType = 'PROBABLE' | 'NONE';

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
  hyphenate: (word: string) => string;

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
