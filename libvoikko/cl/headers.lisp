;; -*- truncate-lines:t -*-

(include "libvoikko/voikko_defines.h")

(in-package :voikko)

;;; Spell checker return codes
(constant (+spell-failed+ "VOIKKO_SPELL_FAILED"))
(constant (+spell-ok+ "VOIKKO_SPELL_OK"))
(constant (+internal-error+ "VOIKKO_INTERNAL_ERROR"))
(constant (+charset-conversion-failed+ "VOIKKO_CHARSET_CONVERSION_FAILED"))

;;; Boolean options
(constant (+opt-ignore-dot+ "VOIKKO_OPT_IGNORE_DOT"))
(constant (+opt-ignore-numbers+ "VOIKKO_OPT_IGNORE_NUMBERS"))
(constant (+opt-ignore-uppercase+ "VOIKKO_OPT_IGNORE_UPPERCASE"))
(constant (+opt-accept-first-uppercase+ "VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE"))
(constant (+opt-accept-all-uppercase+ "VOIKKO_OPT_ACCEPT_ALL_UPPERCASE"))
(constant (+opt-no-ugly-hyphenation+ "VOIKKO_OPT_NO_UGLY_HYPHENATION"))
(constant (+opt-ocr-suggestions+ "VOIKKO_OPT_OCR_SUGGESTIONS"))
(constant (+opt-ignore-nonwords+ "VOIKKO_OPT_IGNORE_NONWORDS"))
(constant (+opt-accept-extra-hyphens+ "VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS"))
(constant (+opt-accept-missing-hyphens+ "VOIKKO_OPT_ACCEPT_MISSING_HYPHENS"))
(constant (+opt-accept-titles-in-gc+ "VOIKKO_OPT_ACCEPT_TITLES_IN_GC"))
(constant (+opt-accept-unfinished-paragraphs-in-gc+ "VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC"))
(constant (+opt-hyphenate-unknown-words+ "VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS"))
(constant (+opt-accept-bulleted-lists-in-gc+ "VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC"))

;;; Integer options
(constant (+min-hyphenated-word-length+ "VOIKKO_MIN_HYPHENATED_WORD_LENGTH"))
(constant (+speller-cache-size+ "VOIKKO_SPELLER_CACHE_SIZE"))
