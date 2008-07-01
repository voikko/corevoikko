/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

#include "voikko_defs.h"
#include "voikko_setup.h"
#ifdef HAVE_GETPWUID_R
#include <pwd.h>
#endif // HAVE_GETPWUID_R
#include <malaga.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#endif

voikko_options_t voikko_options;

int voikko_handle_count;

int voikko_set_bool_option(int handle, int option, int value) {
	switch (option) {
		case VOIKKO_OPT_IGNORE_DOT:
			if (value) voikko_options.ignore_dot = 1;
			else voikko_options.ignore_dot = 0;
			return 1;
		case VOIKKO_OPT_IGNORE_NUMBERS:
			if (value) voikko_options.ignore_numbers = 1;
			else voikko_options.ignore_numbers = 0;
			return 1;
		case VOIKKO_OPT_IGNORE_UPPERCASE:
			if (value) voikko_options.ignore_uppercase = 1;
			else voikko_options.ignore_uppercase = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE:
			if (value) voikko_options.accept_first_uppercase = 1;
			else voikko_options.accept_first_uppercase = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_ALL_UPPERCASE:
			if (value) voikko_options.accept_all_uppercase = 1;
			else voikko_options.accept_all_uppercase = 0;
			return 1;
		case VOIKKO_OPT_NO_UGLY_HYPHENATION:
			if (value) voikko_options.no_ugly_hyphenation = 1;
			else voikko_options.no_ugly_hyphenation = 0;
			return 1;
		case VOIKKO_OPT_OCR_SUGGESTIONS:
			if (value) voikko_options.suggestion_type = ST_OCR;
			else voikko_options.suggestion_type = ST_STD;
			return 1;
		case VOIKKO_OPT_IGNORE_NONWORDS:
			if (value) voikko_options.ignore_nonwords = 1;
			else voikko_options.ignore_nonwords = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS:
			if (value) voikko_options.accept_extra_hyphens = 1;
			else voikko_options.accept_extra_hyphens = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_MISSING_HYPHENS:
			if (value) voikko_options.accept_missing_hyphens = 1;
			else voikko_options.accept_missing_hyphens = 0;
			return 1;
	}
	return 0;
}

int voikko_set_int_option(int handle, int option, int value) {
	switch (option) {
		case VOIKKO_INTERSECT_COMPOUND_LEVEL:
			voikko_options.intersect_compound_level = value;
			return 1;
		case VOIKKO_MIN_HYPHENATED_WORD_LENGTH:
			voikko_options.min_hyphenated_word_length = value;
			return 1;
	}
	return 0;
}

int voikko_set_string_option(int handle, int option, const char * value) {
	switch (option) {
		case VOIKKO_OPT_ENCODING:
			if (!value) return 0;
			#ifdef HAVE_ICONV
			iconv_t toext = iconv_open(value, INTERNAL_CHARSET);
			if (toext == (iconv_t) -1) {
				return 0;
			}
			iconv_t fromext = iconv_open(INTERNAL_CHARSET, value);
			if (fromext == (iconv_t) -1) {
				iconv_close(toext);
				return 0;
			}
			iconv_close(voikko_options.iconv_ucs4_ext);
			voikko_options.iconv_ucs4_ext = toext;
			iconv_close(voikko_options.iconv_ext_ucs4);
			voikko_options.iconv_ext_ucs4 = fromext;
			#endif
			voikko_options.encoding = value;
			return 1;
	}
	return 0;
}

const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	return voikko_init_with_path(handle, langcode, cache_size, 0);
}

const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path) {
	char * project;
	voikko_options.ignore_dot = 0;
	voikko_options.ignore_numbers = 0;
	voikko_options.ignore_uppercase = 0;
	voikko_options.ignore_nonwords = 1;
	voikko_options.accept_first_uppercase = 1;
	voikko_options.accept_all_uppercase = 1;
	voikko_options.no_ugly_hyphenation = 0;
	voikko_options.accept_extra_hyphens = 0;
	voikko_options.accept_missing_hyphens = 0;
	voikko_options.intersect_compound_level = 1;
	voikko_options.min_hyphenated_word_length = 2;
	voikko_options.encoding = "UTF-8";
	voikko_options.cache_size = cache_size;
	voikko_options.suggestion_type = ST_STD;
	init_gc_cache(&voikko_options.gc_cache);
	
	project = malloc(1024);
	if (project == 0) return "Out of memory";
	
	/* FIXME: Temporary hack needed for MT unsafe malaga library */
	if (voikko_handle_count++ > 0) return "Maximum handle count exceeded";
	
	if (!voikko_find_malaga_project(project, 1024, langcode, path)) {
		free(project);
		return "Unsupported language";
	}
	#ifdef HAVE_ICONV
	/* Initialise converters */
	voikko_options.iconv_ucs4_utf8 = iconv_open("UTF-8", INTERNAL_CHARSET);
	if (voikko_options.iconv_ucs4_utf8 == (iconv_t) -1) {
		free(project);
		return "iconv_open(\"UTF-8\", \"" INTERNAL_CHARSET "\") failed";
	}
	voikko_options.iconv_utf8_ucs4 = iconv_open(INTERNAL_CHARSET, "UTF-8");
	if (voikko_options.iconv_utf8_ucs4 == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_ucs4_utf8);
		free(project);
		return "iconv_open(\"" INTERNAL_CHARSET "\", \"UTF-8\") failed";
	}
	voikko_options.iconv_ucs4_ext = iconv_open(voikko_options.encoding, INTERNAL_CHARSET);
	if (voikko_options.iconv_ucs4_ext == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		free(project);
		return "iconv_open(voikko_options.encoding, \"" INTERNAL_CHARSET "\") failed";
	}
	voikko_options.iconv_ext_ucs4 = iconv_open(INTERNAL_CHARSET, voikko_options.encoding);
	if (voikko_options.iconv_ext_ucs4 == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		free(project);
		return "iconv_open(\"" INTERNAL_CHARSET "\", voikko_options.encoding) failed";
	}
	#endif
	
	const char * malaga_init_error = voikko_init_malaga(project);
	free(project);
	if (malaga_init_error) {
		voikko_handle_count--;
		#ifdef HAVE_ICONV
		iconv_close(voikko_options.iconv_ext_ucs4);
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		#endif
		return malaga_init_error;
	}
	if (cache_size >= 0) {
		voikko_options.cache = malloc(6544 * sizeof(wchar_t) << cache_size);
		if (voikko_options.cache) {
			voikko_options.cache_meta = malloc(1008 << cache_size);
			if (voikko_options.cache_meta)
				memset(voikko_options.cache_meta, 0, 1008 << cache_size);
			else {
				free(voikko_options.cache);
				voikko_options.cache = 0;
			}
			memset(voikko_options.cache, 0, 6544 * sizeof(wchar_t) << cache_size);
		}
	}
	else voikko_options.cache = 0;
	*handle = voikko_handle_count;
	return 0;
}

int voikko_terminate(int handle) {
	if (handle == 1 && voikko_handle_count > 0) {
		voikko_handle_count--;
		#ifdef HAVE_ICONV
		iconv_close(voikko_options.iconv_ext_ucs4);
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		#endif
		terminate_libmalaga();
		/*int c = 0;
		for (int i = 0; i < 1*1008; i++) if (voikko_options.cache_meta[i] == '.') c++;
		printf("Cache slots used: %d\n", c);*/
		if (voikko_options.cache) {
			free(voikko_options.cache);
			free(voikko_options.cache_meta);
		}
		gc_clear_cache(handle);
		return 1;
	}
	else return 0;
}

#define VOIKKO_DICTIONARY_FILE "voikko-fi_FI.pro"
#ifdef WIN32
#define VOIKKO_KEY                   "SOFTWARE\\Voikko"
#define VOIKKO_VALUE_DICTIONARY_PATH "DictionaryPath"
#endif // WIN32

int voikko_find_malaga_project(char * buffer, size_t buflen, const char * langcode,
                               const char * path) {
#ifdef HAVE_GETPWUID_R
	struct passwd pwd;
	struct passwd * pwd_result;
#endif // HAVE_GETPWUID_R
#ifdef WIN32
	HKEY hKey;
	DWORD dwBufLen;
	LONG lRet;
#endif // WIN32
	char * path_from_env;
	// Minimum sensible size for the buffer
	if (buflen < 18) return 0;
	char * tmp_buf = malloc(buflen + 2048);
	if (tmp_buf == 0) return 0;
	
	// Clear the buffer.
	memset(buffer, 0x00, buflen);

	if (strcmp(langcode, "fi_FI") == 0) {
		/* Check the user specified dictionary path */
		if (path && strlen(path) < buflen - 18 ) {
			strcpy(buffer, path);
			strcpy(buffer + strlen(path), "/" VOIKKO_DICTIONARY_FILE);
			if (voikko_check_file(buffer)) {
				free(tmp_buf);
				return 1;
			}
		}

		/* Check the path specified by environment variable VOIKKO_DICTIONARY_PATH */
		/* FIXME: thread safety */
		path_from_env = getenv("VOIKKO_DICTIONARY_PATH");
		if (path_from_env && strlen(path_from_env) < buflen - 18) {
			strcpy(buffer, path_from_env);
			strcpy(buffer + strlen(path_from_env), "/" VOIKKO_DICTIONARY_FILE);
			if (voikko_check_file(buffer)) {
				free(tmp_buf);
				return 1;
			}
		}

		#ifdef HAVE_GETPWUID_R
		/* Check for project file in $HOME/.voikko/VOIKKO_DICTIONARY_FILE */
		getpwuid_r(getuid(), &pwd, tmp_buf, buflen + 2048, &pwd_result);
		if (pwd_result && pwd.pw_dir && strlen(pwd.pw_dir) < buflen - 26 ) {
			strcpy(buffer, pwd.pw_dir);
			strcpy(buffer + strlen(pwd.pw_dir), "/.voikko/" VOIKKO_DICTIONARY_FILE);
			if (voikko_check_file(buffer)) {
				free(tmp_buf);
				return 1;
			}
		}
		#endif // HAVE_GETPWUID_R
		#ifdef WIN32
		/* Check the user default dictionary from Windows registry */
		lRet = RegOpenKeyEx(HKEY_CURRENT_USER, VOIKKO_KEY,
		                    0, KEY_QUERY_VALUE, &hKey);
		dwBufLen = buflen - 18;
		if (ERROR_SUCCESS == lRet) {
			lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
			                       (LPBYTE)buffer, &dwBufLen);
			RegCloseKey(hKey);
			if ((ERROR_SUCCESS == lRet)) {
				strcpy(buffer + dwBufLen - 1, "/" VOIKKO_DICTIONARY_FILE);
				if (voikko_check_file(buffer)) {
					free(tmp_buf);
					return 1;
				}
			}
		}

		/* Check the system default dictionary from Windows registry */
		lRet = RegOpenKeyEx(HKEY_LOCAL_MACHINE, VOIKKO_KEY,
			                    0, KEY_QUERY_VALUE, &hKey);
		dwBufLen = buflen - 18;
		if (ERROR_SUCCESS == lRet) {
			lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
			                       (LPBYTE)buffer, &dwBufLen);
			RegCloseKey(hKey);
			if ((ERROR_SUCCESS == lRet)) {
				strcpy(buffer + dwBufLen - 1, "/" VOIKKO_DICTIONARY_FILE);
				if (voikko_check_file(buffer)) {
					free(tmp_buf);
					return 1;
				}
			}
		}
		#endif // WIN32
		/* Fall back to using the compile time default project file */
		strcpy(buffer, DICTIONARY_PATH "/" VOIKKO_DICTIONARY_FILE);
		free(tmp_buf);
		return 1;
	}
	/* Language is not supported */
	free(tmp_buf);
	return 0;
}

const char * voikko_init_malaga(const char * project) {
	init_libmalaga(project);
	if (malaga_error) return malaga_error;
	
	if (strncmp(get_info(), "Voikko-Dictionary-Format: 1\n", 28) == 0) return 0;
	else {
		terminate_libmalaga();
		return "Dictionary file has incompatible version";
	}
}

int voikko_check_file(const char * name) {
#ifdef HAVE_GETPWUID_R
	struct stat sbuf;
	if (stat(name, &sbuf) == 0) return 1;
	else return 0;
#else
 #ifdef WIN32
	HANDLE h;
	h = CreateFileA(name, 0, 0, NULL, OPEN_EXISTING, 0, NULL);
	if (h == INVALID_HANDLE_VALUE) return 0;
	else {
		CloseHandle(h);
		return 1;
	}
 #endif // WIN32
#endif // HAVE_GETPWUID_R
}

void voikko_free_suggest_ucs4(wchar_t ** suggest_result) {
	if (suggest_result) {
		for (wchar_t ** p = suggest_result; *p; p++) free(*p);
		free(suggest_result);
	}
}

void voikko_free_suggest_cstr(char ** suggest_result) {
	if (suggest_result) {
		for (char ** p = suggest_result; *p; p++) free(*p);
		free(suggest_result);
	}
}

void voikko_free_hyphenate(char * hyphenate_result) {
	free(hyphenate_result);
}
