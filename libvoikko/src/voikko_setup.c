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
	}
	return 0;
}

int voikko_set_int_option(int handle, int option, int value) {
	switch (option) {
		case VOIKKO_INTERSECT_COMPOUND_LEVEL:
			voikko_options.intersect_compound_level = value;
			return 1;
	}
	return 0;
}

int voikko_set_string_option(int handle, int option, const char * value) {
	switch (option) {
		case VOIKKO_OPT_ENCODING:
			if (!value) return 0;
			iconv_t toext = iconv_open(value, "WCHAR_T");
			if (toext == (iconv_t) -1) {
				return 0;
			}
			iconv_t fromext = iconv_open("WCHAR_T", value);
			if (fromext == (iconv_t) -1) {
				iconv_close(toext);
				return 0;
			}
			iconv_close(voikko_options.iconv_ucs4_ext);
			voikko_options.iconv_ucs4_ext = toext;
			iconv_close(voikko_options.iconv_ext_ucs4);
			voikko_options.iconv_ext_ucs4 = fromext;
			voikko_options.encoding = value;
			return 1;
	}
	return 0;
}

const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	char * project;
	voikko_options.ignore_dot = 0;
	voikko_options.ignore_numbers = 0;
	voikko_options.ignore_uppercase = 0;
	voikko_options.accept_first_uppercase = 1;
	voikko_options.accept_all_uppercase = 1;
	voikko_options.no_ugly_hyphenation = 0;
	voikko_options.intersect_compound_level = 1;
	voikko_options.encoding = "UTF-8";
	
	project = malloc(1024);
	if (project == 0) return "Out of memory";
	
	/* FIXME: Temporary hack needed for MT unsafe malaga library */
	if (voikko_handle_count++ > 0) return "Maximum handle count exceeded";
	
	if (!voikko_find_malaga_project(project, 1024, langcode)) {
		free(project);
		return "Unsupported language";
	}
	/* Initialise converters */
	voikko_options.iconv_ucs4_utf8 = iconv_open("UTF-8", "WCHAR_T");
	if (voikko_options.iconv_ucs4_utf8 == (iconv_t) -1) {
		free(project);
		return "iconv_open(\"UTF-8\", \"WCHAR_T\") failed";
	}
	voikko_options.iconv_utf8_ucs4 = iconv_open("WCHAR_T", "UTF-8");
	if (voikko_options.iconv_utf8_ucs4 == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_ucs4_utf8);
		free(project);
		return "iconv_open(\"WCHAR_T\", \"UTF-8\") failed";
	}
	voikko_options.iconv_ucs4_ext = iconv_open(voikko_options.encoding, "WCHAR_T");
	if (voikko_options.iconv_ucs4_ext == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		free(project);
		return "iconv_open(voikko_options.encoding, \"WCHAR_T\") failed";
	}
	voikko_options.iconv_ext_ucs4 = iconv_open("WCHAR_T", voikko_options.encoding);
	if (voikko_options.iconv_ext_ucs4 == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		free(project);
		return "iconv_open(\"WCHAR_T\", voikko_options.encoding) failed";
	}
	
	init_libmalaga(project);
	free(project);
	if (malaga_error) {
		voikko_handle_count--;
		return malaga_error;
	}
	voikko_options.cache = malloc(1*6544 * sizeof(wchar_t));
	if (voikko_options.cache) {
		voikko_options.cache_meta = malloc(1*1008);
		if (voikko_options.cache_meta) memset(voikko_options.cache_meta, 0, 1*1008);
		else {
			free(voikko_options.cache);
			voikko_options.cache = 0;
		}
		memset(voikko_options.cache, 0, 1*6544 * sizeof(wchar_t));
	}
	*handle = voikko_handle_count;
	return 0;
}

int voikko_terminate(int handle) {
	if (handle == 1 && voikko_handle_count > 0) {
		voikko_handle_count--;
		iconv_close(voikko_options.iconv_ext_ucs4);
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		terminate_libmalaga();
		/*int c = 0;
		for (int i = 0; i < 1*1008; i++) if (voikko_options.cache_meta[i] == '.') c++;
		printf("Cache slots used: %d\n", c);*/
		if (voikko_options.cache) {
			free(voikko_options.cache);
			free(voikko_options.cache_meta);
		}
		return 1;
	}
	else return 0;
}

#define VOIKKO_DICTIONARY_FILE "suomi.pro"
#ifdef WIN32
#define VOIKKO_KEY                   "SOFTWARE\\Voikko"
#define VOIKKO_VALUE_DICTIONARY_PATH "DictionaryPath"
#endif // WIN32

int voikko_find_malaga_project(char * buffer, size_t buflen, const char * langcode) {
#ifdef HAVE_GETPWUID_R
	struct passwd pwd;
	struct stat sbuf;
	struct passwd * pwd_result;
#endif // HAVE_GETPWUID_R
	char * tmp_buf = malloc(buflen + 2048);
	if (tmp_buf == 0) return 0;

	// Clear the buffer.
	memset(buffer, 0x00, buflen);

	if (strcmp(langcode, "fi_FI") == 0) {
#ifdef WIN32
		/* Check the Windows registry */
		HKEY hKey;
		DWORD dwBufLen=buflen;
		LONG lRet;

		lRet = RegOpenKeyEx(HKEY_CURRENT_USER, VOIKKO_KEY,
		                    0, KEY_QUERY_VALUE, &hKey);

		if (ERROR_SUCCESS != lRet) {
			// Check from local machine
			lRet = RegOpenKeyEx(HKEY_LOCAL_MACHINE, VOIKKO_KEY,
			                    0, KEY_QUERY_VALUE, &hKey);
		}

		if (ERROR_SUCCESS == lRet) {
			lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
			                       (LPBYTE)buffer, &dwBufLen);

			RegCloseKey(hKey);

			if ((ERROR_SUCCESS == lRet) && (dwBufLen <= buflen)) {
				if (strlen(buffer) > 0) {
					free(tmp_buf);
					return 1;
				}
			}
		}

		free(tmp_buf);
		return 0;
#endif
#ifdef HAVE_GETPWUID_R
		/* Check for project file in $HOME/.voikko/suomi.pro */
		getpwuid_r(getuid(), &pwd, tmp_buf, buflen + 2048, &pwd_result);
		if (pwd_result && pwd.pw_dir && strlen(pwd.pw_dir) < buflen - 19 ) {
			strcpy(buffer, pwd.pw_dir);
			strcpy(buffer + strlen(pwd.pw_dir), "/.voikko/suomi.pro");
			if (stat(buffer, &sbuf) == 0) {
				free(tmp_buf);
				return 1;
			}
		}
#endif
		/* Use the compile time default project file */
		strcpy(buffer, DICTIONARY_PATH "/" VOIKKO_DICTIONARY_FILE);
		free(tmp_buf);
		return 1;
	}
	/* Language is not supported */
	free(tmp_buf);
	return 0;
}

