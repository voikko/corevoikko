/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include "morphology/MalagaAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defs.h"
#include <wchar.h>
#include <cstdlib>

using namespace std;
using namespace libvoikko::utils;
using namespace libvoikko::morphology::malaga;

namespace libvoikko { namespace morphology {

bool MalagaAnalyzer::symbolsInited = false;
symbol_t MalagaAnalyzer::symbols[] = {0,0,0};
map<symbol_t, const wchar_t *> MalagaAnalyzer::sijamuotoMap;
map<symbol_t, const wchar_t *> MalagaAnalyzer::classMap;

list<Analysis *> * MalagaAnalyzer::analyze(const wchar_t * word) const {
	return analyze(word, wcslen(word));
}

list<Analysis *> * MalagaAnalyzer::analyze(const wchar_t * word,
                                           size_t wlen) const {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	list<Analysis *> * result = analyze(wordUtf8);
	delete[] wordUtf8;
	return result;
}

list<Analysis *> * MalagaAnalyzer::analyze(const char * word) const {
	if (strlen(word) > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();
	try {
		initSymbols();
		analyse_item(word);
		value_t res = first_analysis_result();
		int currentAnalysisCount = 0;
		while (res && currentAnalysisCount < LIBVOIKKO_MAX_ANALYSIS_COUNT) {
			Analysis * analysis = new Analysis();
			parseStructure(analysis, res);
			parseSijamuoto(analysis, res);
			parseClass(analysis, res);
			parsePerusmuoto(analysis, res);
			analysisList->push_back(analysis);
			res = next_analysis_result();
			++currentAnalysisCount;
		}
	}
	catch (setup::DictionaryException e) {
		// Something went wrong during analysis, just return the
		// (probably empty) analysis list.
	}
	return analysisList;
}

void MalagaAnalyzer::terminate() {
	terminate_libmalaga();
}

static symbol_t findSymbol(const char * name) {
	value_t symbolValue = parse_malaga_symbol(name);
	symbol_t symbol = value_to_symbol(symbolValue);
	free(symbolValue);
	return symbol;
}

static void insertToSymbolMap(map<symbol_t, const wchar_t *> &map,
                              const char * malagaName,
                              const wchar_t * externalName) {
	map.insert(std::make_pair(findSymbol(malagaName), externalName));
}

void MalagaAnalyzer::initSymbols() {
	if (symbolsInited) {
		return;
	}
	for (size_t sym = MS_RAKENNE; sym < MS_LAST_SYMBOL; sym++) {
		const char * symbolName = 0;
		switch (sym) {
			case MS_RAKENNE:
				symbolName = "rakenne";
				break;
			case MS_SIJAMUOTO:
				symbolName = "sijamuoto";
				break;
			case MS_CLASS:
				symbolName = "luokka";
				break;
			case MS_PERUSMUOTO:
				symbolName = "perusmuoto";
				break;
		}
		symbols[sym] = findSymbol(symbolName);
	}
	
	sijamuotoMap.clear();
	insertToSymbolMap(sijamuotoMap, "nil", L"none");
	insertToSymbolMap(sijamuotoMap, "niment\xc3\xb6", L"nimento");
	insertToSymbolMap(sijamuotoMap, "omanto", L"omanto");
	insertToSymbolMap(sijamuotoMap, "osanto", L"osanto");
	insertToSymbolMap(sijamuotoMap, "olento", L"olento");
	insertToSymbolMap(sijamuotoMap, "tulento", L"tulento");
	insertToSymbolMap(sijamuotoMap, "kohdanto", L"kohdanto");
	insertToSymbolMap(sijamuotoMap, "sis\xc3\xa4olento", L"sisaolento");
	insertToSymbolMap(sijamuotoMap, "sis\xc3\xa4""eronto", L"sisaeronto");
	insertToSymbolMap(sijamuotoMap, "sis\xc3\xa4tulento", L"sisatulento");
	insertToSymbolMap(sijamuotoMap, "ulko_olento", L"ulkoolento");
	insertToSymbolMap(sijamuotoMap, "ulkoeronto", L"ulkoeronto");
	insertToSymbolMap(sijamuotoMap, "ulkotulento", L"ulkotulento");
	insertToSymbolMap(sijamuotoMap, "vajanto", L"vajanto");
	insertToSymbolMap(sijamuotoMap, "seuranto", L"seuranto");
	insertToSymbolMap(sijamuotoMap, "keinonto", L"keinonto");
	insertToSymbolMap(sijamuotoMap, "kerronto_sti", L"kerrontosti");
	
	classMap.clear();
	insertToSymbolMap(classMap, "nimisana", L"nimisana");
	insertToSymbolMap(classMap, "laatusana", L"laatusana");
	insertToSymbolMap(classMap, "nimi_laatusana", L"nimisana_laatusana");
	insertToSymbolMap(classMap, "teonsana", L"teonsana");
	insertToSymbolMap(classMap, "seikkasana", L"seikkasana");
	insertToSymbolMap(classMap, "asemosana", L"asemosana");
	insertToSymbolMap(classMap, "suhdesana", L"suhdesana");
	insertToSymbolMap(classMap, "huudahdussana", L"huudahdussana");
	insertToSymbolMap(classMap, "sidesana", L"sidesana");
	insertToSymbolMap(classMap, "etunimi", L"etunimi");
	insertToSymbolMap(classMap, "sukunimi", L"sukunimi");
	insertToSymbolMap(classMap, "paikannimi", L"paikannimi");
	insertToSymbolMap(classMap, "nimi", L"nimi");
	insertToSymbolMap(classMap, "kieltosana", L"kieltosana");
	insertToSymbolMap(classMap, "lyhenne", L"lyhenne");
	insertToSymbolMap(classMap, "lukusana", L"lukusana");
	
	symbolsInited = true;
}

void MalagaAnalyzer::parseStructure(Analysis * &analysis, value_t &result) const {
	value_t structureVal = get_attribute(result, symbols[MS_RAKENNE]);
	char * value = get_value_string(structureVal);
	wchar_t * structure = StringUtils::ucs4FromUtf8(value);
	analysis->addAttribute("STRUCTURE", structure);
	free(value);
}

void MalagaAnalyzer::parseSijamuoto(Analysis * &analysis, value_t &result) const {
	value_t sijamuotoVal = get_attribute(result, symbols[MS_SIJAMUOTO]);
	symbol_t sijamuoto = value_to_symbol(sijamuotoVal);
	map<symbol_t, const wchar_t *>::iterator mapIterator = sijamuotoMap.find(sijamuoto);
	if (mapIterator == sijamuotoMap.end()) {
		return;
	}
	const wchar_t * sijamuotoName = (*mapIterator).second;
	if (sijamuotoName) {
		analysis->addAttribute("SIJAMUOTO", StringUtils::copy(sijamuotoName));
	}
}

void MalagaAnalyzer::parseClass(Analysis * &analysis, value_t &result) const {
	value_t classVal = get_attribute(result, symbols[MS_CLASS]);
	symbol_t classSym = value_to_symbol(classVal);
	map<symbol_t, const wchar_t *>::iterator mapIterator = classMap.find(classSym);
	if (mapIterator == classMap.end()) {
		return;
	}
	const wchar_t * className = (*mapIterator).second;
	if (className) {
		analysis->addAttribute("CLASS", StringUtils::copy(className));
	}
}

void MalagaAnalyzer::parsePerusmuoto(Analysis * &analysis, value_t &result) const {
	value_t perusmuotoVal = get_attribute(result, symbols[MS_PERUSMUOTO]);
	if (!perusmuotoVal) {
		return;
	}
	char * value = get_value_string(perusmuotoVal);
	wchar_t * perusmuoto = StringUtils::ucs4FromUtf8(value);
	free(value);
	wchar_t * baseForm = parseBaseform(perusmuoto);
	wchar_t * wordIds = parseAttributeFromPerusmuoto(perusmuoto, L's');
	wchar_t * wordBases = parseAttributeFromPerusmuoto(perusmuoto, L'p');
	delete[] perusmuoto;
	if (baseForm) {
		analysis->addAttribute("BASEFORM", baseForm);
	}
	if (wordIds) {
		analysis->addAttribute("WORDIDS", wordIds);
	}
	if (wordBases) {
		analysis->addAttribute("WORDBASES", wordBases);
	}
}

static void passAttribute(wchar_t * &perusmuoto, size_t &index) {
	while (perusmuoto[index] != L')' && perusmuoto[index] != L'\0') {
		++index;
	}
	++index;
}

wchar_t * MalagaAnalyzer::parseBaseform(wchar_t * &perusmuoto) const {
	size_t lenPerusmuoto = wcslen(perusmuoto);
	wchar_t * baseForm = new wchar_t[lenPerusmuoto + 1];
	size_t posBaseForm = 0;
	for (size_t i = 0; i < lenPerusmuoto;) {
		if (perusmuoto[i] == L'+') {
			++i;
			while (i < lenPerusmuoto && perusmuoto[i] != L'+' && perusmuoto[i] != L'(') {
				baseForm[posBaseForm++] = perusmuoto[i++];
			}
			while (perusmuoto[i] == L'(') {
				passAttribute(perusmuoto, i);
			}
		}
		else {
			// Something is wrong with perusmuoto, do not return a base form
			delete[] baseForm;
			return 0;
		}
	}
	baseForm[posBaseForm] = L'\0';
	return baseForm;
}

wchar_t * MalagaAnalyzer::parseAttributeFromPerusmuoto(wchar_t * &perusmuoto, wchar_t id) const {
	size_t lenPerusmuoto = wcslen(perusmuoto);
	wchar_t * attribute = new wchar_t[lenPerusmuoto + 1];
	size_t posAttribute = 0;
	bool foundAttribute = false;
	for (size_t i = 0; i < lenPerusmuoto;) {
		if (perusmuoto[i] == L'+') {
			attribute[posAttribute++] = perusmuoto[i++]; // '+'
			while (i < lenPerusmuoto && perusmuoto[i] != L'+' && perusmuoto[i] != L'(') {
				attribute[posAttribute++] = perusmuoto[i++];
			}
			while (perusmuoto[i] == L'(') {
				if (perusmuoto[++i] == id) {
					foundAttribute = true;
					attribute[posAttribute++] = L'(';
					i += 2; // pass attribute id and '='
					while (i < lenPerusmuoto && perusmuoto[i] != L')') {
						attribute[posAttribute++] = perusmuoto[i++];
					}
					attribute[posAttribute++] = perusmuoto[i++]; // ')'
				}
				else {
					passAttribute(perusmuoto, i);
				}
			}
		}
		else {
			// Something is wrong with perusmuoto, do not return attribute
			delete[] attribute;
			return 0;
		}
	}
	if (foundAttribute) {
		attribute[posAttribute] = L'\0';
		return attribute;
	}
	else {
		delete[] attribute;
		return 0;
	}
}

} }
