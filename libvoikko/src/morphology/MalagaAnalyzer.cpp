/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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
#include "utils/StringUtils.hpp"
#include "voikko_defines.h"
#include <cwchar>
#include "character/SimpleChar.hpp"
#include <cstdlib>

using namespace std;
using namespace libvoikko::utils;
using namespace libvoikko::morphology::malaga;
using namespace libvoikko::character;

namespace libvoikko { namespace morphology {

MalagaAnalyzer::MalagaAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	malaga::init_libmalaga(directoryName.c_str(), &malagaState);
	initSymbols();
}

list<Analysis *> * MalagaAnalyzer::analyze(const wchar_t * word) {
	return analyze(word, wcslen(word));
}

list<Analysis *> * MalagaAnalyzer::analyze(const wchar_t * word,
                                           size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	list<Analysis *> * result = analyze(wordUtf8);
	delete[] wordUtf8;
	return result;
}

list<Analysis *> * MalagaAnalyzer::analyze(const char * word) {
	if (strlen(word) > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();
	try {
		analyse_item(word, &malagaState);
		value_t res = first_analysis_result(&malagaState);
		int currentAnalysisCount = 0;
		while (res && currentAnalysisCount < LIBVOIKKO_MAX_ANALYSIS_COUNT) {
			Analysis * analysis = new Analysis();
			parseStructure(analysis, res);
			parseBasicAttribute(analysis, res, symbols[MS_SIJAMUOTO], "SIJAMUOTO");
			parseBasicAttribute(analysis, res, symbols[MS_CLASS], "CLASS");
			parseBasicAttribute(analysis, res, symbols[MS_NUMBER], "NUMBER");
			parseBasicAttribute(analysis, res, symbols[MS_PERSON], "PERSON");
			parseBasicAttribute(analysis, res, symbols[MS_MOOD], "MOOD");
			parseBasicAttribute(analysis, res, symbols[MS_VAPAA_JALKIOSA], "MALAGA_VAPAA_JALKIOSA");
			parseBasicAttribute(analysis, res, symbols[MS_NEGATIVE], "NEGATIVE");
			parsePerusmuoto(analysis, res);
			analysisList->push_back(analysis);
			res = next_analysis_result(&malagaState);
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
	terminate_libmalaga(&malagaState);
}

symbol_t MalagaAnalyzer::findSymbol(const char * name) {
	value_t symbolValue;
	try {
		symbolValue = parse_malaga_symbol(name, &malagaState);
	} catch (setup::DictionaryException e) {
		return 0;
	}
	symbol_t symbol;
	try {
		symbol = value_to_symbol(symbolValue);
	} catch (setup::DictionaryException e) {
		symbol = 0;
	}
	free(symbolValue);
	return symbol;
}

void MalagaAnalyzer::insertToSymbolMap(map<symbol_t, const wchar_t *> &map,
                              const char * malagaName,
                              const wchar_t * externalName) {
	map.insert(std::make_pair(findSymbol(malagaName), externalName));
}

void MalagaAnalyzer::initSymbols() {
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
			case MS_NUMBER:
				symbolName = "luku";
				break;
			case MS_PERSON:
				symbolName = "tekij\xc3\xa4";
				break;
			case MS_MOOD:
				symbolName = "tapaluokka";
				break;
			case MS_VAPAA_JALKIOSA:
				symbolName = "vapaa_j\xc3\xa4lkiosa";
				break;
			case MS_NEGATIVE:
				symbolName = "kielto";
				break;
		}
		symbols[sym] = findSymbol(symbolName);
	}
	
	symbolMap.clear();
	insertToSymbolMap(symbolMap, "niment\xc3\xb6", L"nimento");
	insertToSymbolMap(symbolMap, "omanto", L"omanto");
	insertToSymbolMap(symbolMap, "osanto", L"osanto");
	insertToSymbolMap(symbolMap, "olento", L"olento");
	insertToSymbolMap(symbolMap, "tulento", L"tulento");
	insertToSymbolMap(symbolMap, "kohdanto", L"kohdanto");
	insertToSymbolMap(symbolMap, "sis\xc3\xa4olento", L"sisaolento");
	insertToSymbolMap(symbolMap, "sis\xc3\xa4""eronto", L"sisaeronto");
	insertToSymbolMap(symbolMap, "sis\xc3\xa4tulento", L"sisatulento");
	insertToSymbolMap(symbolMap, "ulko_olento", L"ulkoolento");
	insertToSymbolMap(symbolMap, "ulkoeronto", L"ulkoeronto");
	insertToSymbolMap(symbolMap, "ulkotulento", L"ulkotulento");
	insertToSymbolMap(symbolMap, "vajanto", L"vajanto");
	insertToSymbolMap(symbolMap, "seuranto", L"seuranto");
	insertToSymbolMap(symbolMap, "keinonto", L"keinonto");
	insertToSymbolMap(symbolMap, "kerronto_sti", L"kerrontosti");
	
	insertToSymbolMap(symbolMap, "nimisana", L"nimisana");
	insertToSymbolMap(symbolMap, "laatusana", L"laatusana");
	insertToSymbolMap(symbolMap, "nimi_laatusana", L"nimisana_laatusana");
	insertToSymbolMap(symbolMap, "teonsana", L"teonsana");
	insertToSymbolMap(symbolMap, "seikkasana", L"seikkasana");
	insertToSymbolMap(symbolMap, "asemosana", L"asemosana");
	insertToSymbolMap(symbolMap, "suhdesana", L"suhdesana");
	insertToSymbolMap(symbolMap, "huudahdussana", L"huudahdussana");
	insertToSymbolMap(symbolMap, "sidesana", L"sidesana");
	insertToSymbolMap(symbolMap, "etunimi", L"etunimi");
	insertToSymbolMap(symbolMap, "sukunimi", L"sukunimi");
	insertToSymbolMap(symbolMap, "paikannimi", L"paikannimi");
	insertToSymbolMap(symbolMap, "nimi", L"nimi");
	insertToSymbolMap(symbolMap, "kieltosana", L"kieltosana");
	insertToSymbolMap(symbolMap, "lyhenne", L"lyhenne");
	insertToSymbolMap(symbolMap, "lukusana", L"lukusana");
	
	insertToSymbolMap(symbolMap, "yksikk\xc3\xb6", L"singular");
	insertToSymbolMap(symbolMap, "monikko", L"plural");
	
	insertToSymbolMap(symbolMap, "tekij\xc3\xa4_1", L"1");
	insertToSymbolMap(symbolMap, "tekij\xc3\xa4_2", L"2");
	insertToSymbolMap(symbolMap, "tekij\xc3\xa4_3", L"3");
	insertToSymbolMap(symbolMap, "tekij\xc3\xa4_4", L"4");
	
	insertToSymbolMap(symbolMap, "tositapa", L"indicative");
	insertToSymbolMap(symbolMap, "k\xc3\xa4skytapa", L"imperative");
	insertToSymbolMap(symbolMap, "ehtotapa", L"conditional");
	insertToSymbolMap(symbolMap, "mahtotapa", L"potential");
	
	insertToSymbolMap(symbolMap, "yes", L"true");
	insertToSymbolMap(symbolMap, "no", L"false");
	insertToSymbolMap(symbolMap, "molemmat", L"both");
}

void MalagaAnalyzer::parseStructure(Analysis * &analysis, value_t &result) const {
	value_t structureVal = get_attribute(result, symbols[MS_RAKENNE]);
	char * value = get_value_string(structureVal);
	wchar_t * structure = StringUtils::ucs4FromUtf8(value);
	analysis->addAttribute("STRUCTURE", structure);
	free(value);
}

void MalagaAnalyzer::parseBasicAttribute(Analysis * &analysis, value_t &result,
                                         symbol_t symbol, const char * attrName) const {
	if (!symbol) {
		return;
	}
	value_t value = get_attribute(result, symbol);
	if (!value) {
		return;
	}
	symbol_t valueSym;
	try {
		valueSym = value_to_symbol(value);
	} catch (setup::DictionaryException e) {
		return;
	}
	map<symbol_t, const wchar_t *>::const_iterator mapIterator = symbolMap.find(valueSym);
	if (mapIterator == symbolMap.end()) {
		return;
	}
	const wchar_t * valueName = (*mapIterator).second;
	if (valueName) {
		analysis->addAttribute(attrName, StringUtils::copy(valueName));
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
	const wchar_t * structure = analysis->getValue("STRUCTURE");
	wchar_t * baseForm = parseBaseform(perusmuoto, structure);
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

static void fixCapitalisation(wchar_t * baseForm, const wchar_t * structure) {
	size_t basePos = 0;
	size_t structPos = 0;
	while (baseForm[basePos] != L'\0' && structure[structPos] != L'\0') {
		if (structure[structPos] == L'=') {
			++structPos;
			continue;
		}
		if (structure[structPos] == L'i' || structure[structPos] == L'j') {
			baseForm[basePos] = SimpleChar::upper(baseForm[basePos]);
		}
		++structPos;
		++basePos;
	}
}

wchar_t * MalagaAnalyzer::parseBaseform(wchar_t * &perusmuoto, const wchar_t * structure) const {
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
	fixCapitalisation(baseForm, structure);
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
