# -*- coding: utf-8 -*-

# Copyright 2005, 2006 Harri Pitkänen (hatapitk@cc.jyu.fi)
# Functions and data for Hunspell-fi <-> Suomi-malaga converter

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import re

grads = [ (u'sw', u'tt', u'av1'),
	(u'sw', u'pp', u'av1'),
	(u'sw', u'kk', u'av1'),
	(u'sw', u'mp', u'av1'),
	(u'sw', u'p', u'av1'),
	(u'sw', u'nt', u'av1'),
	(u'sw', u'lt', u'av1'),
	(u'sw', u'rt', u'av1'),
	(u'sw', u't', u'av1'),
	(u'sw', u'nk', u'av1'),
	(u'sw', u'uku', u'av1'),
	(u'sw', u'yky', u'av1'),
	(u'ws', u't', u'av2'),
	(u'ws', u'p', u'av2'),
	(u'ws', u'k', u'av2'),
	(u'ws', u'mm', u'av2'),
	(u'ws', u'v', u'av2'),
	(u'ws', u'nn', u'av2'),
	(u'ws', u'll', u'av2'),
	(u'ws', u'rr', u'av2'),
	(u'ws', u'd', u'av2'),
	(u'ws', u'ng', u'av2'),
	(u'sw', u'k>j', u'av3'),
	(u'ws', u'j>k', u'av4'),
	(u'sw', u'k>', u'av5'),
	(u'ws', u'>k', u'av6')   ]

classmap = [(u'valo', u'sw', [(None,u'(.*)',u'valo'),
			(u'k>',u'(ko)ko',u'koko'),
			(u'k>',u'(ruo)ko',u'ruoko'),
			(u'k>',u'(.*)kU',u'alku'),
			(u'uku',u'(.U)kU',u'luku'),
			(u'uku',u'(..U)kU',u'tiuku'),
			(u'lt',u'(.*l)tO',u'aalto'),
			(u'nt',u'(.*n)tO',u'anto'),
			(u'nt',u'(.*n)tU',u'lintu'),
			(u'nk',u'(.*n)kO',u'hanko'),
			(u'tt',u'(.*t)tU',u'hattu'),
			(u'tt',u'(.*t)tO',u'liitto'),
			(u'nk',u'(.*n)kU',u'hinku'),
			(u'pp',u'(.*p)pU',u'hoppu'),
			(u'rt',u'(.*r)tO',u'kaarto'),
			(u'pp',u'(.*p)pO',u'kippo'),
			(u'mp',u'(.*m)pO',u'sampo'),
			(u'mp',u'(.*m)pU',u'kumpu'),
			(u't',u'(.*)tU',u'laatu'),
			(u'p',u'(.*)pU',u'apu'),
			(u'p',u'(.*)pO',u'lepo'),
			(u't',u'(.*)tO',u'leuto'),
			(u'kk',u'(.*k)kO',u'verkko'),
			(u'k>',u'(.*h)kO',u'vihko'),
			(u'k>',u'(.*)kO',u'verkko')   ]),
	(u'arvelu', u'sw', [(None,u'(.*e[lr])O',u'hontelo'),
			(None,u'(.*)',u'arvelu'),
			(u'nk',u'(.*n)kO',u'alanko'),
			(u'nt',u'(.*n)tO',u'avanto'),
			(u'kk',u'(.*k)kO',u'laatikko'),
			(u'tt',u'(.*t)tO',u'pihatto'),
			(u'tt',u'(.*t)tU',u'raamattu') ]),
	(u'autio', u'-', [(None,u'(.*)',u'autio')]),
	(u'kiiski', u'-', [(None,u'(.*)i',u'kiiski')]),
	(u'risti', u'sw', [(None,u'(.*)i',u'risti'),
			(u'pp',u'(.*p)pi',u'keppi'),
			(u'lt',u'(.*l)ti',u'pelti'),
			(u'nk',u'(.*n)ki',u'renki'),
			(u'kk',u'(.*k)ki',u'takki'),
			(u'tt',u'(.*t)ti',u'tatti'),
			(u'nt',u'(.*n)ti',u'tunti'),
			(u'p',u'(.*)pi',u'hupi'),
			(u't',u'(.*)ti',u'vati') ]),
	(u'paperi', u'sw', [(None,u'(.*)i',u'paperi'),
			(None,u'(.*)i',u'banaali'),
			(None,u'(.*)',u'edam'),
			(u'nt',u'(.*n)ti',u'hollanti'),
			(u'nk',u'(.*n)ki',u'killinki'),
			(u'kk',u'(.*k)ki',u'kajakki'),
			(u'tt',u'(.*t)ti',u'salaatti'),
			(u'pp',u'(.*p)pi',u'sinappi'),
			(u't',u'(.*)ti',u'konvehti') ]),
	(u'kalsium', u'sw', [(None,u'(.*)i',u'fan'),
			(None,u'(.*)',u'kalsium'),
			(u'pp',u'(.*p)pi',u'pop'),
			(u'kk',u'(.*k)ki',u'punk')]),
	(u'lovi', u'sw',   [(None,u'(.*)i',u'lovi'),
			(u'nk',u'(.*n)ki',u'hanki'),
			(u'pp',u'(.*p)pi',u'happi'),
			(u'mp',u'(.*m)pi',u'sampi'),
			(u'kk',u'(.*k)ki',u'kaikki'),
			(u'k>j',u'(.*)ki',u'kylki'),
			(u'p',u'(.*n)pi',u'siipi'),
			(u't',u'(.*n)pi',u'lehti'),
			(u'k',u'(.*)ki',u'kaikki') ])
	 ]



def match_re(string, pattern):
	pattern = pattern.replace(u'V', u'(?:a|e|i|o|u|y|ä|ö)')
	pattern = pattern.replace(u'C', u'(?:b|c|d|f|g|h|j|k|l|m|n|p|q|r|s|t|v|w|x|y|z|š)')
	pattern = pattern.replace(u'A', u'(?:a|ä)')
	pattern = pattern.replace(u'O', u'(?:o|ö)')
	pattern = pattern.replace(u'U', u'(?:u|y)')
	match = re.compile(u'^' + pattern + u'$').match(string)
	if match == None: return None
	else: return match.group(1)
