#!/usr/bin/make -f

# Komennolla "make" luodaan asennuspaketti vimchant.vba (ja .vba.gz), joka
# sisältää lisäosan tarvitsemat tiedostot. Lisäosapaketti asennetaan lataamalla
# se ensin Vim-editoriin, esimerkiksi komentotulkista:
#
#     $ vim vimchant.vba.gz
#
# Sen jälkeen suoritetaan Vimistä komento
#
#     :so %
#
# joka asentaa tiedostot oikeisiin hakemistoihin ja päivittää help-tagit.
# Lisäosa poistetaan Vimin komennolla
#
#     :RmVimball vimchant
#

NAME := vimchant
FILES := */$(NAME).*

$(NAME).vba.gz: $(NAME).vba
	gzip -9 --stdout $^ >$@

$(NAME).vba: $(FILES)
	printf "%s\n" $^ | vim \
		-c 'let g:vimball_home="."' \
		-c 'silent! 1,$$MkVimball! $(NAME)' \
		-c 'qa!' -

clean:
	rm -f *.vba *.vba.gz

.PHONY: clean
