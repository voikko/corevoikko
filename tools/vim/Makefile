# Komennolla "make" luodaan asennuspaketti vimchant.vmb (ja .vmb.gz),
# joka sisältää lisäosan tarvitsemat tiedostot. Lisäosapaketti
# asennetaan lataamalla se ensin Vim-editoriin, esimerkiksi
# komentotulkista:
#
#     $ vim vimchant.vmb.gz
#
# Sen jälkeen suoritetaan Vimistä komento
#
#     :so %
#
# joka asentaa tiedostot oikeisiin hakemistoihin ja päivittää
# help-tagit. Lisäosa poistetaan Vimin komennolla
#
#     :RmVimball vimchant
#

NAME := vimchant
FILES := */$(NAME).*

$(NAME).vmb.gz: $(NAME).vmb
	gzip -9 --stdout $^ >$@

$(NAME).vmb: $(FILES)
	printf "%s\n" $^ | vim \
		-c 'let g:vimball_home="."' \
		-c 'silent! 1,$$MkVimball! $(NAME)' \
		-c 'qa!' -

clean:
	rm -f *.vmb *.vmb.gz

.PHONY: clean
