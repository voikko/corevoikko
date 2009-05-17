;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wcheck-mode.el


;; Copyright (C) 2009 Teemu Likonen <tlikonen@iki.fi>
;;
;; LICENSE
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.


;;; Muuttujat ja asetukset


(defgroup wcheck nil
  "Interface for external text-filtering programs."
  :group 'applications)


(defcustom wcheck-language-data nil
  "Wcheck-mode language configuration.
Elements of this alist are of the form:

  (LANGUAGE (KEY . VALUE) [(KEY . VALUE) ...])

LANGUAGE is a name string for a language and KEY and VALUE pairs
denote settings for the language. Here is a list of possible KEYs
and a description of VALUE types:

  * `program': VALUE denotes the executable program that is
    responsible for spell-checking this language. This setting is
    mandatory.

  * `args': Optional command-line arguments for the program.

  * `syntax': VALUE is a symbol referring to an Emacs syntax
    table. See the Info node `(elisp)Syntax Tables' for more
    information. The default value is `text-mode-syntax-table'.

  * `face': A symbol referring to a face which is used to mark
    text with this LANGUAGE. The default value is
    `wcheck-default-face'.

  * `regexp-start', `regexp-body', `regexp-end': Regular
    expression strings which match the start of a string body,
    characters within the body and the end of the body,
    respectively.

    This is how they are used in practice: Wcheck mode looks for
    text that matches the construct `regexp-start + regexp-body +
    regexp-end'. The text that matches `regexp-body' is sent to
    an external program to analyze. When strings return from the
    external program they are marked in Emacs buffer using the
    following construction: `regexp-start + (regexp-quote STRING)
    + regexp-end'.

    Do not use grouping constructs `\\( ... \\)' in the regular
    expressions because the back reference `\\1' is used for
    separating the body string from the start and end match. You
    can use \"shy\" groups `\\(?: ... \\)' which do not record
    the matched substring.

    The default values for the regular expressions are:

        \\=\\<'*         (regexp-start)
        \\w+?         (regexp-body)
        '*\\=\\>         (regexp-end)

    Effectively they match word characters defined in the syntax
    table. Single quotes (') at the start and end of a word are
    excluded. This is probably a good thing when using Wcheck
    mode as a spelling checker.

  * `regexp-discard': The string that matched `regexp-body' is
    then matched against the value of this option. If this
    regular expression matches, then the word is discarded and
    won't be sent to the external program. You can use this to
    define exceptions to the previous regexp rules. The default
    value is

        \\`'+\\'

    which discards the body string if it consists only of single
    quotes. This was chosen as the default because the standard
    syntax table `text-mode-syntax-table' defines single quote as
    a word character. It's probably not useful to mark separate
    single quotes in a buffer when Wcheck mode is used as a
    spelling checker. If you don't want to have any discarding
    rules set this to empty string.

An example contents of the `wcheck-language-data' variable:

    ((\"suomi\"
      (program . \"/usr/bin/enchant\")
      (args . \"-l -d fi_FI\"))
      (syntax . my-finnish-syntax-table)
     (\"British English\"
      (program . \"/usr/bin/ispell\")
      (args . \"-l -d british\")
     (\"Trailing whitespace\"
      (program . \"/bin/cat\")
      (regexp-start . \"\")
      (regexp-body . \"\\\\s-+\")
      (regexp-end . \"$\")
      (regexp-discard . \"\"))))"

  :group 'wcheck
  :type '(alist :key-type (string :tag "Language")
                :value-type
                (cons :format "%v"
                      (cons :format "%v"
                            (const :tag "Program: "
                                   :format "%t" program)
                            (file :must-match t :format "%v"))
                      (set :format "%v"
                           (cons :format "%v"
                                 (const :tag "Arguments:      "
                                        :format "%t" args)
                                 (string :format "%v"))
                           (cons :format "%v"
                                 (const :tag "Face:           "
                                        :format "%t" face)
                                 (face :format "%v"
                                       :value wcheck-default-face))
                           (cons :format "%v"
                                 (const :tag "Syntax table:   "
                                        :format "%t" syntax)
                                 (symbol :format "%v"))
                           (cons :format "%v"
                                 (const :tag "Regexp start:   "
                                        :format "%t" regexp-start)
                                 (regexp :format "%v"
                                         :value "\\<'*"))
                           (cons :format "%v"
                                 (const :tag "Regexp body:    "
                                        :format "%t" regexp-body)
                                 (regexp :format "%v"
                                         :value "\\w+?"))
                           (cons :format "%v"
                                 (const :tag "Regexp end:     "
                                        :format "%t" regexp-end)
                                 (regexp :format "%v"
                                         :value "'*\\>"))
                           (cons :format "%v"
                                 (const :tag "Regexp discard: "
                                        :format "%t" regexp-discard)
                                 (regexp :format "%v"
                                         :value "\\`'+\\'"))))))



(defface wcheck-default-face
  '((t (:underline "red")))
  "Default face for marking strings in a buffer.
This is used when language does not define face."
  :group 'wcheck
  )


(setq-default wcheck-language-data-defaults
              '((args . "")
                (face . wcheck-default-face)
                (syntax . text-mode-syntax-table)
                (regexp-start . "\\<'*")
                (regexp-body . "\\w+?")
                (regexp-end . "'*\\>")
                (regexp-discard . "\\`'+\\'")))

(defvar wcheck-language
  (let ((first (caar wcheck-language-data)))
    (if (stringp first)
        first
      ""))

  "Oletuskieli on globaalissa muuttujassa, puskurikohtainen kieli
on puskurikohtaisessa muuttujassa. Tätä muuttujaa ei kannata
muokata suoraan; kieli kannattaa muuttaa komennolla
`\\[wcheck-change-language]'.")
(make-variable-buffer-local 'wcheck-language)

(setq-default wcheck-buffer-process-data nil
              wcheck-received-words nil)

(make-variable-buffer-local 'wcheck-received-words)

(defconst wcheck-process-name-prefix "wcheck/"
  "Oikolukuprosessien nimen etuliite. Tämä on vain ohjelman
sisäiseen käyttöön.")



(defvar wcheck-mode-map
  (make-sparse-keymap)
  "Keymap for wcheck-mode")


(defconst wcheck-timer-idle .6
  "Näin monta sekuntia odotetaan, kunnes ajastin käynnistää
oikoluvun niissä ikkunoissa, joiden puskuri on sitä pyytänyt.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Käyttäjän funktiot


(defun wcheck-change-language (language &optional global)
  "Vaihtaa oikoluvun kielen arvoksi LANGUAGE. Tavallisesti muutos
koskee vain nykyistä puskuri, mutta jos GLOBAL on
non-nil (interaktiivisesti prefix argument), niin vaihdetaan
oletuskieli."
  (interactive
   (let* ((comp (mapcar 'car wcheck-language-data))
          (default
            (if (member wcheck-language comp)
                wcheck-language
              (car comp))))
     (list (completing-read
            (format "Vaihda %s (%s): " (if current-prefix-arg
                                           "oletuskieli uusiin puskureihin"
                                         "nykyisen puskurin kieli")
                    default)
            comp nil t nil nil default)
           current-prefix-arg)))

  (when (stringp language)
    (if global
        (setq-default wcheck-language language)
      (setq wcheck-language language)
      (when wcheck-mode
        (wcheck-update-buffer-process-data (current-buffer) language)))

    (when (called-interactively-p)
      (let ((program (wcheck-query-language-data language 'program)))
        (cond ((not (wcheck-program-executable-p program))
               (when wcheck-mode
                 (wcheck-mode 0))
               (message (format "Kielen \"%s\" ohjelma \"%s\" ei ole ajettava"
                                language program)))

              (wcheck-mode
               (wcheck-timer-read-request (current-buffer))
               (wcheck-remove-overlays)))))

    wcheck-buffer-process-data))


(define-minor-mode wcheck-mode
  "Sanojen tarkistus, oikoluku."
  :init-value nil
  :lighter " Wck"
  :keymap wcheck-mode-map
  (if wcheck-mode
      ;; Oikoluku päälle mutta ensin pari tarkistusta:
      (cond
       ((minibufferp (current-buffer))
        ;; Kyseessä on minibuffer, joten ei kytketä päälle
        (wcheck-mode 0))

       ((not (wcheck-language-valid-p wcheck-language))
        ;; Kieli ei ole toimiva
        (wcheck-mode 0)
        (message (format "Sopimaton kieli \"%s\", ei kytketä oikolukua"
                         wcheck-language)))

       ((not (wcheck-program-executable-p
              (wcheck-query-language-data wcheck-language 'program)))
        ;; Ohjelmaa ei löydy tai sillä ei ole suoritusoikeuksia
        (wcheck-mode 0)
        (message (format "Kielen \"%s\" ohjelma \"%s\" ei ole ajettava"
                         wcheck-language
                         (wcheck-query-language-data wcheck-language
                                                     'program))))

       (t
        ;; Käynnistetään "oikoluku"

        ;; Puskurikohtaiset koukut
        (add-hook 'kill-buffer-hook 'wcheck-hook-kill-buffer nil t)
        (add-hook 'window-scroll-functions 'wcheck-hook-window-scroll nil t)
        (add-hook 'after-change-functions 'wcheck-hook-after-change nil t)
        (add-hook 'change-major-mode-hook
                  'wcheck-hook-change-major-mode nil t)

        ;; Globaalit koukut. Riittää, että nämä lisää vain kerran, mutta
        ;; varmuuden vuoksi ajetaan seuraavat komennot joka kerta, kun
        ;; wcheck-tila kytketään päälle.
        (add-hook 'window-size-change-functions
                  'wcheck-hook-window-size-change)
        (add-hook 'window-configuration-change-hook
                  'wcheck-hook-window-configuration-change)

        (wcheck-update-buffer-process-data (current-buffer) wcheck-language)

        (unless wcheck-timer
          (setq wcheck-timer
                (run-with-idle-timer wcheck-timer-idle t
                                     'wcheck-timer-event)))

        (wcheck-timer-read-request (current-buffer))))

    ;; Oikoluku pois
    (setq wcheck-returned-words nil)
    (wcheck-remove-overlays)
    (wcheck-update-buffer-process-data (current-buffer) nil)

    (when (and (not wcheck-buffer-process-data)
               wcheck-timer)
      (cancel-timer wcheck-timer)
      (setq wcheck-timer nil)
      ;; Globaalit koukut poistetaan vasta, kun ajastinkin poistetaan
      ;; eli kun mikään puskuri ei enää tarvitse oikolukua.
      (remove-hook 'window-size-change-functions
                   'wcheck-hook-window-size-change)
      (remove-hook 'window-configuration-change-hook
                   'wcheck-hook-window-configuration-change))

    ;; Puskurikohtaiset koukut
    (remove-hook 'kill-buffer-hook 'wcheck-hook-kill-buffer t)
    (remove-hook 'window-scroll-functions 'wcheck-hook-window-scroll t)
    (remove-hook 'after-change-functions 'wcheck-hook-after-change t)
    (remove-hook 'change-major-mode-hook
                 'wcheck-hook-change-major-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ajastimet


(setq-default wcheck-timer nil
              wcheck-timer-read-requested nil
              wcheck-timer-paint-requested nil)


(defun wcheck-timer-read-request (buffer)
  (add-to-list 'wcheck-timer-read-requested buffer))
(defun wcheck-timer-read-request-delete (buffer)
  (setq wcheck-timer-read-requested
        (delq buffer wcheck-timer-read-requested)))

(defun wcheck-timer-paint-request (buffer)
  (add-to-list 'wcheck-timer-paint-requested buffer))
(defun wcheck-timer-paint-request-delete (buffer)
  (setq wcheck-timer-paint-requested
        (delq buffer wcheck-timer-paint-requested)))


(defun wcheck-timer-event ()
  ;; Käydään läpi kaikki puskurit, jotka ovat pyytäneet päivitystä.
  (dolist (buffer wcheck-timer-read-requested)
    (with-current-buffer buffer
      (if (not (wcheck-language-valid-p wcheck-language))
          (progn
            (wcheck-mode 0)
            (message
             (format "Kieltä \"%s\" ei ole olemassa, sammutetaan oikoluku"
                     wcheck-language)))

        ;; Käydään läpi kaikki ikkunat, joissa kyseinen puskuri on
        ;; näkyvissä, ja lähetetään sanat ulkoiselle prosessille.
        (walk-windows
         (function (lambda (window)
                     (when (eq buffer (window-buffer window))
                       (wcheck-send-words wcheck-language
                                          (wcheck-read-words wcheck-language
                                                             window)))))
         'nomb t)
        ;; Sanat on lähetetty, joten voidaan poistaa tämä puskuri
        ;; päivityslistasta.
        (wcheck-timer-read-request-delete buffer))))

  ;; Käynnistetään ajastin, joka maalaa sanat, mikäli joku puskuri on
  ;; sellaista pyytänyt.
  (run-with-idle-timer
   (* 2 wcheck-timer-idle)
   nil
   (function (lambda ()
               (dolist (buffer wcheck-timer-paint-requested)
                 (with-current-buffer buffer
                   (wcheck-remove-overlays)
                   (when wcheck-mode
                     (walk-windows
                      (function (lambda (window)
                                  (when (eq buffer (window-buffer window))
                                    (with-current-buffer buffer
                                      (wcheck-paint-words
                                       wcheck-language
                                       window
                                       wcheck-received-words)))))
                      'nomb t)
                     (wcheck-timer-paint-request-delete buffer)
                     (setq wcheck-received-words nil))))))))


(defun wcheck-receive-words (process string)
  "Ottaa sanat vastaan oikolukuprosessilta ja tallentaa ne
listamuodossa puskurikohtaiseen muuttujaan
wcheck-received-words."
  (setq wcheck-received-words
        (append wcheck-received-words (split-string string "\n+" t)))
  (wcheck-timer-paint-request (current-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Koukut, joilla pyydetään oikoluvun päivitystä puskurille


(defun wcheck-hook-window-scroll (window window-start)
  "Ajetaan kun ikkunaa WINDOW on vieritetty."
  (with-current-buffer (window-buffer window)
    (when wcheck-mode
      (wcheck-timer-read-request (current-buffer)))))


(defun wcheck-hook-window-size-change (frame)
  "Tämä ajetaan aina, kun ikkunan kokoa on muutettu."
  (walk-windows (function (lambda (window)
                            (with-current-buffer (window-buffer window)
                              (when wcheck-mode
                                (wcheck-timer-read-request
                                 (window-buffer window))))))
                'nomb
                frame))


(defun wcheck-hook-window-configuration-change ()
  "Tämä ajetaan aina, kun ikkunan kokoa tai muita asetuksia on
muutettu."
  (walk-windows (function (lambda (window)
                            (with-current-buffer (window-buffer window)
                              (when wcheck-mode
                                (wcheck-timer-read-request
                                 (current-buffer))))))
                'nomb
                'currentframe))


(defun wcheck-hook-after-change (beg end len)
  "Ajetaan aina, kun puskuria on muokattu."
  ;; Tämä hook ajetaan aina siinä puskurissa, mitä muokattiin.
  (when wcheck-mode
    (wcheck-timer-read-request (current-buffer))))


(defun wcheck-hook-kill-buffer ()
  "Sammuttaa oikoluvun tämän puskurin osalta."
  (wcheck-mode 0))


(defun wcheck-hook-change-major-mode ()
  "Ajetaan ennen kuin käyttäjä vaihtaa major-tilaa. Tämä
sammuttaa oikoluvun tästä puskurista."
  (wcheck-mode 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prosessien käsittely


(defun wcheck-start-get-process (language)
  "Palauttaa oikolukuprosessin, joka käsittelee kieltä LANGUAGE.
Mikäli sellaista prosessia ei ennestään ole, käynnistetään."
  (when (wcheck-language-valid-p language)
    (let ((proc-name (concat wcheck-process-name-prefix language)))
      ;; Jos prosessi on jo ennestään olemassa, palautetaan se.
      (or (get-process proc-name)
          ;; Ei ole, joten luodaan uusi.
          (let ((program (wcheck-query-language-data language 'program))
                (args (split-string
                       (wcheck-query-language-data language 'args t)
                       "[ \t\n]+" t))
                (process-connection-type t) ;Käytetään PTY:itä
                proc)

            (when (file-executable-p program)
              (setq proc (apply 'start-process proc-name nil program args))
              ;; Asetetaan oikolukuprosessin tulosteenkäsittely kutsumaan
              ;; funktiota, joka tallentaa tulosteen eli tunnistamattomat
              ;; sanat muuttujaan wcheck-returned-words (buffer-local).
              (set-process-filter proc 'wcheck-receive-words)
              (when (wcheck-process-running-p language)
                proc)))))))


(defun wcheck-process-running-p (language)
  "Tarkistetaan, onko prosessi käynnissä."
  (eq 'run (process-status (concat wcheck-process-name-prefix language))))


(defun wcheck-end-process (language)
  "Poistaa oikolukuprosessin kielelle LANGUAGE, mikäli sellainen
on olemassa. Palautetaan poistettu prosessi tai nil, mikäli ei
tehty mitään."
  (let ((proc (get-process (concat wcheck-process-name-prefix
                                   language))))
    (when proc
      (delete-process proc)
      proc)))


(defun wcheck-update-buffer-process-data (buffer language)
  "Päivittää `wcheck-buffer-process-data' -muuttujan puskurin
BUFFER ja sitä vastaavan kielen LANGUAGE osalta. Mikäli LANGUAGE
on nil, poistetaan kyseinen puskuri listasta ja lopetetaan myös
kieltä vastaava prosessi, mikäli sitä ei enää mikään prosessi
tarvitse. Palautetaan muuttujan `wcheck-buffer-process-data'
uusi arvo tai nil, mikäli funktion parametrit eivät olleet
oikeanlaiset."

  ;; Tämä funktio voisi myös poistaa ne prosessit, joiden nimeen on
  ;; tullut <1>, <2> jne. siitä syystä, ettei olisi kahta samannimistä.
  ;; Tällaista ei pitäisi sattua mutta todellisuudessa kaikki on
  ;; mahdollista. Toinen vaihtoehto on luopua kokonaan miettimästä
  ;; prosessien nimiä ja tehdä sen sijaan alist, jossa on (KIELI .
  ;; PROSESSI) -elementtejä. Se tosin olisi yksi ajan tasalla pidettävä
  ;; lista lisää.

  (when (and (bufferp buffer)
             (or (stringp language)
                 (not language)))

    ;; Poistetaan listasta elementit, joiden cdr ei ole merkkijono
    (dolist (item wcheck-buffer-process-data)
      (unless (stringp (cdr item))
        (setq wcheck-buffer-process-data
              (delq item wcheck-buffer-process-data))))

    (let ((old-langs (mapcar 'cdr wcheck-buffer-process-data))
          new-langs)

      ;; Poistetaan listasta mahdolliset kuolleet puskurit sekä
      ;; minibufferit.
      (dolist (item wcheck-buffer-process-data)
        (when (or (not (buffer-live-p (car item)))
                  (minibufferp (car item)))
          (setq wcheck-buffer-process-data
                (delq item wcheck-buffer-process-data))))

      ;; Poistetaan tämä puskuri listasta
      (setq wcheck-buffer-process-data
            (assq-delete-all buffer wcheck-buffer-process-data))
      (if language
          ;; Lisätään puskurille uusi kieli
          (add-to-list 'wcheck-buffer-process-data
                       (cons buffer language))
        ;; Oikolukua on pyydetty sammutettavaksi, joten poistetaan se
        ;; päivitystä pyytäneiden prosessien listasta.
        (wcheck-timer-read-request-delete buffer))

      ;; Poistetaan turhat prosessit
      (setq new-langs (mapcar 'cdr wcheck-buffer-process-data))
      (dolist (lang old-langs)
        (unless (member lang new-langs)
          (wcheck-end-process lang)))))

  wcheck-buffer-process-data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matalan tason apufunktioita


(defun wcheck-read-words (language window)
  "Palauttaa listan sanoista, jotka näkyvät ikkunassa IKKUNA."
  (when (window-live-p window)
    (with-selected-window window
      (save-excursion

        (let ((regexp (concat
                       (wcheck-query-language-data language 'regexp-start t)
                       "\\("
                       (wcheck-query-language-data language 'regexp-body t)
                       "\\)"
                       (wcheck-query-language-data language 'regexp-end t)))

              (syntax (eval (wcheck-query-language-data language 'syntax t)))
              (w-end (window-end window 'update))
              (discard (wcheck-query-language-data language 'regexp-discard t))
              (case-fold-search nil)
              words)

          (move-to-window-line 0)
          (beginning-of-line)
          (with-syntax-table syntax
            (while (< (point) w-end)
              (while (re-search-forward regexp (line-end-position) t)
                (when (or (equal discard "")
                          (not (string-match discard
                                             (match-string-no-properties 1))))
                  (add-to-list 'words
                               (match-string-no-properties 1)
                               'append))
                (goto-char (1+ (point))))
              (end-of-line)
              (vertical-motion 1)))
          words)))))


(defun wcheck-send-words (language wordlist)
  "Lähettää sanalistan WORDLIST oikolukuprosessille, joka
käsittelee kieltä LANGUAGE."
  ;; Noudetaan prosessi, joka hoitaa pyydetyn kielen.
  (let ((proc (wcheck-start-get-process language))
        string)
    ;; Tehdään sanalistasta merkkijono, yksi sana rivillään.
    (setq string (concat "\n" (mapconcat 'concat wordlist "\n") "\n"))
    (process-send-string proc string)
    string))


(defun wcheck-paint-words (language window wordlist)
  "Merkkaa listassa WORDLIST listatut sanat ikkunassa WINDOW."
  (when (window-live-p window)
    (with-selected-window window
      (save-excursion
        (let ((buffer (window-buffer window))
              (w-start (window-start window))
              (w-end (window-end window 'update))
              (r-start (wcheck-query-language-data language 'regexp-start t))
              (r-end (wcheck-query-language-data language 'regexp-end t))
              (syntax (eval (wcheck-query-language-data language 'syntax t)))
              (case-fold-search nil))
          (with-syntax-table syntax
            (dolist (word wordlist)
              (setq word (regexp-quote word))
              (goto-char w-start)
              (while (re-search-forward
                      (concat r-start "\\(" word "\\)" r-end)
                      w-end t)
                (wcheck-make-overlay language buffer
                                     (match-beginning 1)
                                     (match-end 1))))))))))


(defun wcheck-query-language-data (language key &optional default)
  "Palauttaa pyydetyn tiedon kielitietokannasta tai mahdollisesti
oletusarvon."
  (or (cdr (assq key (cdr (assoc language wcheck-language-data))))
      (when default
        (cdr (assq key wcheck-language-data-defaults)))))


(defun wcheck-language-valid-p (language)
  "Tarkistaa, onko LANGUAGE olemassa ja onko sille määritelty
ulkoista ohjelmaa. Palauttaa t tai nil."
  (and (member language (mapcar 'car wcheck-language-data))
       (stringp (wcheck-query-language-data language 'program))
       t))


(defun wcheck-program-executable-p (program)
  (and (stringp program)
       (file-regular-p program)
       (file-executable-p program)
       t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay


(defun wcheck-make-overlay (language buffer beg end)
  (let ((overlay (make-overlay beg end buffer))
        (face (wcheck-query-language-data language 'face t)))
    (dolist (prop `((wcheck-mode . t)
                    (face . ,face)
                    (modification-hooks . (wcheck-remove-overlay-word))
                    (insert-in-front-hooks . (wcheck-remove-overlay-word))
                    (insert-behind-hooks . (wcheck-remove-overlay-word))
                    (evaporate . t)))
      (overlay-put overlay (car prop) (cdr prop)))))


(defun wcheck-remove-overlays (&optional beg end)
  (remove-overlays beg end 'wcheck-mode t))


(defun wcheck-remove-overlay-word (overlay after beg end &optional len)
  "Poistaa overlayn, jonka osoittamaa sanaa muokataan."
  (unless after
    ;; Juuri ennen kuin muokkaus alkaa poistetaan overlay.
    (delete-overlay overlay)))


(provide 'wcheck-mode)
