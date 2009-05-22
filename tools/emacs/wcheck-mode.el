;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wcheck-mode.el
;;
;; Interface for external spell-checkers and text-filtering programs.


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


;; INSTALLATION
;;
;; Put this file to some directory in your "load-path" and add the
;; following lines to your Emacs initialization file (~/.emacs):
;;
;;     (autoload 'wcheck-mode "wcheck-mode"
;;       "Toggle Wcheck mode." t)
;;     (autoload 'wcheck-change-language "wcheck-mode"
;;       "Switch Wcheck-mode languages." t)
;;
;; See customize group "wcheck" for information on how to configure
;; Wcheck mode. (M-x customize-group RET wcheck RET)


;;; Muuttujat ja asetukset


;;;###autoload
(defgroup wcheck nil
  "Interface for external text-filtering programs."
  :group 'applications)


;;;###autoload
(defcustom wcheck-language-data nil
  "Wcheck mode language configuration.

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
                            (file :format "%v"))
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



;;;###autoload
(defcustom wcheck-language ""
  "Default language for `wcheck-mode'.
The default language used by new buffers. For buffer-specific
languages use the command `\\[wcheck-change-language]'."
  :type '(string :tag "Default language")
  :group 'wcheck)
(make-variable-buffer-local 'wcheck-language)


;;;###autoload
(defface wcheck-default-face
  '((t (:underline "red")))
  "Default face for marking strings in a buffer.
This is used when language does not define face."
  :group 'wcheck)


(setq-default wcheck-language-data-defaults
              '((args . "")
                (face . wcheck-default-face)
                (syntax . text-mode-syntax-table)
                (regexp-start . "\\<'*")
                (regexp-body . "\\w+?")
                (regexp-end . "'*\\>")
                (regexp-discard . "\\`'+\\'")))


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


;;;###autoload
(defun wcheck-change-language (language &optional global)
  "Change language for current buffer (or globally).
Change `wcheck-mode' language to LANGUAGE. The change is
buffer-local but if GLOBAL is non-nil (prefix argument if called
interactively) then change the default language for new buffers."
  (interactive
   (let* ((comp (mapcar 'car wcheck-language-data))
          (default (cond ((member wcheck-language comp)
                          wcheck-language)
                         ((car comp))
                         (t ""))))
     (list (completing-read
            (format (if current-prefix-arg
                        "Default language for new buffers (%s): "
                      "Language for the current buffer (%s): ")
                    default)
            comp nil t nil nil default)
           current-prefix-arg)))

  ;; Change the language, locally or globally, and update buffer-process
  ;; bookkeeping data, if needed.
  (when (stringp language)
    (if global
        (setq-default wcheck-language language)
      (setq wcheck-language language)
      (when wcheck-mode
        (wcheck-update-buffer-process-data (current-buffer) language)))

    ;; If this was called interactively do some checks and maintenance.
    (when (called-interactively-p)
      (let ((program (wcheck-query-language-data language 'program)))
        (cond ((not (wcheck-program-executable-p program))
               ;; No executable program for the selected language. Turn
               ;; the mode off.
               (when wcheck-mode
                 (wcheck-mode 0))
               (message
                (format "Language \"%s\": program \"%s\" is not executable"
                        language program)))

              ;; If the mode is currently turned on we request an update
              (wcheck-mode
               (wcheck-timer-read-request (current-buffer))
               (wcheck-remove-overlays)))))

    wcheck-buffer-process-data))


;;;###autoload
(define-minor-mode wcheck-mode
  "Interface for external spell-checkers and filtering programs.

Wcheck is a minor mode for automatically marking words or other
text pieces in Emacs buffer. Wcheck sends (parts of) buffer's
content to an external text-filtering program and, based on its
output, decides if some parts of text should be marked.

Wcheck can be used with spell-checker programs such as Ispell,
Aspell and Enchant. Then the semantics of operation is that the
words returned from a spelling checker are spelling mistakes and
are marked as such in Emacs buffer.

The mode can also be useful with other kind of external tools.
Any tool that can receive text stream from standard input and
send text to standard output can be used. User is free to
interpret the semantics. In Wcheck configuration different
semantical units are called \"languages\".

See the documentation of variable `wcheck-language-data' for
information on how to configure Wcheck mode. Interactive command
`wcheck-change-language' is used to switch languages."

  :init-value nil
  :lighter " Wck"
  :keymap wcheck-mode-map
  (if wcheck-mode
      ;; Turn on Wcheck mode, but first some checks...

      (cond
       ((minibufferp (current-buffer))
        ;; This is a minibuffer; stop here.
        (wcheck-mode 0))

       ((not (wcheck-language-valid-p wcheck-language))
        ;; Not a valid language.
        (wcheck-mode 0)
        (message (format "Language \"%s\" is not valid"
                         wcheck-language)))

       ((not (wcheck-program-executable-p
              (wcheck-query-language-data wcheck-language 'program)))
        ;; The program does not exist or is not executable.
        (wcheck-mode 0)
        (message (format "Language \"%s\": program \"%s\" is not executable"
                         wcheck-language
                         (wcheck-query-language-data wcheck-language
                                                     'program))))

       (t
        ;; We are ready to really turn on the mode.

        ;; Add buffer-local hooks. These ask for updates for the buffer
        ;; or may sometimes automatically turn off the mode.
        (add-hook 'kill-buffer-hook 'wcheck-hook-kill-buffer nil t)
        (add-hook 'window-scroll-functions 'wcheck-hook-window-scroll nil t)
        (add-hook 'after-change-functions 'wcheck-hook-after-change nil t)
        (add-hook 'change-major-mode-hook
                  'wcheck-hook-change-major-mode nil t)
        (add-hook 'outline-view-change-hook
                  'wcheck-hook-outline-view-change nil t)

        ;; Add global hooks. It's probably sufficient to add these only
        ;; once but it's no harm to ensure their existence every time.
        (add-hook 'window-size-change-functions
                  'wcheck-hook-window-size-change)
        (add-hook 'window-configuration-change-hook
                  'wcheck-hook-window-configuration-change)

        ;; Add this buffer to the bookkeeper.
        (wcheck-update-buffer-process-data (current-buffer) wcheck-language)

        ;; Start idle timer if it's not already started. The timer runs
        ;; a function which updates buffers which have requested for
        ;; that.
        (unless wcheck-timer
          (setq wcheck-timer
                (run-with-idle-timer wcheck-timer-idle t
                                     'wcheck-timer-read-event)))

        ;; Request update for this buffer.
        (wcheck-timer-read-request (current-buffer))))

    ;; Turn off the mode.

    ;; We clear overlays form the buffer, remove the buffer from
    ;; bookkeeper's data and clear the variable holding words received
    ;; from external process.
    (wcheck-remove-overlays)
    (wcheck-update-buffer-process-data (current-buffer) nil)
    (setq wcheck-received-words nil)

    ;; If there are no buffers using Wcheck mode anymore, stop the idle
    ;; timer and remove global hooks.
    (when (not wcheck-buffer-process-data)
      (remove-hook 'window-size-change-functions
                   'wcheck-hook-window-size-change)
      (remove-hook 'window-configuration-change-hook
                   'wcheck-hook-window-configuration-change)
      (when wcheck-timer
        (cancel-timer wcheck-timer)
        (setq wcheck-timer nil)))

    ;; Remove buffer-local hooks.
    (remove-hook 'kill-buffer-hook 'wcheck-hook-kill-buffer t)
    (remove-hook 'window-scroll-functions 'wcheck-hook-window-scroll t)
    (remove-hook 'after-change-functions 'wcheck-hook-after-change t)
    (remove-hook 'change-major-mode-hook
                 'wcheck-hook-change-major-mode t)
    (remove-hook 'outline-view-change-hook
                 'wcheck-hook-outline-view-change t)))


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


(defun wcheck-timer-read-event ()
  "Send windows' content to external program.
This function is usually called by the wcheck-mode idle timer.
The function walks through all windows which belong to buffer
that have requested update. It reads windows' content and sends
it to an external program. Finally, this function starts another
idle timer (just once) for marking words or other text elements
in buffers."

  (dolist (buffer wcheck-timer-read-requested)
    (with-current-buffer buffer

      ;; We are about to fulfill buffer's window-reading request so
      ;; remove this buffer from the request list.
      (wcheck-timer-read-request-delete buffer)

      ;; Reset also the list of received word.
      (setq wcheck-received-words nil)

      (if (not (wcheck-language-valid-p wcheck-language))
          (progn
            (wcheck-mode 0)
            (message
             (format "Language \"%s\" is not valid"
                     wcheck-language)))

        ;; Walk through all windows which belong to this buffer and send
        ;; their content to an external program.
        (walk-windows
         (function (lambda (window)
                     (when (eq buffer (window-buffer window))
                       (wcheck-send-words wcheck-language
                                          (wcheck-read-words wcheck-language
                                                             window)))))
         'nomb t))))

  ;; Start a timer which will mark text in buffers/windows.
  (run-with-idle-timer (+ wcheck-timer-idle
                          (wcheck-current-idle-time-seconds))
                       nil 'wcheck-timer-paint-event
                       2))


(defun wcheck-timer-paint-event (&optional repeat)
  "Mark text in windows.

This is normally called by the `wcheck-mode' idle timer. This
function marks (with overlays) words or other text elements in
buffers that have requested it through the variable
`wcheck-timer-paint-requested'.

If the optional argument REPEAT exists and is integer then also
call the function repeatedly that many times after the first
call. The delay between consecutive calls is defined in variable
`wcheck-timer-idle'."

  (dolist (buffer wcheck-timer-paint-requested)
    (with-current-buffer buffer
      (wcheck-remove-overlays)

      ;; We are about to mark text in this buffer so remove the buffer
      ;; from the request list.
      (wcheck-timer-paint-request-delete buffer)

      ;; Walk through windows and mark text based on the word list
      ;; returned by an external process.
      (when wcheck-mode
        (walk-windows
         (function (lambda (window)
                     (when (eq buffer (window-buffer window))
                       (with-current-buffer buffer
                         (wcheck-paint-words wcheck-language window
                                             wcheck-received-words)))))
         'nomb t))))

  ;; If REPEAT is positive integer call this function again after
  ;; waiting wcheck-timer-idle. Pass REPEAT minus one as the parameter.
  (when (and (integerp repeat)
             (> repeat 0))
    (run-with-idle-timer (+ wcheck-timer-idle
                            (wcheck-current-idle-time-seconds))
                         nil 'wcheck-timer-paint-event
                         (1- repeat))))


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


(defun wcheck-hook-outline-view-change ()
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

            (when (wcheck-program-executable-p program)
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
  "Return a list of visible text elements in WINDOW.
Function scans WINDOW and searches for text elements defined in
LANGUAGE (see `wcheck-language-data'). The returned list contains
only visible text elements; all hidden parts are omitted."
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
              (w-start (window-start window))
              (w-end (window-end window 'update))
              (buffer (window-buffer window))
              (discard (wcheck-query-language-data language 'regexp-discard t))
              (case-fold-search nil)
              (old-point 0)
              words)

          (with-syntax-table syntax
            (goto-char w-start)
            (catch 'infinite
              (while (re-search-forward regexp w-end t)
                (cond ((= (point) old-point)
                       ;; Make sure we don't end up in an infinite loop
                       ;; when the regexp always matches with zero width
                       ;; in the current point position.
                       (throw 'infinite t))

                      ((get-char-property (match-beginning 1)
                                          'invisible buffer)
                       ;; This point is invisible. Let's jump forward to
                       ;; next change of "invisible" property.
                       (goto-char (next-single-char-property-change
                                   (match-beginning 1) 'invisible buffer
                                   w-end)))

                      ((or (equal discard "")
                           (not (string-match
                                 discard (match-string-no-properties 1))))
                       ;; Add the match to the word list.
                       (add-to-list 'words
                                    (match-string-no-properties 1)
                                    'append)))
                (setq old-point (point)))))
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
  "Mark words in WORDLIST which are visible in WINDOW.
Mark all words (or other text elements) in WORDLIST which are
visible in WINDOW. Regular expression text-search respects the
syntax table settings defined in LANGUAGE (see
`wcheck-language-data')."

  (when (window-live-p window)
    (with-selected-window window
      (save-excursion
        (let ((buffer (window-buffer window))
              (w-start (window-start window))
              (w-end (window-end window 'update))
              (r-start (wcheck-query-language-data language 'regexp-start t))
              (r-end (wcheck-query-language-data language 'regexp-end t))
              (syntax (eval (wcheck-query-language-data language 'syntax t)))
              (case-fold-search nil)
              regexp old-point)

          (with-syntax-table syntax
            (dolist (word wordlist)
              (setq regexp (concat r-start "\\("
                                   (regexp-quote word) "\\)"
                                   r-end)
                    old-point 0)
              (goto-char w-start)

              (catch 'infinite
                (while (re-search-forward regexp w-end t)
                  (cond ((= (point) old-point)
                         ;; We didn't move forward so break the loop.
                         (throw 'infinite t))
                        ((get-char-property (match-beginning 1)
                                            'invisible buffer)
                         ;; The point is invisible so jump forward to
                         ;; the next change of "invisible" text property
                         (goto-char (next-single-char-property-change
                                     (match-beginning 1) 'invisible buffer
                                     w-end)))
                        (t
                         ;; Make an overlay
                         (wcheck-make-overlay language buffer
                                              (match-beginning 1)
                                              (match-end 1))))
                  (setq old-point (point)))))))))))


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


(defun wcheck-current-idle-time-seconds ()
  "Return current idle time in seconds."
  (let* ((idle (or (current-idle-time)
                   '(0 0 0)))
         (high (nth 0 idle))
         (low (nth 1 idle))
         (micros (nth 2 idle)))
    (+ (* high
          (expt 2 16))
       low
       (/ micros 1000000.0))))


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
