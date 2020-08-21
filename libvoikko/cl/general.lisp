;;;; A Common Lisp interface for libvoikko
;;
;; Copyright (C) 2011-2012, 2019-2020 Teemu Likonen <tlikonen@iki.fi>
;;
;; The contents of this file are subject to the Mozilla Public License Version
;; 1.1 (the "License"); you may not use this file except in compliance with
;; the License. You may obtain a copy of the License at
;; http://www.mozilla.org/MPL/
;;
;; Software distributed under the License is distributed on an "AS IS" basis,
;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;; for the specific language governing rights and limitations under the
;; License.
;;
;; Alternatively, the contents of this file may be used under the terms of
;; either the GNU General Public License Version 2 or later (the "GPL"), or
;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;; in which case the provisions of the GPL or the LGPL are applicable instead
;; of those above. If you wish to allow use of your version of this file only
;; under the terms of either the GPL or the LGPL, and not to allow others to
;; use your version of this file under the terms of the MPL, indicate your
;; decision by deleting the provisions above and replace them with the notice
;; and other provisions required by the GPL or the LGPL. If you do not delete
;; the provisions above, a recipient may use your version of this file under
;; the terms of any one of the MPL, the GPL or the LGPL.

(in-package #:voikko)

(eval-when (:load-toplevel :execute)
  (with-simple-restart (skip-libvoikko "Skip loading libvoikko.")
    (cffi:load-foreign-library '(:default "libvoikko"))))

(define-condition voikko-error (error)
  ((error-string :initarg :string :reader error-string))
  (:report (lambda (condition stream)
             (format stream "~A" (error-string condition)))))

(define-condition initialize-error (voikko-error) nil)
(define-condition internal-error (voikko-error) nil)
(define-condition charset-conversion-error (voikko-error) nil)
(define-condition hyphenation-error (voikko-error) nil)
(define-condition not-active-instance-error (voikko-error) nil)

(defclass foreign-object ()
  ((address :initarg :address :accessor address)))

(defclass instance (foreign-object)
  nil
  (:documentation
   "The object denotes a Voikko instance which have been initialized
with INITIALIZE function.

Objects in this class can be either active or inactive. Active means
that the instance can be used with Voikko functions such as SPELL,
SUGGEST and HYPHENATE. Inactive means that the instance have been
terminated with TERMINATE function and can't be used with Voikko
functions anymore.

You can use function ACTIVEP to test whether an INSTANCE object is
active or not."))

(defun proper-pointer-p (object)
  (and (cffi:pointerp object)
       (not (cffi:null-pointer-p object))))

(defun instancep (object)
  (typep object 'instance))

(defgeneric activep (object)
  (:documentation "Return a boolean whether OBJECT is active."))

(defmethod activep ((object foreign-object))
  (proper-pointer-p (address object)))

(defun error-if-not-active-instance (object)
  (unless (and (instancep object) (activep object))
    (error 'not-active-instance-error
           :string "The object is not an active Voikko instance.")))

(defmethod print-object ((object foreign-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~A)" (if (activep object) "ACTIVE" "INACTIVE"))))

(defun version ()
  "Return the version number of libvoikko (as a string)."
  (cffi:foreign-funcall "voikkoGetVersion" :string))

(defgeneric free-foreign-resource (object))

(defmethod free-foreign-resource :around ((object foreign-object))
  (when (activep object)
    (call-next-method)
    (setf (address object) nil)
    object))

(defmethod free-foreign-resource ((object instance))
  (cffi:foreign-funcall "voikkoTerminate" :pointer (address object) :void))

(defun initialize (&key (language "fi_FI") path)
  "Initialize a Voikko instance for LANGUAGE (string). Language
dictionaries are searched from Libvoikko's default locations and
additionally from PATH (string).

Return an object of type INSTANCE which can then be used with other
Voikko functions. The instance must be closed with TERMINATE function
after use. See also the macro WITH-INSTANCE which automatically
initializes and terminates a Voikko instance.

If instance couldn't be initialized a condition of type INITIALIZE-ERROR
is signaled. In that case there is an active restart called
CHANGE-LANGUAGE. It can be invoked with a new language string as its
argument. The restart retries the initialize process."

  (check-type language string)
  (check-type path (or string null))
  (cffi:with-foreign-object (error :pointer)
    (loop (restart-case
              (let ((address (cffi:foreign-funcall "voikkoInit"
                                                   :pointer error
                                                   :string language
                                                   :string (if (null path)
                                                               (cffi:null-pointer)
                                                               path)
                                                   :pointer)))

                (if (and (cffi:pointerp address)
                         (cffi:null-pointer-p (cffi:mem-aref error :pointer)))
                    (return (make-instance 'instance :address address))
                    (error 'initialize-error
                           :string (cffi:mem-aref error :string))))

            (change-language (new-language)
              :report "Use a different language"
              :interactive (lambda ()
                             (format *query-io* "~&Enter new language: ")
                             (force-output *query-io*)
                             (list (string-trim " " (read-line *query-io*))))
              (setf language new-language))))))

(defun terminate (instance)
  "Terminate a Voikko instance and free all resources associated with
it."
  (assert (instancep instance) nil "The object is not a Voikko instance.")
  (free-foreign-resource instance))

(defmacro with-instance ((variable &key (language "fi_FI") path)
                         &body body)
  "Initialize a Voikko instance using INITIALIZE function (with its
keyword arguments), bind VARIABLE to the INSTANCE object and execute
BODY forms. Finally, terminate the instance with TERMINATE function and
return the values of the last body form."

  (let ((instance (gensym "INSTANCE")))
    `(let* ((,instance (initialize :language ,language
                                   :path ,path))
            (,variable ,instance))
       (declare (ignorable ,variable))
       (unwind-protect (progn ,@body)
         (terminate ,instance)))))

(defun list-dictionaries (&optional path)
  "Get a list of available dictionaries from Libvoikko's default search
paths and additionally from PATH (string). Return a list which contains
one association list for each dictionary. The association list has the
following keys (keyword symbols): :LANGUAGE, :SCRIPT, :VARIANT and
:DESCRIPTION. Each key's value is a string."
  (check-type path (or string null))
  (let ((dicts-ptr (cffi:foreign-funcall "voikko_list_dicts"
                                    :string (if (null path)
                                                (cffi:null-pointer)
                                                path)
                                    :pointer)))
    (when (proper-pointer-p dicts-ptr)
      (unwind-protect
           (loop :for i :upfrom 0
              :for dict-ptr := (cffi:mem-aref dicts-ptr :pointer i)
              :while (proper-pointer-p dict-ptr)
              :collect
                (list (cons :language (cffi:foreign-funcall
                                       "voikko_dict_language"
                                       :pointer dict-ptr :string))
                      (cons :script (cffi:foreign-funcall
                                     "voikko_dict_script"
                                     :pointer dict-ptr :string))
                      (cons :variant (cffi:foreign-funcall
                                      "voikko_dict_variant"
                                      :pointer dict-ptr :string))
                      (cons :description (cffi:foreign-funcall
                                          "voikko_dict_description"
                                          :pointer dict-ptr :string))))
        (cffi:foreign-funcall "voikko_free_dicts" :pointer dicts-ptr :void)))))

(defun list-supported-spelling-languages (&optional path)
  "Return a list of language codes (string) representing the languages
for which at least one dictionary is available for spell checking."
  (check-type path (or string null))
  (let ((ptr (cffi:foreign-funcall "voikkoListSupportedSpellingLanguages"
                                   :string (if (null path)
                                               (cffi:null-pointer)
                                               path)
                                   :pointer)))
    (when (proper-pointer-p ptr)
      (loop :for i :upfrom 0
         :for lang := (cffi:mem-aref ptr :string i)
         :while (stringp lang)
         :collect lang))))

(defun list-supported-hyphenation-languages (&optional path)
  "Return a list of language codes (string) representing the languages
for which at least one dictionary is available for hyphenation."
  (check-type path (or string null))
  (let ((ptr (cffi:foreign-funcall "voikkoListSupportedHyphenationLanguages"
                                   :string (if (null path)
                                               (cffi:null-pointer)
                                               path)
                                   :pointer)))
    (when (proper-pointer-p ptr)
      (loop :for i :upfrom 0
         :for lang := (cffi:mem-aref ptr :string i)
         :while (stringp lang)
         :collect lang))))

(defun list-supported-grammar-checking-languages (&optional path)
  "Return a list of language codes (string) representing the languages
for which at least one dictionary is available for grammar checking."
  (check-type path (or string null))
  (let ((ptr (cffi:foreign-funcall "voikkoListSupportedGrammarCheckingLanguages"
                                   :string (if (null path)
                                               (cffi:null-pointer)
                                               path)
                                   :pointer)))
    (when (proper-pointer-p ptr)
      (loop :for i :upfrom 0
         :for lang := (cffi:mem-aref ptr :string i)
         :while (stringp lang)
         :collect lang))))
