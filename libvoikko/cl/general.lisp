;;;; A Common Lisp interface for libvoikko
;;
;; Copyright (C) 2011-2012 Teemu Likonen <tlikonen@iki.fi>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; The license text: <http://www.gnu.org/licenses/gpl-2.0.html>

(in-package #:voikko)

(eval-when (:load-toplevel)
  (with-simple-restart (skip-libvoikko "Skip loading libvoikko.")
    (load-foreign-library '(:default "libvoikko"))))

(define-condition voikko-error (error)
  ((error-string :initarg :string))
  (:report (lambda (condition stream)
             (format stream "~A" (slot-value condition 'error-string)))))

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
  (and (pointerp object)
       (not (null-pointer-p object))))

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
  (foreign-funcall "voikkoGetVersion" :string))

(defgeneric free-foreign-resource (object))

(defmethod free-foreign-resource :around ((object foreign-object))
  (when (activep object)
    (call-next-method)
    (setf (address object) nil)
    object))

(defmethod free-foreign-resource ((object instance))
  (foreign-funcall "voikkoTerminate" :pointer (address object) :void))

(defun initialize (&key (language "fi_FI"))
  "Initialize a Voikko instance for LANGUAGE. Return an object of type
INSTANCE which can then be used with other Voikko functions. The
instance must be closed with TERMINATE function after use. See also the
macro WITH-INSTANCE which automatically initializes and terminates a
Voikko instance.

If instance couldn't be initialized a condition of type INITIALIZE-ERROR
is signaled. In that case there is an active restart called
CHANGE-LANGUAGE. It can be invoked with a new language string as its
argument. The restart retries the initialize process."

  (with-foreign-object (error :pointer)
    (loop (restart-case
              (let ((address (foreign-funcall "voikkoInit"
                                              :pointer error
                                              :string language
                                              :pointer (null-pointer)
                                              :pointer)))

                (if (and (pointerp address)
                         (null-pointer-p (mem-aref error :pointer)))
                    (return (make-instance 'instance :address address))
                    (error 'initialize-error
                           :string (mem-aref error :string))))

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

(defmacro with-instance ((variable &key (language "fi_FI")) &body body)
  "Initialize a Voikko instance for LANGUAGE, bind VARIABLE to the
INSTANCE object and execute BODY forms. Finally, terminate the instance
and return the values of the last body form."

  (let ((instance (gensym "INSTANCE")))
    `(let* ((,instance (initialize :language ,language))
            (,variable ,instance))
       (declare (ignorable ,variable))
       (unwind-protect (progn ,@body)
         (terminate ,instance)))))
