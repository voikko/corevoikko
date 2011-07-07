(in-package :voikko)


(defclass option ()
  ((id :initarg :id)
   (value :initarg :value)))
(defclass boolean-option (option) nil)
(defclass integer-option (option) nil)

(defun make-option (key value)
  (let (data)
    (cond ((setf data (assoc key *voikko-boolean-options*))
           (make-instance 'boolean-option :id (cdr data) :value value))
          ((setf data (assoc key *voikko-integer-options*))
           (make-instance 'integer-option :id (cdr data) :value value))
          (t (error "Unknown option.")))))

(defgeneric set-option-caller (instance option))
  
(defmethod set-option-caller :before ((instance instance) option)
  (declare (ignore option))
  (error-if-not-active-instance instance))
  
(defmethod set-option-caller ((instance instance) (option boolean-option))
  (with-slots (id value) option
    (let ((success (foreign-funcall "voikkoSetBooleanOption"
                                    :pointer (address instance)
                                    :int id :int (if value 1 0) :int)))
      (if (zerop success) nil t))))

(defmethod set-option-caller ((instance instance) (option integer-option))
  (with-slots (id value) option
    (unless (integerp value)
      (error "Value must be an integer."))
    (let ((success (foreign-funcall "voikkoSetIntegerOption"
                                    :pointer (address instance)
                                    :int id :int value :int)))
      (if (zerop success) nil t))))

(defun set-option (instance key value)
  (set-option-caller instance (make-option key value)))
