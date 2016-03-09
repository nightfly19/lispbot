#!/usr/bin/sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (debug 3))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-irc))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl+ssl))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre))

(defvar *connection* nil)
(defvar *settings* (with-open-file (in "settings.lisp")
                     (with-standard-io-syntax (read in))))

(defun part ()
  (when *connection*
    (irc:quit *connection*)
    (setf *connection* nil)))

(defparameter *safe-symbols* '(+ - * /))

(defun expression-safep (expression)
  (if (listp expression)
      (if (member (car expression) *safe-symbols*)
          t)
      t))

(defun message-handler (message)
  (let* ((arguments (irc:arguments message))
         (channel (first arguments))
         (contents (second arguments))
         (sender (irc:source message)))
    (ppcre:register-groups-bind
     (expression-to-read) ("^\\|(.*)$" contents)
     (when expression-to-read
       (handler-case
           (let ((user-expression (read-from-string expression-to-read)))
             (when (expression-safep user-expression)
               (let ((user-result (eval user-expression)))
                 (irc:privmsg *connection* channel (format nil "~a" user-result)))))
         (error (e) (format t "~%derp~%")))))))

(defun derpage ()
  (unless *connection*
    (setf *connection* (apply #'irc:connect (getf (getf *settings* :irc) :server))))
  (loop for channel in (getf (getf *settings* :irc) :channels) do
       (format t "~a~%" channel)
       (apply #'irc:join (cons *connection* channel)))
  (irc:add-hook *connection* 'irc:irc-privmsg-message (lambda (message) (message-handler message)))
  (irc:read-message-loop *connection*))

(derpage)
