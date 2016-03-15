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

(defparameter *channel* nil)
(defparameter *sender* nil)
(defparameter *message* nil)
(defparameter *silently* nil)

(defun part (channel &optional reason)
  (when *connection*
    (if reason
        (irc:part *connection* channel reason)
        (irc:part *connection* channel))))

(defun join (channel &optional key)
  (when *connection*
    (if key
        (irc:join *connection* channel :password key)
        (irc:join *connection* channel))))

(defun say (channel message)
  (irc:privmsg *connection* channel (format nil "~a~%" message)))

(defmacro silently (&body body)
  `(progn
     (setf *silently* t)
     ,@body))

(defgeneric command (command &rest args))

(defmethod command (command &rest args)
  (irc:privmsg *connection* *channel* (format nil "~a~%" command)))

(defmethod command ((command list) &rest args)
  (let* ((user-lambda (eval `(lambda () ,command)))
         *silently*
         (result (funcall user-lambda)))
    (when (not *silently*)
      (irc:privmsg *connection* *channel* (format nil "~a~%" result)))))

(defmethod command ((command (eql 'help)) &rest args)
  (irc:privmsg *connection* *channel* "I do lisp things"))

(defmethod command ((command (eql 'source)) &rest args)
  (irc:privmsg *connection* *channel* "https://github.com/nightfly19/lispbot"))

(defun message-handler (message)
  (let* ((arguments (irc:arguments message))
         (*channel* (first arguments))
         (*message* (second arguments))
         (*sender* (irc:source message)))
    (ppcre:register-groups-bind
        (expression-to-read) ("^\\|(.*)$" *message*)
      (when expression-to-read
        (handler-case
            (let ((user-expression (read-from-string (format nil "(~a)" expression-to-read))))
              (apply #'command user-expression))
          (error (e)
            (irc:privmsg *connection* *channel* (format nil "ERROR: ~a~%" e))))))))

(defun start ()
  (unless *connection*
    (setf *connection* (apply #'irc:connect (getf (getf *settings* :irc) :server))))
  (loop for channel in (getf (getf *settings* :irc) :channels) do
       (format t "~a~%" channel)
       (apply #'irc:join (cons *connection* channel)))
  (irc:add-hook *connection* 'irc:irc-privmsg-message #'message-handler)
  (irc:read-message-loop *connection*))

(sb-ext:save-lisp-and-die "lispbot" :executable t :toplevel #'start)
