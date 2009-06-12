;;; Copyright © 2009 The Open University

(in-package #:porky.io.support)

(defun read-file-to-string (filename)
  (with-open-file (stream filename)
     (read-stream-to-string stream)))

(defun read-stream-to-string (stream)
  (apply #'concatenate 'string
         (apply #'append
                (let ((whitespace (format nil "~%")))
                  (loop for line = (read-line stream nil :eof)
                        until (eq line :eof)
                        collect (list line whitespace))))))

;;; {{{ Lexer tools
(defmacro deflexer (name &body rule-definitions)
  (let ((buffer (gensym))
        (rules (gensym)))
    `(let ((,rules (list ,@(compile-rule-definitions rule-definitions))))
       (defun ,name (,buffer)
         (make-lexer ,rules ,buffer)))))

(defun make-lexer (rules buffer)
  (let ((pos 0))
    (lambda ()
      (let ((match (try-tokenising rules buffer pos)))
	(when match
	  (setf pos (third match))
          (if (eq :end (first match))
              nil
              (values (first match)
                      (subseq buffer (second match) (third match)))))))))

(defun try-tokenising (rules buffer pos)
  (loop for rule in rules
	do (multiple-value-bind (start end)
               (cl-ppcre:scan (fourth rule) buffer :start pos)
             (when (and start (= start pos))
               (return (if (eq 'ignore (first rule))
                           (try-tokenising rules buffer end)
                           (list (second rule) start end)))))))

(defun compile-rule-definitions (defs)
  (mapcar (lambda (rule)
            (let ((action (first rule))
                  (label (second rule))
                  (regexp (third rule))
                  (options (cdddr rule)))
              `(list ',action ',label ',regexp
                     (apply #'cl-ppcre:create-scanner ,regexp
                            ,(if options
                                 (list 'quote options)
                                 ''(:single-line-mode t))))))
          defs))
;;; }}}
