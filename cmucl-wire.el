;;; cmucl-wire.el ---  trivial implementation of the CMUCL wire protocol

;; Author: Eric Marsden <emarsden@laas.fr>
;; Time-stamp: <2003-08-07 emarsden>
;; Version: 0.1
;; Keywords: comm
;;
;;     Copyright (C) 2003  Eric Marsden
;;   
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <emarsden@laas.fr>.
;; The latest version of this package should be available from
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>
;;
;;
;;; Commentary:
;;
;; Communication with a slave CMUCL using the WIRE protocol. We don't
;; implement the remote-object aspects of the protocol, so the
;; marshaling business is pretty simple.
;;
;; A wire is represented by a buffer, which has an associated network
;; stream with the slave CMUCL. The slave CMUCL has to say
;;
;;  (wire:create-request-server port)



;;; Code:

(require 'cl)

(defconst +wire-op/funcall+  6)
(defconst +wire-op/number+   7)
(defconst +wire-op/string+   8)
(defconst +wire-op/symbol+   9)
(defconst +wire-op/cons+     13)


(put 'wire-error
     'error-conditions
     '(error wire-error))


;; can be define a printer for wires, that would show whether the
;; process was active, and the current position?
(defstruct wire
  process
  (position 1))

;; the buffer will contain all wired output from the slave CMUCL
(defun wire-connect-to-remote-server (host port)
  (let ((buf (get-buffer-create " *cmucl-wire*"))
        (process nil))
    (condition-case nil
        (progn
          (setq process (open-network-stream "CMUCL" buf host port))
          (set-process-coding-system process nil)
          (make-wire :process process))
      (file-error
       (kill-buffer buf)
       (signal 'wire-error (list "Can't connect to wire server"))))))

(defun wire-close (wire)
  (and wire
       (eq 'open (process-status (wire-process wire)))
       (delete-process (wire-process wire)))
  (let ((buf (get-buffer " *cmucl-wire*")))
    (when buf (kill-buffer buf)))
  (when wire (setf (wire-process wire) nil)))


;; returns a single value. Note that we send the function name in
;; uppercase, because it doesn't go through the CL reader. 
(defun wire-remote-eval (wire string)
  (wire-output-funcall wire 'EVALUATE string)
  (wire-force-output wire)
  (let ((status (wire-get-number wire))
        (condition (wire-get-string wire))
        (result (wire-get-string wire)))
    ;; for efficiency, we empty the wire buffer when it gets very large
    (when (> (wire-position wire) 100000)
      (save-excursion
        (set-buffer (process-buffer (wire-process wire)))
        (erase-buffer)
        (setf (wire-position wire) 0)))
    (values status condition result)))


;; === low-level encoding issues === 

(defun wire-force-output (wire)
  (accept-process-output (wire-process wire) 1))

(defun wire-output-byte (wire byte)
  (process-send-string (wire-process wire) (char-to-string byte)))

;; use a separate variable for pos, in case input arrives during
;; the execution of this function
(defun wire-get-byte (wire)
  (let ((process (wire-process wire))
        (pos (wire-position wire))
        (inhibit-eol-conversion t))
         (save-excursion
      (set-buffer (process-buffer process))
      (incf (wire-position wire))
      (when (null (char-after pos))
        (accept-process-output process 1))
      (let ((byte (char-after pos)))
        (unless byte
          (signal 'wire-error
                  (list "Can't communicate with %s" (wire-process wire))))
        byte))))

(defun wire-output-number (wire number &optional length)
  (let* ((length (or length 4))
         (process (wire-process wire))
         (str (make-string length 0))
         (i (- length 1)))
    (while (>= i 0)
      (aset str i (% number 256))
      (setq number (floor number 256))
      (decf i))
    (process-send-string process str)))

(defun wire-get-number (wire)
  (do ((i 4 (- i 1))
       (accum 0))
      ((zerop i) accum)
    (setf accum (+ (* 256 accum) (wire-get-byte wire)))))

;; Strings are represented by the length as a number, followed by the
;; bytes of the string. Assume that we're dealing with a "simple"
;; string.
(defun wire-output-string (wire string)
  (let ((process (wire-process wire))
        (length (length string)))
    (wire-output-number wire length)
    (process-send-string process string)))

;; the first four octets read are the size of the string.
(defun wire-get-string (wire)
  (let ((count (wire-get-number wire)))
    (when (minusp count)
      (error "Number overflow in wire-get-string"))
    (do ((i 0 (+ i 1))
         (chars (make-string count ?.)))
        ((= i count) chars)
      (aset chars i (wire-get-byte wire)))))

(defun wire-output-object (wire object)
  (typecase object
    (integer
     (wire-output-byte wire +wire-op/number+)
     (wire-output-number wire object))
    (string
     (wire-output-byte wire +wire-op/string+)
     (wire-output-string wire object))
    (symbol
     (wire-output-byte wire +wire-op/symbol+)
     (wire-output-string wire (symbol-name object))
     (wire-output-string wire "SKANK"))
    (cons
     (wire-output-byte wire +wire-op/cons+)
     (wire-output-object wire (car object))
     (wire-output-object wire (cdr object)))
    (t
     (error "Cannot output objects of type %s across a wire."
            (type-of object))))
  nil)

;; send the function and its arguments down the wire as a funcall. 
(defun wire-output-funcall (wire function &rest args)
  (let ((num-args (length args)))
    (wire-output-byte wire +wire-op/funcall+)
    (wire-output-byte wire num-args)
    (wire-output-object wire function)
    (dolist (arg args)
      (wire-output-object wire arg))
    nil))



(provide 'cmucl-wire)

;;; cmucl-wire.el ends here
