;;; flycheck-meghanada.el --- Flycheck support for meghanada -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2017 Yutaka Matsubara
;; License: http://www.gnu.org/licenses/gpl.html

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `flycheck-meghanada' provides `flycheck-chcker' for java.

;;; Code:

(eval-when-compile
  (require 'pcase))

(require 'flycheck)
(require 'meghanada)
(require 'cl-lib)

(defgroup flycheck-meghanada nil
  "Meghanada mode's flycheck checker."
  :group 'meghanada)

(defcustom flycheck-meghanada-enable-live-check t
  "If true, check the buffer immediately after a new line or a short time."
  :group 'flycheck-meghanada
  :type 'boolean)

(defcustom flycheck-meghanada-java-encoding 'utf-8
  "The default java compilation encoding."
  :group 'flycheck-meghanada
  :type 'symbol)

(defun flycheck-meghanada--build-error (diagnostic checker buffer)
  (let* ((severity (intern (nth 2 diagnostic))))
    (when (memq severity '(NOTE MANDATORY_WARNING WARNING ERROR FATAL OTHER))
      (flycheck-error-new-at
       (nth 0 diagnostic)
       (nth 1 diagnostic)
       (pcase severity
         (`NOTE 'info)
         (`WARNING 'warning)
         (`MANDATORY_WARNING 'warning)
         ((or `ERROR `FATAL `OTHER) 'error))
       (nth 3 diagnostic)
       :checker checker
       :buffer buffer
       :filename (buffer-file-name buffer)))))

(defun flycheck-meghanada--build-errors (buffer result callback checker)
  (mapc (lambda (r)
          (let ((file (nth 0 r))
                (diagnostics (nth 1 r)))

            (with-current-buffer (find-file-noselect file)
              (let* ((file-buf (current-buffer))
                     (errors (mapcar (lambda (d)
                                       (flycheck-meghanada--build-error d checker file-buf))
                                     diagnostics)))
                (when (eq file-buf buffer)
                  (funcall callback 'finished (delq nil errors)))))))
        result))

(defun flycheck-meghanada--decode-diagnostics (diagnostics)
  (let (result result-errors file errors err msg)
    (setq result '())
    (dolist (buffer-errors diagnostics)
      (setq file (car buffer-errors))
      (setq errors (car (cdr buffer-errors)))
      (setq result-errors '())
      (dolist (err errors)
        (setq msg (decode-coding-string
                   (encode-coding-string (car (last err)) flycheck-meghanada-java-encoding)
                   'utf-8))
        (add-to-list 'result-errors (append (subseq err 0 -1) (list msg)) t))
      (add-to-list 'result (list file result-errors) t))
    result))

(defun flycheck-meghanada--callback (result &rest args)
  (let* ((callback (nth 0 args))
         (checker (nth 1 args))
         (buffer (nth 2 args))
         (type (car result))
         (diagnostics (car (cdr result))))
    (pcase type
      (`fatal  (funcall callback 'errored '("Meghanada diagnostics fatal error")))
      (`success (funcall callback 'finished nil))
      (`error (flycheck-meghanada--build-errors buffer (if (eq flycheck-meghanada-java-encoding 'utf-8) diagnostics (flycheck-meghanada--decode-diagnostics diagnostics)) callback checker))
      (_ (progn
           (message "WARN not match type")
           (funcall callback 'finished nil))))))

(defun flycheck-meghanada--start (checker callback)
  (let ((buffer (current-buffer)))
    (meghanada-diagnostics-async
     (list #'flycheck-meghanada--callback callback checker buffer))))

(defun flycheck-meghanada-live--start (checker callback)
  (let ((buffer (current-buffer)))
    (meghanada-diagnostic-string-async
     (list #'flycheck-meghanada--callback callback checker buffer))))

(defun flycheck-meghanada-after-hook ()
  (let* ((errors flycheck-current-errors)
         (current (current-buffer))
         (new-error
          (cl-remove-if-not
           (lambda(e)
             (let ((err-buf (flycheck-error-buffer e)))
               (eq err-buf current))) errors)))
    (setq flycheck-current-errors new-error)))

(flycheck-define-generic-checker 'meghanada
  "A syntax checker for java, using meghanada-mode."
  :start #'flycheck-meghanada--start
  :modes '(java-mode)
  :predicate (lambda ()
               (and (meghanada-alive-p)
                    (flycheck-buffer-saved-p))))

(flycheck-define-generic-checker 'meghanada-live
  "A syntax checker for java, using meghanada-mode."
  :start #'flycheck-meghanada-live--start
  :modes '(java-mode)
  :predicate (lambda ()
               (and (meghanada-alive-p)
                    (not (flycheck-buffer-empty-p))))
  :verify (lambda (_)
	    (list
	     (flycheck-verification-result-new
	      :label "Meghanada server"
	      :message (if (meghanada-alive-p) "Running" "Not Running")
	      :face (if (meghanada-alive-p) 'success '(bold error))))))

;;;###autoload
(defun meghanada-flycheck-enable ()
  "Enable flycheck for meghanada-mode."
  (if flycheck-meghanada-enable-live-check
      (add-to-list 'flycheck-checkers 'meghanada-live)
    (add-to-list 'flycheck-checkers 'meghanada)))

(provide 'flycheck-meghanada)

;;; flycheck-meghanada.el ends here
