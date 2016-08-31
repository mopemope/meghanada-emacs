;;; flycheck-meghanada.el --- Flycheck support for Meghanada -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; Java support for Flycheck, using Meghanada Mode.

;;; Code:

(require 'flycheck)
(require 'meghanada-mode)

(eval-when-compile
  (require 'pcase))

(defgroup flycheck-meghanada nil
  "meghanada mode's flycheck checker."
  :group 'meghanada)

(defun flycheck-meghanada--build-error (diagnostic checker buffer)
  (let ((severity (intern (nth 2 diagnostic))))
    (when (memq severity '(NOTE WARNING ERROR FATAL))
      (flycheck-error-new-at
       (nth 0 diagnostic)
       (nth 1 diagnostic)
       (pcase severity
         (`NOTE 'info)
         (`WARNING 'warning)
         ((or `ERROR `FATAL) 'error))
       (nth 3 diagnostic)
       :checker checker
       :buffer buffer))))

(defun flycheck-meghanada--build-errors (result checker buffer)
  (mapcar (lambda (r)
            (flycheck-meghanada--build-error r checker buffer)) result))

(defun flycheck-meghanada--callback (output &rest args)
  (let* ((callback (nth 0 args))
         (checker (nth 1 args))
         (buffer (nth 2 args))
         (result (read output))
         (type (car result)))
    (pcase type
      (`fatal  (funcall callback 'errored '("Meghanada diagnostics fatal error")))
      (`success (funcall callback 'finished nil))
      (`error (let* ((errors (flycheck-meghanada--build-errors (cdr result) checker buffer)))
                (funcall callback 'finished (delq nil errors))))
      (t (progn
           (message "WARN not match type")
           (funcall callback 'finished nil))))))

(defun flycheck-meghanada--start (checker callback)
  (let ((buffer (current-buffer)))
    (meghanada-diagnostics-async
     (list #'flycheck-meghanada--callback callback checker buffer))))

(flycheck-define-generic-checker 'meghanada
  "A syntax checker for Java, using meghanada mode."
  :start #'flycheck-meghanada--start
  :modes '(java-mode meghanada-mode)
  :predicate (lambda ()
               (and (meghanada-alive-p)
                    (flycheck-buffer-saved-p))))

(add-to-list 'flycheck-checkers 'meghanada)

;;;###autoload
(defun flycheck-meghanda-setup ()
  "Setup Flycheck Meghanada.
Add `meghanada' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'meghanada))


(provide 'flycheck-meghanada)
;;; flycheck-meghanada.el ends here
