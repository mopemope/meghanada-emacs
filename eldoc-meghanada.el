;;; el-doc-meghanada.el --- eldoc for meganada -*- coding: utf-8; lexical-binding: t; -*-

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
;; `eldoc-meghanada' provides eldoc for `meghanada-mode'.It shows type information
;; for variable, method, class and current argument position of function.

;;; Code:

(require 'cl-lib)

(require 'eldoc)
(require 'meghanada)
(require 'thingatpt)

(defgroup eldoc-meghanada nil
  "Eldoc for meghanada."
  :group 'meghanada)

(defun eldoc-meghanada--call-server (buf line col sym)
  (let* ((decl (meghanada--send-request-sync
                "sd"
                buf
                line
                col
                (format "\"%s\"" sym)))
         (type (nth 0 decl))
         (name (nth 1 decl))
         (signature (nth 2 decl))
         (arg-index (nth 3 decl)))
    (cl-case type
      (method
       (format "%s: %s"
               (propertize name 'face 'font-lock-function-name-face)
               signature))
      (class
       (format "%s: %s"
               (propertize name 'face 'font-lock-type-face)
               signature))
      (field
       (format "%s: %s"
               (propertize name 'face 'font-lock-variable-name-face)
               signature))
      (var
       (format "%s: %s"
               (propertize name 'face 'font-lock-variable-name-face)
               signature))
      (other ""))))

(defun eldoc-meghanada--documentation-function ()
  (when (and meghanada--client-process (process-live-p meghanada--client-process))
    (let* ((buf (buffer-file-name))
           (line (meghanada--what-line))
           (col (meghanada--what-column))
           (raw-sym (meghanada--what-symbol))
           (sym (if raw-sym
                    (string-trim raw-sym)
                  raw-sym))
           (meta (get-text-property (point) 'meta))
           (type (get-text-property (point) 'type)))
      (when (and sym (> (length sym) 0))
        (eldoc-meghanada--call-server buf line col sym)))))

;; (if meta
;;     (cl-case type
;;       (method
;;        (format "%s: %s"
;;                (propertize sym 'face 'font-lock-function-name-face)
;;                meta))
;;       (class
;;        (format "%s: %s"
;;                (propertize sym 'face 'font-lock-type-face)
;;                meta))
;;       (field
;;        (format "%s: %s"
;;                (propertize sym 'face 'font-lock-variable-name-face)
;;                meta))
;;       (var
;;        (format "%s: %s"
;;                (propertize sym 'face 'font-lock-variable-name-face)
;;                meta))
;;       (other ""))
      ;;   (when sym
      ;;     (eldoc-meghanada--call-server buf line col sym)))


;;;###autoload
(defun eldoc-meghanada-setup ()
  "Set up eldoc function and enable 'eldoc-mode'."
  (interactive)
  (setq-local eldoc-documentation-function #'eldoc-meghanada--documentation-function)
  (eldoc-mode +1))

;;;###autoload
(defun meghanada-eldoc-enable ()
  "Enable eldoc for meghanada-mode."
  (eldoc-meghanada-setup))

(provide 'eldoc-meghanada)

;;; go-eldoc.el ends here
