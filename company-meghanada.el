;;; company-meghanada.el --- Company support for meganada -*- coding: utf-8; lexical-binding: t; -*-

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
;; The `company-meghanada' is a `company' backend that
;; will serve completion candidates asynchronously.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pcase))

(require 'company)
(require 'company-template)
(require 'thingatpt)
(require 'meghanada)
(require 'yasnippet)

(defgroup company-meghanada nil
  "Company-mode completion backend for Meghanada."
  :group 'meghanada)

(defcustom company-meghanada-show-annotation t
  "Show an annotation inline with the candidate."
  :group 'company-meghanada
  :type 'boolean)

(defcustom company-meghanada-auto-import t
  "Add new package class autoimport."
  :group 'company-meghanada
  :type 'boolean)

(defcustom company-meghanada-prefix-length nil
  "Start completion prefix-length."
  :group 'company-meghanada
  :type 'integer)

(defconst company-meghanada--trigger "^package \\|^import \\w\\{%d,\\}\\|new \\w\\{%d,\\}\\|@\\w\\{%d,\\}\\|(.*)\\.\\w*\\|[A-Za-z0-9]+\\.\\w*\\|\\.\\w*")

(defvar company-meghanada-trigger-regex nil)

;;;###autoload
(defun meghanada-company-enable ()
  "Enable auto completion with company."
  (company-mode t)
  (if company-meghanada-prefix-length
      (set (make-local-variable 'company-minimum-prefix-length) company-meghanada-prefix-length)
    (set (make-local-variable 'company-meghanada-prefix-length) company-minimum-prefix-length))
  (setq company-meghanada-trigger-regex (format company-meghanada--trigger
                                                company-meghanada-prefix-length
                                                company-meghanada-prefix-length
                                                company-meghanada-prefix-length
                                                company-meghanada-prefix-length))
  (add-to-list 'company-backends '(company-meghanada :separate company-dabbrev-code))
  (yas-minor-mode t)
  (make-local-variable 'yas-minor-mode-map))

(defun make-icon-hash (type)
  (let ((kind-val (pcase type
                    ("VARIABLE" 6)
                    ("METHOD" 2)
                    ("CONSTRUCTOR" 2)
                    ("FIELD" 5)
                    ("CLASS" 22)
                    ("IMPORT" 10)
                    ("PACKAGE" 10)))
        (ht (make-hash-table :test 'equal)))
    (puthash "kind" kind-val ht)
    ht))

(defun company-meghanada--to-candidate (result)
  (mapcar (lambda (candidate)
            (propertize (nth 1 candidate)
                        'desc
                        (nth 2 candidate)
                        'meta
                        (nth 3 candidate)
                        'type
                        (nth 0 candidate)
                        'return-type
                        (nth 4 candidate)
                        'extra
                        (nth 5 candidate)
                        'lsp-completion-item
                        (make-icon-hash (nth 0 candidate)))) result))

(defun company-meghanada--to-candidates (output)
  (when (> (length output) 0)
    (company-meghanada--to-candidate output)))

(defun company-meghanada--autocomplete-callback (output &rest args)
  (let ((callback (car args)))
    (funcall callback (company-meghanada--to-candidates output))))

(defvar meghanada--sp-prefix nil)

(defun company-meghanada--candidates (prefix)
  (let ((prefix (if meghanada--sp-prefix
                    meghanada--sp-prefix
                  prefix)))
    (when prefix
      (cons :async
            #'(lambda (callback)
                (meghanada-autocomplete-prefix-async
                 (format "\"%s\"" prefix)
                 (list #'company-meghanada--autocomplete-callback callback)))))))

(defun meghanada--search-method-caller ()
  (save-excursion
    (backward-list)
    (forward-char -1)
    (get-text-property (point) 'return-type)))

(defun meghanada--search-access-caller ()
  (save-excursion
    (search-backward "." nil t)
    (backward-word)
    (if (= (meghanada--what-word) "this")
        "this"
      (get-text-property (point) 'return-type))))

(defun meghanada--last-is-paren ()
  (save-excursion
    (search-backward "." nil t)
    (forward-char -1)
    (while (or
            (string= (char-to-string (char-after)) " ")
            (string= (char-to-string (char-after)) "\n"))
      (forward-char -1))
    (string= (char-to-string (char-after)) ")")))

(defun meghanada--last-statement-position ()
  ;; Get position of nearest ";" and "{" character before current line, And
  ;; assume this is the last statement position
  (max (save-excursion (or (search-backward ";" nil t) 0)
                       (save-excursion (or (search-backward "{" nil t) 0)))))

(defun meghanada--last-is-assignment (lap)
  ;; Whether is in an assignment statement, if in assignment statement, variable
  ;; type or name need to be send to backend to do smart completion
  (save-excursion
    (search-backward "=" lap t) 0))

(defun meghanada--variable-type-or-name (lap)
  ;; Get assignment statement variable type or variable name, then send it to
  ;; backend for smart completion
  (save-excursion
    (search-backward "=" lap t) (backward-word 2)
    (if (< (point) lap)
        (forward-word 2))
    (meghanada--what-word)))

(defun meghanada--grab-symbol-cons ()
  (let ((symbol (company-grab-symbol))
        (re company-meghanada-trigger-regex))

    (setq meghanada--sp-prefix nil)
    (when symbol
      (save-excursion
        (if (looking-back re (line-beginning-position) t)
            (let* ((match (match-string 0))
                   (keyword
                    (cond
                     ((string-prefix-p "package" match) "*package")

                     ((string-prefix-p "import " match)
                      (concat "*" (replace-regexp-in-string " " ":" match)))

                     ((string-prefix-p "new" match)
                      (concat "*" (replace-regexp-in-string " " ":" match)))

                     ((string-match "\)\\.\\(\\w*\\)$" match)
                      (let ((prefix (match-string 1 match))
                            (rt (meghanada--search-method-caller)))
                        (if rt
                            (concat "*method:" rt "#" prefix)
                          (concat "*method#" prefix))))

                     ((string-match "\\.\\(\\w*\\)$" match)
                      (let* ((prefix (match-string 1 match))
                             (paren (meghanada--last-is-paren))
                             (lap (meghanada--last-statement-position))
                             (assign (meghanada--last-is-assignment lap))
                             (vt (if assign
                                     (ignore-errors (meghanada--variable-type-or-name lap))))
                             (rt (if paren
                                     (ignore-errors (meghanada--search-method-caller))
                                   (ignore-errors (meghanada--search-access-caller))))
                             (sym (if paren
                                      (save-excursion
                                        (backward-list)
                                        (forward-char -1)
                                        (meghanada--what-word))
                                    (save-excursion
                                      (search-backward "." nil t)
                                      (backward-word)
                                      (meghanada--what-word)))))
                        (if assign
                            (if rt (concat "*method:" rt "*" vt "#" prefix)
                              (concat "*" sym "*" vt "#" prefix))
                          (if rt (concat "*method:" rt "#" prefix)
                            (concat "*" sym "#" prefix))
                          )))

                     ((string-match "\\(.*\\)\\.\\(\\w*\\)$" match)
                      (let* ((var (match-string 1 match))
                             (prefix (match-string 2 match)))
                        (concat "*" var "#" prefix)))
                     (t match))))

              (setq meghanada--sp-prefix keyword)
              (cons symbol t))
          symbol)))))

;; (defun company-meghanada--prefix ()
;;   (company-grab-symbol-cons "\\(package \\)\\|\\(news \\)\\|[A-Za-z0-9)]+\\."))

(defun company-meghanada--prefix ()
  (meghanada--grab-symbol-cons))

(defun company-meghanada--in-num-literal-p ()
  "Returns t if point is in a numeric literal."
  (let ((word (company-grab-word)))
    (when word
      (string-match-p "^0x\\|^[0-9]+" word))))

(defun company-meghanada--annotation (arg)
  (let ((meta (get-text-property 0 'meta arg)))
    (when (string-match "\\(([^-]*\\)" meta)
      (substring meta (match-beginning 1) (match-end 1)))))

(defun company-meghanada--typep-annotation (arg)
  (let ((desc (get-text-property 0 'desc arg)))
    (when (string-match "\\(<[^-]*\\)" desc)
      (substring desc (match-beginning 1) (match-end 1)))))

(defun company-meghanada--post-class (arg)
  (let ((meta (get-text-property 0 'meta arg))
        (anno (company-meghanada--typep-annotation arg))
        (return-t (get-text-property 0 'return-type arg)))
    (unless (meghanada--import-exists-p meta)
      (if company-meghanada-auto-import
          (meghanada--add-import meta (current-buffer))
        (when (y-or-n-p (format "Add import %s ? " (meghanada--import-name meta)))
          (meghanada--add-import meta (current-buffer)))))

    (save-excursion
      (forward-char -1)
      (set-text-properties
       (beginning-of-thing 'symbol)
       (end-of-thing 'symbol)
       (list 'class t 'return-type return-t 'meta meta 'type 'class)))

    (if (and meghanada--sp-prefix
             (or (string-prefix-p "*new" meghanada--sp-prefix)
                 (string-prefix-p "@" meghanada--sp-prefix)))
        (if anno
            ;; complete diamond op. like a new HashMap<>()
            (progn
              (insert "<>()")
              (backward-char 3))
          (progn
              (insert "()")
              (backward-char 1)))
      (when anno
        (insert anno)
        (company-template-c-like-templatify anno)))))

(defun company-meghanada--post-method (arg)
  (let* ((meta (get-text-property 0 'meta arg))
         (desc (get-text-property 0 'desc arg))
         (anno (company-meghanada--annotation arg))
         (return-t (get-text-property 0 'return-type arg))
         (extra (split-string (get-text-property 0 'extra arg))))
    (when return-t
      (save-excursion
        (forward-char -1)
        (set-text-properties
         (beginning-of-thing 'symbol)
         (end-of-thing 'symbol)
         (list 'return-type return-t 'meta meta 'type 'method))))
    (when anno
      (company-template-c-like-templatify anno)
      (when (and
             (> (length extra) 1)
             (string= "static-import" (car extra)))
        (let* ((class (nth 1 extra))
               (imp (format "%s#%s" class arg)))
          (if company-meghanada-auto-import
              (meghanada--add-import imp (current-buffer))
            (when (y-or-n-p
                   (format "Add import %s ? " (meghanada--import-name class)))
              (meghanada--add-import imp (current-buffer)))))))))

(defun company-meghanada--post-field (arg)
  (let ((meta (get-text-property 0 'meta arg))
        (anno (company-meghanada--annotation arg))
        (return-t (get-text-property 0 'return-type arg))
        (extra (split-string (get-text-property 0 'extra arg))))
    (when return-t
      (save-excursion
        (forward-char -1)
        (set-text-properties
         (beginning-of-thing 'symbol)
         (end-of-thing 'symbol)
         (list 'return-type return-t 'meta meta 'type 'field)))
      (when (and
             (> (length extra) 1)
             (string= "static-import" (car extra)))
        (let* ((class (nth 1 extra))
               (imp (format "%s#%s" class arg)))
          (if company-meghanada-auto-import
              (meghanada--add-import imp (current-buffer))
            (when (y-or-n-p
                   (format "Add import %s ? " (meghanada--import-name class)))
              (meghanada--add-import imp (current-buffer)))))))))

(defun company-meghanada--post-var (arg)
  (let ((meta (get-text-property 0 'meta arg))
        (anno (company-meghanada--annotation arg))
        (return-t (get-text-property 0 'return-type arg)))
    (when return-t
      (save-excursion
        (forward-char -1)
        (set-text-properties
         (beginning-of-thing 'symbol)
         (end-of-thing 'symbol)
         (list 'return-type return-t 'meta meta 'type 'var))))))

(defun company-meghanada--post-completion (arg)
  (let ((type (intern (get-text-property 0 'type arg)))
        (meta (get-text-property 0 'meta arg))
        (desc (get-text-property 0 'desc arg))
        (anno (company-meghanada--annotation arg)))
    (meghanada-autocomplete-resolve-async
     type
     arg
     desc
     #'identity)
    (pcase type
      ;; completion class
      (`CLASS (company-meghanada--post-class arg))
      ;; completion field
      (`FIELD (company-meghanada--post-field arg))
      ;; completion method
      (`METHOD (company-meghanada--post-method arg))
      ;; completion var
      (`VAR (company-meghanada--post-var arg))
      ;; completion const
      (`CONSTRUCTOR (progn (insert "()") (backward-char 1)))
      ;; completion const
      (`IMPORT (progn
                 (backward-word)
                 (insert meta)
                 (insert ";")
                 (delete-region (point) (+ (point) (length arg))))))))

(defun company-meghanada (command &optional arg &rest ignored)
  (cl-case command
    (prefix (and (meghanada-alive-p)
                 (not (company-in-string-or-comment))
                 (not (company-meghanada--in-num-literal-p))
                 (or (company-meghanada--prefix) 'stop)))
    (candidates (company-meghanada--candidates arg))
    (meta (get-text-property 0 'meta arg))
    (annotation (when company-meghanada-show-annotation
                  (concat " " (get-text-property 0 'desc arg))))
    (ignore-case t)
    (sorted t)
    (no-cache
     (unless (and (string= "prefix" meghanada-completion-matcher) (string= "prefix" meghanada-class-completion-matcher))
       t))
    (require-match 'never)
    (post-completion
     (company-meghanada--post-completion arg))))

(provide 'company-meghanada)

;;; company-meghanada.el ends here
