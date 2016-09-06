;;; company-meghanada.el --- Company support for Meganada -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Better new java-mode for GNU Emacs 24.
;;

;;; Code:

(require 'company-template)

(eval-when-compile
  (require 'cl))

(require 'meghanada-mode)
(require 'company)

(defgroup company-meghanada nil
  "Company-mode completion back-end for Meghanada."
  :group 'company)

(defcustom company-meghanada-show-annotation t
  "Show an annotation inline with the candidate."
  :group 'company-meghanada
  :type 'boolean)

(defcustom company-meghanada-auto-import t
  "Add new package class autoimport."
  :group 'company-meghanada
  :type 'boolean)

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
                        (nth 4 candidate))) result))

(defun company-meghanada--to-candidates (output)
  (when (> (length output) 0)
    (company-meghanada--to-candidate (read output))))

;; (defun company-meghanada--candidates-prefix (prefix)
;;   (if (string= prefix "")
;;       (save-excursion
;;         (cond
;;          ((= ?\. (preceding-char))
;;           (progn
;;             (forward-char (- 1))
;;             (concat "@" (thing-at-point 'symbol))))
;;          ((= ?\s (preceding-char))
;;           (progn
;;             (forward-char (- 1))
;;             (concat "@" (thing-at-point 'symbol))))))
;;     prefix))

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
                ;; (message (format "send-prefix prefix '%s'" prefix))
                (meghanada-autocomplete-prefix-async
                 prefix
                 (list #'company-meghanada--autocomplete-callback callback)))))))

(defun meghanada--search-return-type ()
  (save-excursion
    (backward-sexp)
    (forward-char -1)
    (get-text-property (point) 'return-type)))

(defun meghanada--grab-symbol-cons ()
  (let ((symbol (company-grab-symbol))
        (re "^package \\|new \\w\\{3,\\}\\|(.*)\\.\\w*\\|[A-Za-z0-9]+\\.\\w*"))
    (setq meghanada--sp-prefix nil)
    (when symbol
      (save-excursion
        (if (looking-back re (line-beginning-position) t)
            (let* ((match (match-string 0))
                   (keyword
                    (cond

                     ((string-prefix-p "package" match) "*package")

                     ((string-prefix-p "new" match)
                      (concat "*" (replace-regexp-in-string " " ":" match)))

                     ((string-suffix-p ")." match)
                      (let ((rt (meghanada--search-return-type)))
                        (if rt
                            (concat "*method:" rt)
                          "*method")))

                     ((string-suffix-p "." match)
                      (concat "*" (replace-regexp-in-string "\\." "" match)))

                     (t match))))
              ;; (message (format "match:%s send-keyword:%s" match keyword))
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
        (anno (company-meghanada--typep-annotation arg)))
    (unless (meghanada--import-exists-p meta)
      (if company-meghanada-auto-import
          (meghanada--add-import meta)
        (when (y-or-n-p (format "Add import %s ?" (meghanada--import-name meta)))
          (meghanada--add-import meta))))

    (save-excursion
      (forward-char -1)
      (set-text-properties
       (beginning-of-thing 'symbol) (end-of-thing 'symbol)
       (list 'class t 'return-type meta 'meta meta)))
    (if (and meghanada--sp-prefix (string-prefix-p "*new" meghanada--sp-prefix))
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
  (let ((meta (get-text-property 0 'meta arg))
        (anno (company-meghanada--annotation arg))
        (return-t (get-text-property 0 'return-type arg)))
    (when return-t
      (save-excursion
        (forward-char -1)
        (set-text-properties
         (beginning-of-thing 'symbol) (end-of-thing 'symbol)
         (list 'return-type return-t 'meta meta))))
    (when anno
      (insert anno)
      (company-template-c-like-templatify anno))))

(defun company-meghanada--post-completion (arg)
  (let ((type (intern (get-text-property 0 'type arg))))

    (pcase type
      ;; completion class
      (`CLASS (company-meghanada--post-class arg))
      ;; completion method
      (`METHOD (company-meghanada--post-method arg))
      ;; completion const
      (`CONSTRUCTOR (progn (insert "()") (backward-char 1))))))

(defun company-meghanada (command &optional arg &rest ignored)
  (case command
    (prefix (and (derived-mode-p 'meghanada-mode)
                 (not (company-in-string-or-comment))
                 (not (company-meghanada--in-num-literal-p))
                 (or (company-meghanada--prefix) 'stop)))
    (candidates (company-meghanada--candidates arg))
    (meta (get-text-property 0 'meta arg))
    (annotation (when company-meghanada-show-annotation
                  (concat " " (get-text-property 0 'desc arg))))
    (sorted t)
    (no-cache t)
    (post-completion (company-meghanada--post-completion arg))))

(defun meghanada-grab-symbol-test ()
  (interactive)
  (message (format "%s" (meghanad--grab-symbol-cons))))

(defun meghanada-prop-test ()
  (interactive)
  (let ((pos (next-property-change (point))))
    (goto-char pos)
    (message (format "prop:%s" (get-text-property (point) 'return-type)))))

(provide 'company-meghanada)
;;; company-meghanada.el ends here
