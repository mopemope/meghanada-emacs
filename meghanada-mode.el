;;; meghanada-mode.el --- A better new java-mode -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Better new java-mode for GNU Emacs 24.
;;

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'imenu)

;;
;; Customizable variables
;;

(defgroup meghanada nil
  "Java minor mode powered by meghanada."
  :group 'java)

(defcustom meghanada-host "127.0.0.1"
  "Meghanada server host address."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-port 55555
  "Meghanada server port."
  :group 'meghanada
  :type 'int)

(defcustom meghanada-debug nil
  "Meghanada server debug mode."
  :group 'meghanada
  :type 'boolean)


(defconst meghanada--eot "\n;;EOT\n")
(defconst meghanada--junit-buf-name "*meghanada-junit*")
(defconst meghanada--task-buf-name "*meghanada-task*")

;;
;; utility
;;

(defun meghanada--what-line ()
  "TODO: FIX DOC ."
  (format-mode-line "%l"))

(defun meghanada--what-column ()
  "TODO: FIX DOC ."
  (number-to-string (current-column)))

(defun meghanada--what-word ()
  "TODO: FIX DOC ."
  (thing-at-point 'word))

(defmacro meghanada--without-narrowing (&rest body)
  "TODO: FIX DOC BODY."
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))

(defun meghanada--remove-eot (out)
  "TODO: FIX DOC OUT ."
  (replace-regexp-in-string meghanada--eot "" out))

(defun meghanada--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun meghanada--line-column-to-point (line column)
  (save-excursion
    (meghanada--goto-line line)
    (forward-char (1- column))
    (point)))

;;
;; meghanada-server process management.
;;

(defvar meghanada--server-jar nil)
(defvar meghanada--server-process nil)
(defvar meghanada--server-buffer "*meghanada-server-log*")
(defvar meghanada--server-pending nil)

;; curl -L "https://dl.bintray.com/mopemope/meghanada/meghanada.jar" -o meghanada.jar
(defun meghanada-install-server ()
  "Install meghanada-server's jar file from bintray ."
  (interactive)
  (let ((d (expand-file-name "~/.meghanada"))
        (dest (expand-file-name "~/.meghanada/meghanada.jar")))
    (shell-command (format "mkdir -p %s" d))
    (call-process "curl" nil nil t "-L" "https://dl.bintray.com/mopemope/meghanada/meghanada.jar" "-o" dest)
    (message "SUCCESS install meghanada-server. Please restart emacs")))

(defun meghanada--locate-server-jar ()
  "TODO: FIX DOC ."
  (let ((jar (expand-file-name "~/.meghanada/meghanada.jar")))
    (if (file-exists-p jar)
        jar
      (message "missing meghanada.jar"))))

(defun meghanada--start-server-process ()
  "TODO: FIX DOC ."
  (when (setq meghanada--server-jar (or meghanada--server-jar
                                        (meghanada--locate-server-jar)))
    (let ((process-connection-type nil)
          (process-adaptive-read-buffering nil)
          process)
      (message "Meghanada-Server Starting ...")
      (setq process
            (start-process-shell-command
             "meghanada-server"
             meghanada--server-buffer
             (format "java -ea -XX:+TieredCompilation -XX:+UseConcMarkSweepGC -XX:SoftRefLRUPolicyMSPerMB=50 -Xverify:none -Xms256m -Xmx2G -Dfile.encoding=UTF-8 -jar %s -p %d"
                     (shell-quote-argument meghanada--server-jar)
                     meghanada-port)))
      (buffer-disable-undo meghanada--server-buffer)
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process 'meghanada--server-process-sentinel)
      (set-process-filter process 'meghanada--server-process-filter)
      (message "Meghanada-Server Started")
      process)))

(defun meghanada--get-server-process-create ()
  "TODO: FIX DOC ."
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      meghanada--server-process
    (setq meghanada--server-process (meghanada--start-server-process))))

(defun meghanada--server-process-sentinel (process event)
  "TODO: FIX DOC PROCESS EVENT ."
  (unless (process-live-p process)
    (setq meghanada--server-process nil)
    (message "meghanada-server process stopped")))

(defun meghanada--server-process-filter (process output)
  "TODO: FIX DOC PROCESS OUTPUT ."
  (let ((pbuf (process-buffer process)))
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))
          (if moving
              (goto-char (process-mark process)))
          (when (string-match "Start server" output)
            (message "Server waiting client connection ...")
            (when meghanada--server-pending
              (funcall meghanada--server-pending)
              (setq meghanada--server-pending nil))))))))

(defun meghanada-server-start ()
  "TODO: FIX DOC ."
  (interactive)
  (meghanada--get-server-process-create))

(defun meghanada-server-kill ()
  "TODO: FIX DOC ."
  (interactive)
  (when (and meghanada--server-process (process-live-p meghanada--server-process))
    (kill-process meghanada--server-process)
    (setq meghanada--server-process nil)))


;;
;; meghanada-client process management.
;;

(defvar meghanada--client-process nil)
(defvar meghanada--client-buffer "*meghanada-client*")
(defvar meghanada--connect-host meghanada-host)
(defvar meghanada--connect-port meghanada-port)
(defvar meghanada--client-pending nil)

(defvar meghanada--task-client-process nil)
(defvar meghanada--task-buffer nil)

(defun meghanada--start-client-process ()
  "TODO: FIX DOC ."
  (let (process)
    (message "meghanada-client process start ...")
    (setq process
          (make-network-process
           :name "meghanada-client"
           :buffer meghanada--client-buffer
           :family 'ipv4
           :host meghanada--connect-host
           :service meghanada--connect-port
           :noquery t
           :sentinel 'meghanada--client-process-sentinel
           :filter 'meghanada--client-process-filter))
      (buffer-disable-undo meghanada--client-buffer)
      (message "Meghanada Ready")
      (setq meghanada--task-client-process (meghanada--start-task-client-process))
      process))

(defun meghanada--start-task-client-process ()
  "TODO: FIX DOC ."
  (make-network-process
   :name "meghanada-task-client"
   :buffer "*meghanada-task-client*"
   :family 'ipv4
   :host meghanada--connect-host
   :service meghanada--connect-port
   :noquery t
   :sentinel 'meghanada--task-client-process-sentinel
   :filter 'meghanada--task-client-process-filter))

(defun meghanada--start-server-and-client ()
  "TODO: FIX DOC ."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      meghanada--client-process
    (if (and meghanada--server-process (process-live-p meghanada--server-process))
        (setq meghanada--client-process (meghanada--start-client-process))
      (progn
        (meghanada--client-kill)
        (setq meghanada--server-pending
              #'(lambda ()
                  (setq meghanada--client-process (meghanada--start-client-process))))
        (meghanada--get-server-process-create)))))

(defun meghanada--get-client-process-create ()
  "TODO: FIX DOC ."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      meghanada--client-process
    (setq meghanada--client-process (meghanada--start-client-process))))

(defun meghanada--client-process-sentinel (process event)
  "TODO: FIX DOC PROCESS EVENT."
  (unless (process-live-p process)
    (setq meghanada--client-process nil)
    (message "meghanada-client process stopped")))

(defun meghanada--task-client-process-sentinel (process event)
  "TODO: FIX DOC PROCESS EVENT."
  (unless (process-live-p process)
    (setq meghanada--task-client-process nil)
    (message "meghanada-client process stopped")))

(defun meghanada--process-client-response (process response)
  "TODO: FIX DOC PROCESS RESPONSE ."
  (let ((out (meghanada--remove-eot response))
        (callback (meghanada--process-pop-callback process)))
    (when callback
      (with-demoted-errors "Warning: %S"
        (apply (car callback) out (cdr callback))))))

(defun meghanada--client-process-filter (process output)
  "TODO: FIX DOC PROCESS OUTPUT."
  (let ((pbuf (process-buffer process))
        responses)
    (when meghanada--client-pending
      (funcall meghanada--client-pending)
      (setq meghanada--client-pending nil))
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (save-excursion
          (goto-char (process-mark process))
          (insert output)
          (set-marker (process-mark process) (point))
          (goto-char (point-min))
          (while (search-forward meghanada--eot nil t)
            (let ((response (buffer-substring-no-properties (point-min)
                                                            (point))))
              (delete-region (point-min) (point))
              (setq responses (cons (meghanada--remove-eot response) responses))))
          (goto-char (process-mark process)))))
    ;; Handle all responses.
    (mapc #'(lambda (r)
              (meghanada--process-client-response process r))
          (nreverse responses))))

(defun meghanada--task-client-process-filter (process output)
  "TODO: FIX DOC PROCESS OUTPUT."
  (let* ((buf meghanada--task-buffer)
         (cwin (get-buffer-window (current-buffer) t))
         (bwin (get-buffer-window buf t)))
    ;; (pop-to-buffer buf)
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only nil)
      (insert output)
      (goto-char (point-max))
      (if (and (string= buf meghanada--junit-buf-name)
               (search-backward meghanada--eot nil t))
          (progn
            (while (re-search-forward meghanada--eot nil t)
              (replace-match ""))
            (goto-char (point-min))
            (compilation-mode)
            (compilation-next-error 1))
        (progn
          (while (re-search-backward meghanada--eot nil t)
            (replace-match ""))
            (compilation-mode))))))

(defun meghanada--process-push-callback (process cb)
  "TODO: FIX DOC PROCESS CB."
  (let ((callbacks (process-get process 'meghanada-callback-stack)))
    (if callbacks
        (nconc callbacks (list cb))
      (process-put process 'meghanada-callback-stack (list cb)))))

(defun meghanada--process-pop-callback (process)
  "TODO: FIX DOC PROCESS."
  (let ((callbacks (process-get process 'meghanada-callback-stack)))
    (process-put process 'meghanada-callback-stack (cdr callbacks))
    (car callbacks)))

(defun meghanada--client-kill ()
  "TODO: FIX DOC ."
  (when (and meghanada--client-process (process-live-p meghanada--client-process))
    (kill-process meghanada--client-process)
    (setq meghanada--client-process nil))
  (when (and meghanada--task-client-process (process-live-p meghanada--task-client-process))
    (kill-process meghanada--task-client-process)
    (setq meghanada--task-client-process nil)))

(defun meghanada--send-request (request callback &rest args)
  "TODO: FIX DOC REQUEST CALLBACK ARGS."
  (let* ((process (meghanada--get-client-process-create))
         (argv (cons request args))
         (callback (if (listp callback) callback (list callback)))
         (send-str (format "%s" argv)))
    (when (and process (process-live-p process))
      (meghanada--process-push-callback process callback)
      (meghanada--without-narrowing
       (process-send-string process
                            (format "%s\n" send-str))))))

(defun meghanada--send-request-process (request process callback &rest args)
  "TODO: FIX DOC REQUEST PROCESS CALLBACK ARGS."
  (let* ((argv (cons request args))
         (callback (if (listp callback) callback (list callback)))
         (send-str (format "%s" argv)))
    (when (and process (process-live-p process))
      (meghanada--process-push-callback process callback)
      (meghanada--without-narrowing
       (process-send-string process
                            (format "%s\n" send-str))))))

(defvar meghanada--sync-id 0)
(defvar meghanada--sync-result '(-1 . nil))

(defun meghanada--sync-request-callback (response id)
  "TODO: FIX DOC RESPONSE ID."
  (setq meghanada--sync-result (cons id response)))

(defun meghanada--send-request-sync (request &rest args)
  "TODO: FIX DOC REQUEST ARGS."
  (let* ((id meghanada--sync-id)
         (callback (list #'meghanada--sync-request-callback id)))
    (setq meghanada--sync-id (1+ meghanada--sync-id))
    (with-local-quit
      (let ((process (meghanada--get-client-process-create)))
        (apply 'meghanada--send-request request callback args)
        (while (not (= id (car meghanada--sync-result)))
          (accept-process-output process))
        (cdr meghanada--sync-result)))))


;;
;; meghanada api
;;

(defun meghanada-alive-p ()
  "TODO: FIX DOC ."
  (and meghanada--client-process (process-live-p meghanada--client-process)))

;;
;; import
;;

(defun meghanada--goto-imports-start ()
  "TODO: FIX DOC ."
  (goto-char (point-min))
  (let ((package-point (re-search-forward "package .*;" nil t))
        (import-point (re-search-forward "import .*;" nil t)))
    (cond (import-point (goto-char import-point)
                        (beginning-of-line))
          (package-point (goto-char package-point)
                         (forward-line)
                         (open-line 2)
                         (forward-line))
          (t (goto-char (point-min))
             (open-line 1)))))

(defun meghanada--import-name (imp)
  "TODO: FIX DOC IMP ."
  (let ((cs case-fold-search))
    (when cs
      (setq case-fold-search nil))
    (let ((imp (when (string-match "\\([a-z0-9_]+\\.\\)+[A-Za-z0-9_]+" imp)
                 (match-string 0 imp))))
      (prog1
          imp
        (when cs
          (setq case-fold-search t))))))

(defun meghanada--import-exists-p (imp)
  "TODO: FIX DOC IMP ."
    (let ((imp (meghanada--import-name imp)))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (concat "^import\\s-+" imp "\\s-*;") nil t))))

(defun meghanada--add-import (imp)
  "TODO: FIX DOC IMP ."
  (unless (or (string-prefix-p "java.lang." imp) (meghanada--import-exists-p imp))
    (let ((imp  (meghanada--import-name imp))
          (start t))
      (save-excursion
        (meghanada--goto-imports-start)
        (while (and start
                    (re-search-forward "^import .+;" nil t))
          (forward-line)
          (setq start (/= (point-at-bol) (point-at-eol))))
        (insert (format "import %s;\n" imp)))
      (meghanada-add-import-async imp #'identity))))

(defun meghanada-optimize-import--callback (out)
  "TODO: FIX DOC OUT ."
  (let ((result (read out))
        (start-imp t))
    (when result
      (save-excursion
        (meghanada--goto-imports-start)
        (mapc
         (lambda (imp)
           ;; (when (and start-imp (string-prefix-p "java" imp))
           ;;   (setf start-imp nil)
           ;;   (beginning-of-line)
           ;;   (insert "\n"))
           (insert (format "import %s;\n" imp))) result)

        (while (re-search-forward "^import" nil t)
          (beginning-of-line)
          (kill-line))))))

(defun meghanada-import-all--callback (out optimize)
  "TODO: FIX DOC OUT OPTIMIZE ."
  (let ((result (read out)))
    (when result
      (save-excursion
        (meghanada--goto-imports-start)
        (mapc
         (lambda (imps)
           (if (= (length imps) 1)
               (meghanada--add-import (car imps))
             (let ((res (completing-read "import:" imps nil t)))
               (unless (string= res "")
                 (meghanada--add-import res))))) result)))
    (when optimize
      (save-buffer)
      (meghanada-optimize-import-async #'meghanada-optimize-import--callback))))

;;
;; meghanada client api
;;

(defun meghanada-client-direct-connect ()
  "TODO: FIX DOC ."
  (interactive)
  (meghanada--get-client-process-create))

(defun meghanada-client-connect ()
  "TODO: FIX DOC ."
  (interactive)
  (meghanada--start-server-and-client))

(defun meghanada-client-disconnect ()
  "TODO: FIX DOC ."
  (interactive)
  (meghanada--client-kill))

;;
;; meghanada other api
;;

(defun meghanada-ping ()
  "TODO: FIX DOC ."
  (interactive)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ping" #'message)
    (message "client connection not established")))

(defun meghanada-clear-cache ()
  "TODO: FIX DOC ."
  (interactive)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "cc" #'message)
    (message "client connection not established")))

;;
;; meghanada auto-import api
;;

(defun meghanada-add-import-async (imp callback)
  "TODO: FIX DOC IMP CALLBACK."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ai" callback (buffer-file-name) imp)
    (message "client connection not established")))

(defun meghanada-optimize-import-async (callback)
  "TODO: FIX DOC CALLBACK."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "oi" callback (buffer-file-name))
    (message "client connection not established")))

(defun meghanada-import-all-async (callback optimize)
  "TODO: FIX DOC CALLBACK OPTIMIZE."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ia" (list callback optimize) (buffer-file-name))
    (message "client connection not established")))

(defun meghanada-optimize-import ()
  "TODO: FIX DOC ."
  (interactive)
  (meghanada-import-all-async #'meghanada-import-all--callback t))

(defun meghanada-import-all ()
  "TODO: FIX DOC ."
  (interactive)
  (meghanada-import-all-async #'meghanada-import-all--callback nil))


;;
;; meghanada complete api
;;

(defun meghanada-autocomplete-prefix-async (prefix callback)
  "TODO: FIX DOC PREFIX CALLBACK."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ap"
                               callback
                               (buffer-file-name)
                               (meghanada--what-line)
                               (meghanada--what-column)
                               prefix)
    (message "client connection not established")))

(defun meghanada--local-val-callback (output)
  "TODO: FIX DOC OUTPUT ."
  (let* ((result (read output))
         (return-type (car result))
         (vals (car (cdr result)))
         (len (length vals)))
    (when (and (not (string= "void" return-type)) (> len 0))
      (back-to-indentation)
      (insert (format "final %s  = " return-type))
      (forward-char -3)
      (if (= len 1)
          (insert (car vals))
        (let ((res (completing-read "local variable:" vals nil t)))
          (unless (string= res "")
            (insert res)))))))

(defun meghanada-local-variable ()
  "TODO: FIX DOC CALLBACK."
  (interactive)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "lv"
                               #'meghanada--local-val-callback
                               (buffer-file-name)
                               (meghanada--what-line))
    (message "client connection not established")))

;;
;; meghanada diagnostics api (flycheck)
;;

(defun meghanada-diagnostics-async (callback)
  "TODO: FIX DOC CALLBACK."
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "di"
                               callback
                               (buffer-file-name))
    (message "client connection not established")))

;;
;; meghanada interactive-command (async)
;;

(defun meghanada-parse-file ()
  "TODO: FIX DOC ."
  (interactive)
  (when (eq major-mode 'meghanada-mode)
    (if (and meghanada--client-process (process-live-p meghanada--client-process))
        (meghanada--send-request "p" #'message (buffer-file-name))
      (message "client connection not established"))))

;;
;; meghanada compile-command (async)
;;

(defun kill-buf (name)
  (when (get-buffer name)
    (delete-windows-on (get-buffer name))
    (kill-buffer name)))

(defun meghanada--compile-callback (output)
  "TODO: FIX DOC OUTPUT ."
  (let* ((result (read output))
         (severity (car result)))
    (pcase severity
      (`success
       (progn
         (kill-buf "*compilation*")
         (message "compile finished")))
      (`error
       (let ((messages (cdr result)))
         (with-current-buffer (get-buffer-create "*compilation*")
           (setq buffer-read-only nil)
           (save-excursion
             (dolist (msg messages)
               (insert msg)
               (open-line 1))
             (goto-char (point-min)))
           (compilation-mode)))))))

(defun meghanada-compile-file ()
  "TODO: FIX DOC ."
  (interactive)
  (when (eq major-mode 'meghanada-mode)
    (if (and meghanada--client-process (process-live-p meghanada--client-process))
        (let ((buf (buffer-file-name)))
          (message "compiling ... ")
          (kill-buf "*compilation*")
          (pop-to-buffer "*compilation*")
          (meghanada--send-request "c" #'meghanada--compile-callback buf))
      (message "client connection not established"))))

(defun meghanada-compile-project ()
  "TODO: FIX DOC ."
  (interactive)
  (when (eq major-mode 'meghanada-mode)
    (if (and meghanada--client-process (process-live-p meghanada--client-process))
        (progn
          (message "compiling ... ")
          (kill-buf "*compilation*")
          (pop-to-buffer "*compilation*")
          (meghanada--send-request "cp" #'meghanada--compile-callback))
      (message "client connection not established"))))

(setq compilation-error-regexp-alist
      (append (list
               ;; works for javac
               '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)
               ;; works for maven
               '("^\\(.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)\\]" 1 2 3))
              compilation-error-regexp-alist))


;;
;; meghanada junit api
;;

(defun meghanada--switch-testcase-callback (out)
  "TODO: FIX DOC OUT."
  (let ((result (read out)))
    (when result
      (find-file result))))

(defun meghanada-switch-testcase ()
  "TODO: FIX DOC CALLBACK."
  (interactive)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "st" #'meghanada--switch-testcase-callback (buffer-file-name))
    (message "client connection not established")))


(defun meghanada--setup-task-buffer (buf-name height)
  "TODO: FIX DOC BUF-NAME HEIGHT."
  (when (not (get-buffer-window buf-name))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer buf-name)
          (shrink-window (- h height)))))))

(defun meghanada--junit-callback (output)
  )

(defun meghanada--run-junit (test)
  "TODO: FIX DOC TEST."
  (if meghanada--task-client-process
      (progn
        (kill-buf meghanada--task-buf-name)
        (kill-buf meghanada--junit-buf-name)
        (setq meghanada--task-buffer meghanada--junit-buf-name)
        (pop-to-buffer meghanada--junit-buf-name)
        (meghanada--send-request-process "rj" meghanada--task-client-process #'meghanada--junit-callback test))
    (message "client connection not established")))

(defun meghanada-run-junit-class ()
  "TODO: FIX DOC."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (test-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\."))))
    (meghanada--run-junit test-name)))

(defun meghanada-run-junit-test-case ()
  "TODO: FIX DOC."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (class-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\.")))
         (test-case (car (imenu-choose-buffer-index "Test case: ")))
         (test-name (format "%s#%s" class-name test-case)))
    (meghanada--run-junit test-name)))

(defun meghanada-run-junit-recent ()
  "TODO: FIX DOC."
  (interactive)
  (meghanada--run-junit ""))

(defun meghanada-run-task (args)
  "TODO: FIX DOC ARGS."
  (interactive "sArgs: ")
  (message args)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (progn
        (kill-buf meghanada--task-buf-name)
        (kill-buf meghanada--junit-buf-name)
        (setq meghanada--task-buffer meghanada--task-buf-name)
        (pop-to-buffer meghanada--task-buf-name)
        (meghanada--send-request-process "rt" meghanada--task-client-process #'meghanada--junit-callback args))
    (message "client connection not established")))

;;
;; meghanada jump api
;;

(defun meghanada--jump-callback (output)
  "TODO FIX DOC OUTPUT."
  (let* ((res (read output))
         (filename (nth 0 res))
         (line (nth 1 res))
         (col (nth 2 res)))
    (message filename)
    (funcall #'find-file-other-window filename)
    (meghanada--goto-line line)
    (beginning-of-line)
    (forward-char (1- col))
    (if (buffer-modified-p)
        (message "Buffer is modified, file position might not have been correct"))))

(defun meghanada-jump-declaration ()
  "TODO: FIX DOC."
  (interactive)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "jd" #'meghanada--jump-callback
                               (buffer-file-name)
                               (meghanada--what-line)
                               (meghanada--what-column)
                               (meghanada--what-word))
    (message "client connection not established")))

(defun meghanada-back-jump ()
  "TODO: FIX DOC."
  (interactive)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "bj" #'meghanada--jump-callback)
    (message "client connection not established")))

;;
;; meghanada-mode
;;

(defvar meghanada-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap of Meghanada interactive commands.")

;;;###autoload
(define-derived-mode meghanada-mode java-mode "Meghanada"
  "A new, better, Java mode."
  (meghanada-client-connect))

(remove-hook 'java-mode-hook 'wisent-java-default-setup)

(provide 'meghanada-mode)
;;; meghanada-mode.el ends here
