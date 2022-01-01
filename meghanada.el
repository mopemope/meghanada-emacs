;;; meghanada.el --- A better java development mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2016 - 2021 Yutaka Matsubara
;; License: http://www.gnu.org/licenses/gpl.html

;; Author: Yutaka Matsubara (yutaka.matsubara@gmail.com)
;; Homepage: https://github.com/mopemope/meghanada-emacs
;; Keywords: languages java
;; Package-Version: 1.3.2
;; Package-Requires: ((emacs "24.3") (yasnippet "0.6.1") (company "0.9.0") (flycheck "0.23"))

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
;;  Meghanada has a server component which can read the AST of your
;;  project and its dependencies, providing features.
;;
;;

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'thingatpt)
(require 'compile)
(require 'imenu)
(require 'url)
(require 'which-func)
(require 'xref)

(autoload 'meghanada-company-enable "company-meghanada")
(autoload 'meghanada-flycheck-enable "flycheck-meghanada")
(autoload 'meghanada-eldoc-enable "eldoc-meghanada")

;;
;; Const
;;

(defconst meghanada-version "1.3.2")
(defconst meghanada-setup-version "0.0.2")
(defconst meghanada--eot "\n;;EOT\n")
(defconst meghanada--junit-buf-name "*meghanada-junit*")
(defconst meghanada--task-buf-name "*meghanada-task*")
(defconst meghanada--ref-buf-name "*meghanada-reference*")
(defconst meghanada--search-buf-name "*meghanada-search-everywhere*")
(defconst meghanada--show-project-buf-name "*meghanada-project*")
(defconst meghanada--typeinfo-buf-name "*meghanada-typeinfo*")
(defconst meghanada--install-err-buf-name "*meghanada-install-error*")
(defconst meghanada--err-buf-name "*meghanada-error*")

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

(defcustom meghanada-port 0
  "Meghanada server port.

A port number of 0 means that the port number is automatically allocated, typically from an ephemeral port range.
default 0.
"
  :group 'meghanada
  :type 'integer)

(defcustom meghanada-debug nil
  "If true, meghanada-server outputs debug log."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-use-company t
  "If true, company-mode auto-comletion is enabled."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-use-flycheck t
  "If true, diagnostics report with flyecheck is enabled."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-use-eldoc t
  "If true, eldoc for meghanada enabled."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-auto-start t
  "If true, meghanada-server start automatically."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-server-remote-debug nil
  "If true, meghanda-server enabled remote debug."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-server-install-dir (locate-user-emacs-file "meghanada/")
  "Install directory for meghanada-server.

The slash is expected at the end."
  :group 'meghanada
  :risky t
  :type 'directory)

(defcustom meghanada-server-download-url "https://github.com/mopemope/meghanada-server/releases/download/v%s/meghanada-setup-%s.jar"
  "Download location of meghanada-server's jar."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-java-path "java"
  "Path of the java executable.

If the path of maven is in the environment, we can use the short path.
Otherwise, we can always use the full path, in Windows, we can use this to get it:
`(expand-file-name \"bin/java.exe\" (getenv \"JAVA_HOME\"))'
In linux or macOS, it can be \"java\". In Windows, it can be \"java.exe\"."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-maven-path "mvn"
  "Path of the maven executable.

If the path of maven is in the environment, we can use the short path.
Otherwise, we can always use the full path.
For the short paht:
In linux or macOS, it can be \"mvn\"; In Windows, it can be \"mvn.cmd\". "
  :group 'meghanada
  :type 'string)

(defcustom meghanada-maven-local-repository nil
  "Overriding maven repository path."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-javac-xlint "-Xlint:all"
  "Overriding javac's -Xlint."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-gradle-version nil
  "Overriding gradle version."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-gradle-prepare-compile-task nil
  "Set to gradle prepare compileJava task name."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-gradle-prepare-test-compile-task nil
  "Set to gradle prepare compileTestJava task name."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-skip-build-subprojects t
  "If true, skip gradle dependency subprojects build."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-jvm-option nil
  "Set to all meghanada java process jvm option.

Example. (setq meghanada-jvm-option \"-Dhttp.proxyHost=test.proxy.com -Dhttp.proxyPort=8080\")
"
  :group 'meghanada
  :type 'string)

(defcustom meghanada-server-jvm-option "-Xms128m -XX:ReservedCodeCacheSize=240m -XX:SoftRefLRUPolicyMSPerMB=50 -ea -Dsun.io.useCanonCaches=false"
  "Set to meghanada server process jvm option."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-mode-key-prefix [?\C-c]
  "The prefix key for meghanada-mode commands."
  :group 'meghanada
  :type 'sexp)

(defcustom meghanada-reference-prepare #'meghanada--reference-prepare
  "It is called before meghanada-reference."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-reference-callback #'meghanada--reference-callback
  "It will be called after receiving meghanada-reference result."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-typeinfo-prepare #'meghanada--typeinfo-prepare
  "It is called before meghanada-typeinfo."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-typeinfo-callback #'meghanada--typeinfo-callback
  "It will be called after receiving meghanada-typeinfo result."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-search-prepare #'meghanada--search-prepare
  "It is called before meghanada-search-everywhere."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-search-callback #'meghanada--search-callback
  "It will be called after receiving meghanada-search-everywhere result."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-cache-in-project nil
  "If true, create a cache in the project.otherwise, create in cache root directory (~/.cache/meghanada). default nil"
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-cache-root nil
  "Set to meghanada cache root.default value is '~/.cache/meghanada'."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-task-buffer-auto-scroll t
  "If true, automatically move to the end of the task buffer after inserting new output."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-make-search-query #'meghanada--make-search-query
  "Sets the function that returns the search query."
  :group 'meghanada
  :type 'function)

(defcustom meghanada-import-static-enable "java.util.Objects,org.junit.Assert"
  "Sets import static completion classes."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-full-text-search-enable nil
  "If true, Enable full text search and meghanada-search-everywhere."
  :group 'meghanada
  :type 'boolean)

(defcustom meghanada-completion-matcher "prefix"
  "Select completion matcher.  You can choose from prefix, contains, fuzzy, came-case.  default is prefix."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-class-completion-matcher "prefix"
  "Select class completion matcher. You can choose from prefix, contains, fuzzy, came-case.  default is prefix."
  :group 'meghanada
  :type 'string)

(defcustom meghanada-mode-after-test-hook '()
  "Hook that is called after a JUnit test execution is done."
  :group 'meghanada)

(defcustom meghanada-telemetry-enable nil
  "If true, Enables telemetry and allows you to collect and submit performance data."
  :group 'meghanada
  :type 'boolean)

;;
;; utility
;;

(defun meghanada--what-line ()
  "Return the current buffer line number and narrowed line number of point."
  (let ((start (point-min))
        (n (line-number-at-pos)))
    (if (= start 1)
        n
      (save-excursion
        (save-restriction
          (widen)
          (+ n (line-number-at-pos start) -1) n)))))

(defun meghanada--real-current-column ()
  "like `current-column', but skip invisible characters in pretty-symbol-mode."
  (- (point) (line-beginning-position)))

(defun meghanada--what-column ()
  "Returns the current column number."
  (number-to-string (1+ (meghanada--real-current-column))))

(defun meghanada--what-symbol ()
  "Returns the symbol on the cursor."
  (thing-at-point 'symbol))

(defun meghanada--what-word ()
  "Returns the word on the cursor."
  (thing-at-point 'word))

(defmacro meghanada--without-narrowing (&rest body)
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))

(defun meghanada--remove-eot (out)
  (replace-regexp-in-string meghanada--eot "" out))

(defun meghanada--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun meghanada--line-column-to-point (line column)
  (save-excursion
    (meghanada--goto-line line)
    (forward-char (1- column))
    (point)))

(defun meghanada--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun meghanada-version ()
  "Show meghanada-version ."
  (interactive)
  (message meghanada-version))

;;
;; meghanada-server process management.
;;

(defvar meghanada--server-process nil)
(defvar meghanada--server-buffer "*meghanada-server-log*")
(defvar meghanada--server-pending nil)
(defvar meghanada--server-jar nil)

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

;; TODO pop-to-buffer

(defun meghanada--download-from-url (url dest-jar)
  "Download a jar file from URL to DEST-JAR Path."
  (let ((dest meghanada-server-install-dir))
    (unless (file-exists-p dest)
      (make-directory dest t))
    (message (format "Download module from %s. Please wait ..." url))
    (url-handler-mode t)
    (if (file-exists-p url)
        (progn
          (url-copy-file url dest-jar)
          (message (format "Downloaded module from %s to %s." url dest-jar)))
      (error "Not found %s" url))))


(defun meghanada--setup ()
  "Setup meghanada-server-module."
  (meghanada--download-setup-jar)
  (meghanada--run-setup))

(defun meghanada--setup-options ()
  (let ((options '()))
    (when meghanada-jvm-option
      (push meghanada-jvm-option options))
    (mapconcat 'identity  options " ")))

(defun meghanada--run-setup ()
  "Setup meghanada server module."
  (let ((jar (meghanada--locate-setup-jar))
        (dest meghanada-server-install-dir))
    (if (file-exists-p jar)
        (let ((cmd (format "%s %s -jar %s --dest %s --server-version %s --simple"
                           (shell-quote-argument meghanada-java-path)
                           (meghanada--setup-options)
                           (shell-quote-argument jar)
                           (expand-file-name dest)
                           meghanada-version)))
          (message "Download meghanada server module. Please wait ...")
          (let ((proc (start-process-shell-command "*meghanada-install*" "*meghanada-install*" cmd))
                (buf (current-buffer)))
            (pop-to-buffer "*meghanada-install*")
            (set-process-filter
             proc
             #'(lambda (process msg)
                 (when (process-live-p process)
                   (with-current-buffer (process-buffer process)
                     (insert msg)
                     (message "Download server module ...")))))
            (set-process-sentinel
             proc
             #'(lambda (process msg)
                 (unless (process-live-p process)
                   (if (eq 0 (process-exit-status process))
                       (progn
                         (message (format "Success. It downloaded to %s." meghanada-server-install-dir))
                         (pop-to-buffer buf)
                         (with-current-buffer buf
                           (meghanada-mode t)
                           (meghanada-restart)))
                     (message (format "Failure. The installation seems to have failed, please check the message buffer for details. The jar should have been downloaded to %s." meghanada-server-install-dir)))))))))))

(defun meghanada--download-setup-jar ()
  "Download setup-jar file from bintray."
  (let ((url (format meghanada-server-download-url
                     meghanada-version
                     meghanada-setup-version))
        (setup-jar (meghanada--locate-setup-jar)))

    (unless (file-exists-p setup-jar)
      (meghanada--download-from-url
       url
       setup-jar))))

(defun meghanada--download-server-jar ()
  "Direct download server jar file."
  (let ((dest meghanada-server-install-dir)
        (dest-jar (meghanada--locate-server-jar))
        (url (format meghanada-server-download-url
                     meghanada-version
                     meghanada-setup-version)))
    (unless (file-exists-p dest)
      (make-directory dest t))
    (message (format "Download server module from %s. Please wait." url))
    (url-handler-mode t)
    (if (file-exists-p url)
        (progn
          (url-copy-file url dest-jar)
          (message (format "Downloaded server module from %s to %s." url dest-jar)))
      (error "Not found %s" url))))

;;;###autoload
(defun meghanada-install-server ()
  "Install meghanada-server's jar file from bintray ."
  (interactive)
  (let ((dest-jar (meghanada--locate-server-jar)))
    (if (file-exists-p dest-jar)
        nil
      (condition-case err
          (progn
            (meghanada--setup)
            t)
        (error
         (let ((error-buf meghanada--install-err-buf-name))
           (with-current-buffer (get-buffer-create error-buf)
             (insert (format "Error: %s" (error-message-string err)))
             (compilation-mode))))))))

;;;###autoload
(defun meghanada-update-server ()
  "Update meghanada-server's jar file from bintray ."
  (interactive)
  (meghanada-install-server))

(defun meghanada--locate-server-jar ()
  (expand-file-name
   (format "meghanada-%s.jar" meghanada-version)
   meghanada-server-install-dir))

(defun meghanada--locate-setup-jar ()
  "Return the path of the meghanada-setup.jar file."
  (expand-file-name
   (format "meghanada-setup-%s.jar" meghanada-setup-version)
   meghanada-server-install-dir))

(defun meghanada--server-options ()
  (let ((options '()))
    (when meghanada-maven-path
      (push (format "-Dmeghanada.maven.path=%s" meghanada-maven-path) options))
    (when meghanada-maven-local-repository
      (push (format "-Dmeghanada.maven.local.repository=%s" meghanada-maven-local-repository) options))
    (when meghanada-javac-xlint
      (push (format "-Dmeghanada.javac.arg=%s" meghanada-javac-xlint) options))
    (when meghanada-import-static-enable
      (push (format "-Dmeghanada.search.static.method.classes=%s" meghanada-import-static-enable) options))
    (when meghanada-full-text-search-enable
      (push "-Dmeghanada.full.text.searchs=true" options))
    (when meghanada-gradle-version
      (push (format "-Dmeghanada.gradle.version=%s" meghanada-gradle-version) options))
    (when meghanada-gradle-prepare-compile-task
      (push (format "-Dmeghanada.gradle.prepare.compile.task=%s" meghanada-gradle-prepare-compile-task) options))
    (when meghanada-gradle-prepare-test-compile-task
      (push (format "-Dmeghanada.gradle.prepare.test.compile.task=%s" meghanada-gradle-prepare-test-compile-task) options))
    (if meghanada-skip-build-subprojects
        (push "-Dmeghanada.skip.build.subprojects=true" options)
      (push "-Dmeghanada.skip.build.subprojects=false"  options))
    (if meghanada-cache-in-project
        (push "-Dmeghanada.cache.in.project=true" options)
      (push "-Dmeghanada.cache.in.project=false"  options))
    (when meghanada-cache-root
      (push (format "-Dmeghanada.cache.root=%s" meghanada-cache-root) options))
    (when meghanada-server-remote-debug
      (push "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005" options))
    (push "-Dmeghanada.format=sexp" options)
    (when meghanada-completion-matcher
      (push (format "-Dmeghanada.completion.matcher=%s" meghanada-completion-matcher) options))
    (when meghanada-class-completion-matcher
      (push (format "-Dmeghanada.class.completion.matcher=%s" meghanada-class-completion-matcher) options))
    (when meghanada-jvm-option
      (push meghanada-jvm-option options))
    (when meghanada-telemetry-enable
      (push "-Dmeghanada.telemetry.enable=true" options))
    (push "-Djava.net.preferIPv4Stack=true" options)
    (mapconcat 'identity  options " ")))

(defun meghanada--start-server-process ()
  (let ((jar (meghanada--locate-server-jar)))
    (if (file-exists-p jar)
        (let ((process-connection-type nil)
              (process-adaptive-read-buffering nil)
              (cmd (format "%s %s %s -Dfile.encoding=UTF-8 -Dlog4j2.formatMsgNoLookups=true -jar %s -p %d %s %s"
                           (shell-quote-argument meghanada-java-path)
                           (meghanada--server-options)
                           meghanada-server-jvm-option
                           (shell-quote-argument jar)
                           meghanada-port
                           (if meghanada-debug "-v" "")
                           (concat "-l " temporary-file-directory "meghanada_server_" (number-to-string (user-uid)) ".log")))
              process)
          (message (format "launch server cmd:%s" cmd))
          (setq process
                (start-process-shell-command
                 "meghanada-server"
                 meghanada--server-buffer
                 cmd))
          (buffer-disable-undo meghanada--server-buffer)
          (set-process-query-on-exit-flag process nil)
          (set-process-sentinel process 'meghanada--server-process-sentinel)
          (set-process-filter process 'meghanada--server-process-filter)
          (message "Meghanada-Server Starting ...")
          process)
      (message "%s"
               (substitute-command-keys
                "Missing server module. Type `\\[meghanada-install-server]' to install meghanada-server")))))

(defun meghanada--get-server-process-create ()
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (progn
        (message "already started meghanada-server. see *meghanada-server-log* buffer.")
        meghanada--server-process)
    (setq meghanada--server-process (meghanada--start-server-process))))

(defun meghanada--server-process-sentinel (process msg)
  (unless (process-live-p process)
    (set-process-sentinel process 'ignore)
    (set-process-filter process 'ignore)
    (delete-process process)
    (setq meghanada--server-process nil)
    (error (format "Error:meghanada-server process stopped: %s. Please check *meghanada-server-log* buffer" msg))))

(defun meghanada--server-process-filter (process output)
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
            (string-match "port:\\([0-9]+\\)+" output)
            (let* ((p (substring output (match-beginning 1) (match-end 1))))
              (setq meghanada--connect-port (string-to-number p)))
            (message "Server waiting client connection ...")
            (when meghanada--server-pending
              (funcall meghanada--server-pending)
              (setq meghanada--server-pending nil)))
          (when (string-match "MEGHANADA_FAILED " output)
            (string-match "FILE:\\(.*.log\\)" output)
            (let* ((log-file (substring output (match-beginning 1) (match-end 1))))
              (with-help-window (get-buffer-create meghanada--err-buf-name)
                (pop-to-buffer meghanada--err-buf-name)
                (save-excursion
                  (insert-file-contents-literally log-file)
                  (message
                   (format "ERROR: project initialize error. please check log %s" log-file)))))))))))

;;;###autoload
(defun meghanada-server-start ()
  "Start the meghanada server."
  (interactive)
  (meghanada--get-server-process-create))

;;;###autoload
(defun meghanada-server-kill ()
  "Kill the meghanada server."
  (interactive)
  (when (and meghanada--server-process (process-live-p meghanada--server-process))
    (set-process-sentinel meghanada--server-process 'ignore)
    (set-process-filter meghanada--server-process 'ignore)
    (kill-buffer (process-buffer meghanada--server-process))
    ;; (kill-process meghanada--server-process)
    (delete-process meghanada--server-process)
    (setq meghanada--server-process nil)))


(defun meghanada--start-client-process ()
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
    process))

(defun meghanada--start-task-client-process ()
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
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      meghanada--client-process
    (unless (and meghanada--server-process (process-live-p meghanada--server-process))
      (progn
        (meghanada--client-kill)
        (setq meghanada--server-pending
              #'(lambda ()
                  (setq meghanada--client-process (meghanada--start-client-process))
                  (message "Please wait. Meghanada indexing ... ")))
        (meghanada--get-server-process-create)))))

(defun meghanada--get-client-process-create ()
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      meghanada--client-process
    (setq meghanada--client-process (meghanada--start-client-process))))

(defun meghanada--client-process-sentinel (process msg)
  (unless (process-live-p process)
    (set-process-sentinel process 'ignore)
    (set-process-filter process 'ignore)
    (kill-buffer (process-buffer process))
    (delete-process process)
    (setq meghanada--client-process nil)
    (error (format "Disconnected:meghanada-client process stopped: %s. Please check *meghanada-server-log* buffer" msg))))

(defun meghanada--task-client-process-sentinel (process msg)
  (unless (process-live-p process)
    (set-process-sentinel process 'ignore)
    (set-process-filter process 'ignore)
    (kill-buffer (process-buffer process))
    (delete-process process)
    (setq meghanada--task-client-process nil)
    (error (format "Disconnected:meghanada-task-client process stopped: %s. Please check *meghanada-server-log* buffer" msg))))

(defun meghanada--process-client-response (process response)
  (let* ((output (read (meghanada--normalize-repsonse-file-path (meghanada--remove-eot response))))
         (callback (meghanada--process-pop-callback process))
         (status (car output))
         (res (car (cdr output))))
    (pcase status
      (`success
       (when callback
         (with-demoted-errors "Warning: %S"
           (apply (car callback) res (cdr callback)))))
      (`error
       (ignore-errors
         (progn
           (message (format "Error:%s . Please check *meghanada-server-log*" res))
           (apply (car callback) nil (cdr callback))))))))

(defun meghanada--normalize-repsonse-file-path (response)
  (if (eq system-type 'windows-nt)
      (let (diver-char downcase-char)
        (setq response (replace-regexp-in-string "\\\\" "/" response))
        (when (string-match "\\([A-Z]\\):/" response)
          (setq diver-char (match-string 1 response))
          (setq downcase-char (downcase diver-char))
          (setq response (replace-regexp-in-string (concat diver-char ":/") (concat downcase-char ":/") response)))
        response))
  response)

(defun meghanada--normalize-request-file-path (request)
  (if (eq system-type 'windows-nt)
      (let (diver-char upcase-char)
        (when (string-match "\\([a-z]\\):/" request)
          (setq diver-char (match-string 1 request))
          (setq upcase-char (upcase diver-char))
          (setq request (replace-regexp-in-string (concat diver-char ":/") (concat upcase-char ":/") request)))
        (setq request (replace-regexp-in-string "/" "\\\\" request))
        request))
  request)

(defun meghanada--client-process-filter (process output)
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

(defun meghanada--task-client-process-filter (ignored output)
  (let ((buf meghanada--task-buffer))
    ;; (pop-to-buffer buf)
    (with-current-buffer (get-buffer-create buf)
      (let ((win (get-buffer-window buf))
            (eob (eq (point) (point-max)))
            (eot nil))
        ;; Insert the new output at the end of the buffer
        ;; and restore the buffer point afterward
        (save-excursion
          (goto-char (point-max))
          (setq buffer-read-only nil)
          (insert output)
          (if (and (string= buf meghanada--junit-buf-name)
                   (search-backward meghanada--eot nil t))
              (progn
                (while (re-search-forward meghanada--eot nil t)
                  (replace-match "")
                  (setq eot t))
                (when eot
                  (compilation-mode)))
            (progn
              (while (re-search-backward meghanada--eot nil t)
                (replace-match "")
                (setq eot t))
              (when eot
                (compilation-mode))))
          (setq buffer-read-only t)
          ;; Run all after test hooks now that the buffer is read-only
          (when (string= buf meghanada--junit-buf-name)
            (run-hooks 'meghanada-mode-after-test-hook)))

        ;; If the cursor is already at the end of the buffer or if
        ;; auto-scrolling is activated, move the cursor to the end of the buffer
        ;; (moves both buffer point and window point)
        (when (or meghanada-task-buffer-auto-scroll eob)
          (goto-char (point-max))
          (when win (set-window-point win (point))))))))

(defun meghanada--process-push-callback (process cb)
  (let ((callbacks (process-get process 'meghanada-callback-stack)))
    (if callbacks
        (nconc callbacks (list cb))
      (process-put process 'meghanada-callback-stack (list cb)))))

(defun meghanada--process-pop-callback (process)
  (let ((callbacks (process-get process 'meghanada-callback-stack)))
    (process-put process 'meghanada-callback-stack (cdr callbacks))
    (car callbacks)))

(defun meghanada--client-kill ()
  "Disconnect and kill meghanada-client."
  (when (and meghanada--client-process (process-live-p meghanada--client-process))
    (set-process-sentinel meghanada--client-process 'ignore)
    (set-process-filter meghanada--client-process 'ignore)
    (kill-buffer (process-buffer meghanada--client-process))
    (delete-process meghanada--client-process)
    (setq meghanada--client-process nil))
  (when (and meghanada--task-client-process (process-live-p meghanada--task-client-process))
    (set-process-sentinel meghanada--task-client-process 'ignore)
    (set-process-filter meghanada--task-client-process 'ignore)
    (kill-buffer (process-buffer meghanada--task-client-process))
    (delete-process meghanada--task-client-process)
    (setq meghanada--task-client-process nil)))

(defun meghanada--send-request (request callback &rest args)
  (let* ((process (meghanada--get-client-process-create))
         (argv (cons request args))
         (callback (if (listp callback) callback (list callback)))
         (send-str (meghanada--normalize-request-file-path (format "%s" argv))))
    (when (and process (process-live-p process))
      (meghanada--process-push-callback process callback)
      (meghanada--without-narrowing
        (process-send-string process
                             (format "%s\n" send-str))))))

(defun meghanada--send-request-process (request process callback &rest args)
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
  (setq meghanada--sync-result (cons id response)))

(defun meghanada--send-request-sync (request &rest args)
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
  (and meghanada-mode
       meghanada--client-process
       (process-live-p meghanada--client-process)))

;;
;; import
;;

(defun meghanada--goto-imports-start ()
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
  (let ((cs case-fold-search))
    (when cs
      (setq case-fold-search nil))
    (let ((imp (when (string-match "\\([a-z0-9_]+\\.\\)+[A-Za-z0-9_]+" imp)
                 (match-string 0 imp))))
      (prog1
          imp
        (when cs
          (setq case-fold-search t))))))

(defun meghanada--is-java-lang-package-p (fqcn)
  "Check if FQCN belongs to java.lang package (exclude subpackages,
e.g. java.lang.annotation)."
  (and
   (string-prefix-p "java.lang." fqcn)
   (not (string-match-p (regexp-quote "#") fqcn))
   (<= (length (split-string fqcn "\\.")) 3)))

(defun meghanada--import-exists-p (imp)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^import\\s-+" imp "\\s-*;") nil t)))

(defun meghanada--add-import-callback (result buf)
  (with-current-buffer buf
    (let ((severity (car result)))
      (pcase severity
        (`success
         (let* ((fqcn (car (cdr result)))
                (is-static (string-match-p (regexp-quote "#") fqcn))
                (imp (if is-static
                         (replace-regexp-in-string "#" "." fqcn)
                       fqcn)))

           (unless (or (meghanada--is-java-lang-package-p imp)
                       (meghanada--import-exists-p imp))
             (let ((start t))
               (save-excursion
                 (meghanada--goto-imports-start)
                 (while (and start (re-search-forward "^import .+;" nil t))
                   (forward-line)
                   (setq start (/= (point-at-bol) (point-at-eol))))
                 (if is-static
                     (insert (format "import static %s;\n" imp))
                   (insert (format "import %s;\n" imp))))))))))))

(defun meghanada--add-import (imp buf)
  (unless
      (or
       (meghanada--is-java-lang-package-p imp)
       (meghanada--import-exists-p imp))
    (meghanada-add-import-async
     imp
     #'meghanada--add-import-callback
     buf)))

(defun meghanada-import-all--callback (result buf optimize)
  (with-current-buffer buf
    (when result
      (mapc
       (lambda (imps)
         (if (= (length imps) 1)
             (meghanada--add-import (car imps) buf)
           (let ((res (completing-read "import:" imps nil t)))
             (unless (string= res "")
               (meghanada--add-import res buf))))) result))
    (when optimize
      (save-buffer)
      (meghanada-optimize-import))))

(defun meghanada-import-at--callback (result buf optimize)
  (with-current-buffer buf
    (when result
      (mapc
       (lambda (imports)
         (let ((type (car imports))
               (imps (cdr imports)))
           (pcase type
             (`class
              (if (= (length imps) 1)
                  (meghanada--add-import (car imps) buf)
                (let ((res (completing-read "import:" imps nil t)))
                  (unless (string= res "")
                    (meghanada--add-import res buf)))))
             (`method
              (if (= (length imps) 1)
                  (meghanada--add-import (car imps) buf)
                (let ((res (completing-read "import:" imps nil t)))
                  (unless (string= res "")
                    (meghanada--add-import res buf)))))))) result))
    (when optimize
      (save-buffer)
      (meghanada-optimize-import))))

;;
;; meghanada client api
;;

;;;###autoload
(defun meghanada-client-direct-connect ()
  "Connect the client to a server that is already running."
  (interactive)
  (meghanada--get-client-process-create))

;;;###autoload
(defun meghanada-client-connect ()
  "Start the server and connect the client"
  (interactive)
  ;; TODO check
  (meghanada--start-server-and-client))

;;;###autoload
(defun meghanada-client-disconnect ()
  "Disconnecting the client."
  (interactive)
  (meghanada--client-kill))

;;;###autoload
(defun meghanada-restart ()
  "Restart meghanada server and client."
  (interactive)
  (meghanada--client-kill)
  (meghanada-server-kill)
  (meghanada--start-server-and-client))

;;
;; meghanada other api
;;

(defun meghanada-ping ()
  "Ping the connected server."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((res (meghanada--send-request-sync "ping")))
        (when res
          (message (format "%s" res))))
    (message "client connection not established")))

(defun meghanada-kill-running-process ()
  "Kills another test process running on the server, etc."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((res (meghanada--send-request-sync "kp")))
        (when res
          (meghanada--kill-buf meghanada--task-buf-name)
          (meghanada--kill-buf meghanada--junit-buf-name)
          (message (format "%s" res))))
    (message "client connection not established")))

(defun meghanada-clear-cache ()
  "Clear the server cache."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "cc" #'message)
    (message "client connection not established")))

;;
;; meghanada auto-import api
;;

(defun meghanada-add-import-async (imp callback buf)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ai" (list callback buf) (format "\"%s\"" (buffer-file-name)) imp)
    (message "client connection not established")))

(defun meghanada-import-all-async (callback buf optimize)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ia" (list callback buf optimize)  (format "\"%s\"" (buffer-file-name)))
    (message "client connection not established")))

(defun meghanada-import-at-point-async (callback buf optimize)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ip"
                               (list callback buf optimize)
                               (format "\"%s\"" (buffer-file-name))
                               (meghanada--what-line)
                               (meghanada--what-column)
                               (format "\"%s\"" (meghanada--what-symbol)))
    (message "client connection not established")))

(defun meghanada-optimize-import ()
  "Optimize the import statement.
If there are unimported classes, we will automatically import them as much as possible. Also, unused import statements are deleted."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((output (meghanada--send-request-sync
                     "oi"
                     (format "\"%s\"" (buffer-file-name))
                     (meghanada--write-tmpfile))))
        (when output
          (let ((patchbuf (get-buffer-create "*meghanada-fmt patch*"))
                (tmpfile output))
            (save-excursion
              (widen)
              (with-current-buffer patchbuf
                (erase-buffer))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already formatted")
                  (meghanada--apply-rcs-patch patchbuf)
                  (message "Optimized"))))
            (kill-buffer patchbuf)
            (delete-file tmpfile))))
    (message "client connection not established")))

(defun meghanada-import-all ()
  "Automatically add all import statements.
If there are unimported classes, we will automatically import them as much as possible."
  (interactive)
  (meghanada-import-all-async #'meghanada-import-all--callback (current-buffer) nil))

(defun meghanada-import-at-point ()
  "Add the import statement for the class on the cursor."
  (interactive)
  (meghanada-import-at-point-async #'meghanada-import-at--callback (current-buffer) nil))


;;
;; meghanada complete api
;;

(defun meghanada-autocomplete-prefix-async (prefix callback)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "ap"
                               callback
                               (format "\"%s\"" (buffer-file-name))
                               (meghanada--what-line)
                               (meghanada--what-column)
                               (format "\"%s\"" prefix))
    (message "client connection not established")))

(defun meghanada-autocomplete-resolve-async (type item desc callback)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "cr"
                               callback
                               (format "\"%s\"" (buffer-file-name))
                               (meghanada--what-line)
                               (meghanada--what-column)
                               (format "\"%s\"" type)
                               (format "\"%s\"" item)
                               (format "\"%s\"" desc))
    (message "client connection not established")))


(defun meghanada--local-val-callback (result)
  (let* ((return-type (car result))
         (vals (car (cdr result)))
         (len (length vals)))
    (when (and (not (string= "void" return-type)) (> len 0))
      (back-to-indentation)
      (insert (format "%s  = " return-type))
      (forward-char -3)
      (if (= len 1)
          (insert (car vals))
        (let ((res (completing-read "local variable:" vals nil nil)))
          (unless (string= res "")
            (insert res)))))))

(defun meghanada-local-variable ()
  "Assign a local variable from the return value of an expression."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "lv"
                               #'meghanada--local-val-callback
                               (format "\"%s\"" (buffer-file-name))
                               (meghanada--what-line))
    (message "client connection not established")))

;;
;; meghanada diagnostics api (flycheck)
;;

(defun meghanada-diagnostics-async (callback)
  (if (and meghanada--client-process (process-live-p meghanada--client-process))
      (meghanada--send-request "di"
                               callback
                               (format "\"%s\"" (buffer-file-name)))
    (message "client connection not established")))

(defun meghanada-diagnostic-string-async (callback)
  (let ((buf (buffer-file-name))
        (tmp (meghanada--write-tmpfile)))
    (if (and meghanada--client-process (process-live-p meghanada--client-process))
        (meghanada--send-request "dl"
                                 callback
                                 buf
                                 tmp)
      (message "client connection not established"))))

;;
;; meghanada interactive-command (async)
;;

;; (defun meghanada-parse-file ()
;;   "TODO: FIX DOC ."
;;   (when (meghanada-alive-p)
;;     (if (and meghanada--client-process (process-live-p meghanada--client-process))
;;         (meghanada--send-request "p" #'message (buffer-file-name))
;;       (message "client connection not established"))))

;;
;; meghanada compile-command (async)
;;

(defun meghanada--kill-buf (name)
  (when (get-buffer name)
    (delete-windows-on (get-buffer name))
    (kill-buffer name)))

(defun meghanada--compile-callback (result)
  (let ((severity (car result)))
    (pcase severity
      (`success
       (progn
         (meghanada--kill-buf "*compilation*")
         (message "compile finished")))
      (`error
       (let ((messages (cdr result)))
         (with-current-buffer (get-buffer-create "*compilation*")
           (setq buffer-read-only nil)
           (save-excursion
             (dolist (msg messages)
               (insert (format "%s" msg))
               (open-line 1))
             (goto-char (point-min)))
           (compilation-mode)))))))

(defun meghanada-compile-file ()
  "Compile the current file."
  (interactive)
  (when (meghanada-alive-p)
    (if (and meghanada--server-process (process-live-p meghanada--server-process))
        (let ((buf (format "\"%s\"" (buffer-file-name))))
          (message "compiling ... ")
          (meghanada--kill-buf "*compilation*")
          (pop-to-buffer "*compilation*")
          (meghanada--send-request "c" #'meghanada--compile-callback buf))
      (message "client connection not established"))))

(defun meghanada-compile-project ()
  "Compile the current project."
  (interactive)
  (when (meghanada-alive-p)
    (if (and meghanada--server-process (process-live-p meghanada--server-process))
        (let ((buf  (format "\"%s\"" (buffer-file-name))))
          (message "compiling ... ")
          (meghanada--kill-buf "*compilation*")
          (pop-to-buffer "*compilation*")
          (meghanada--send-request "cp" #'meghanada--compile-callback buf))
      (message "client connection not established"))))

(setq compilation-error-regexp-alist
      (append (list
               ;; works for javac
               '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)
               ;; works for maven
               '("^\\(\\[ERROR\\] \\)?\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 2 3 4)
               '("^\\sw+(\\(\\sw+\\.\\)+\\(\\sw+\\)).+<<< \\(FAILURE\\|ERROR\\)!$"2))
              compilation-error-regexp-alist))


;;
;; meghanada junit api
;;

(defun meghanada--switch-testcase-callback (result)
  (when (and result (file-exists-p result))
    (find-file result)))

(defun meghanada-switch-testcase ()
  "Switch the buffer to the test class of the current class."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "st" #'meghanada--switch-testcase-callback (format "\"%s\"" (buffer-file-name)))
    (message "client connection not established")))


(defun meghanada--setup-task-buffer (buf-name height)
  (when (not (get-buffer-window buf-name))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer buf-name)
          (shrink-window (- h height)))))))

(defun meghanada--junit-callback (ignored)
  "A junit callback dummy function.  IGNORED is not used."
  )

(defun meghanada--run-junit (file debug test)
  (unless (process-live-p meghanada--task-client-process)
    (setq meghanada--task-client-process (meghanada--start-task-client-process)))

  (if meghanada--task-client-process
      (progn
        (meghanada--kill-buf meghanada--task-buf-name)
        (meghanada--kill-buf meghanada--junit-buf-name)
        (setq meghanada--task-buffer meghanada--junit-buf-name)
        (pop-to-buffer meghanada--junit-buf-name)
        (if debug
            (meghanada--send-request-process "dj" meghanada--task-client-process #'meghanada--junit-callback (format "\"%s\"" file) test)
          (meghanada--send-request-process "rj" meghanada--task-client-process #'meghanada--junit-callback (format "\"%s\"" file) test)))
    (message "client connection not established")))

(defun meghanada-run-junit-class ()
  "Run the current test class on JUnit."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (test-name (car (split-string
                          (car (last (split-string file-name "/")))
                          "\\."))))
    (meghanada--run-junit file-name nil test-name)))

(defun meghanada-run-junit-test-case ()
  "Tests the specified test case for the current test class on JUnit."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (class-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\.")))
         (test-case (completing-read "Test case: "
                                     (imenu--make-index-alist t)
                                     nil t
                                     (which-function)))
         (test-name (format "%s#%s" class-name test-case)))
    (meghanada--run-junit file-name nil test-name)))

(defun meghanada-debug-junit-class ()
  "Run the current test class in debug mode on JUnit."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (test-name (car (split-string
                          (car (last (split-string file-name "/")))
                          "\\."))))
    (meghanada--run-junit file-name t test-name)))

(defun meghanada-debug-junit-test-case ()
  "Tests the specified test case for the current test class in debug mode on JUnit.."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (class-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\.")))
         (test-case (completing-read "Test case: "
                                     (imenu--make-index-alist t)
                                     nil t
                                     (which-function)))
         (test-name (format "%s#%s" class-name test-case)))
    (meghanada--run-junit file-name t test-name)))

(defun meghanada-run-junit-recent ()
  "Run the most recent test on JUnit."
  (interactive)
  (meghanada--run-junit (buffer-file-name) nil ""))

(defun meghanada-run-task (args)
  "Runs the tasks of a given maven or gradle project management tool."
  (interactive "sArgs: ")
  (message args)

  (unless (process-live-p meghanada--task-client-process)
    (setq meghanada--task-client-process (meghanada--start-task-client-process)))

  (if (and meghanada--task-client-process (process-live-p meghanada--task-client-process))
      (progn
        (meghanada--kill-buf meghanada--task-buf-name)
        (meghanada--kill-buf meghanada--junit-buf-name)
        (setq meghanada--task-buffer meghanada--task-buf-name)
        (pop-to-buffer meghanada--task-buf-name)
        (meghanada--send-request-process "rt" meghanada--task-client-process #'meghanada--junit-callback args))
    (message "client connection not established")))

(defun meghanada-exec-main ()
  "Execute the Main class."
  (interactive)
  (unless (process-live-p meghanada--task-client-process)
    (setq meghanada--task-client-process (meghanada--start-task-client-process)))

  (if (and meghanada--task-client-process (process-live-p meghanada--task-client-process))
      (let ((file (format "\"%s\"" (buffer-file-name))))
        (meghanada--kill-buf meghanada--task-buf-name)
        (meghanada--kill-buf meghanada--junit-buf-name)
        (setq meghanada--task-buffer meghanada--task-buf-name)
        (pop-to-buffer meghanada--task-buf-name)
        (meghanada--send-request-process "em" meghanada--task-client-process #'meghanada--junit-callback file))
    (message "client connection not established")))

(defun meghanada-debug-main ()
  "Execute the Main class in debug mode."
  (interactive)
  (unless (process-live-p meghanada--task-client-process)
    (setq meghanada--task-client-process (meghanada--start-task-client-process)))

  (if (and meghanada--task-client-process (process-live-p meghanada--task-client-process))
      (let ((file (format "\"%s\"" (buffer-file-name))))
        (meghanada--kill-buf meghanada--task-buf-name)
        (meghanada--kill-buf meghanada--junit-buf-name)
        (setq meghanada--task-buffer meghanada--task-buf-name)
        (pop-to-buffer meghanada--task-buf-name)
        (meghanada--send-request-process "dm" meghanada--task-client-process #'meghanada--junit-callback file))
    (message "client connection not established")))

;;
;; meghanada jump api
;;

(defun meghanada--jump-callback (res)
  (let* ((filename (nth 0 res))
         (line (nth 1 res))
         (col (nth 2 res)))
    (unless (string= filename "")
      (unless (string= filename (buffer-file-name))
        (funcall #'find-file filename))

      (meghanada-mode t)
      (meghanada--goto-line line)
      (beginning-of-line)
      (forward-char (1- col))
      (recenter)
      (if (buffer-modified-p)
          (message "Buffer is modified, file position might not have been correct")))))

(defun meghanada-jump-declaration ()
  "Jump to the definition of the symbol on the cursor."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((sym (meghanada--what-symbol)))
        (when sym
          (meghanada--send-request "jd" #'meghanada--jump-callback
                                   (format "\"%s\"" (buffer-file-name))
                                   (meghanada--what-line)
                                   (meghanada--what-column)
                                   (format "\"%s\"" sym))))
    (message "client connection not established")))

(defun meghanada--list-symbols ()
  "List all available symbols."
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let* ((output (meghanada--send-request-sync "ls")))
        (split-string output "\n"))
    (message "client connection not established")))

(defun meghanada-jump-symbol ()
  "Jump to the specified symbol."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((sym (completing-read "Symbol: " (meghanada--list-symbols) nil nil)))
        (when sym
          (meghanada--send-request "js" #'meghanada--jump-callback
                                   (format "\"%s\"" (buffer-file-name))
                                   (meghanada--what-line)
                                   (meghanada--what-column)
                                   (format "\"%s\"" sym))))
    (message "client connection not established")))

(defun meghanada-back-jump ()
  "Jump to the original position before jumping."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "bj" #'meghanada--jump-callback)
    (message "client connection not established")))

;;
;; meghanada format api
;;

(defun meghanada--write-tmpfile ()
  "Write tmpfile."
  (let ((tmpfile (make-temp-file "meghanada" nil ".java"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (write-region nil nil tmpfile nil 'nomsg))
    tmpfile))

(defun meghanada--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (meghanada--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (meghanada--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in apply-rcs-patch")))))))))

(defun meghanada-code-beautify ()
  "Beautify java code."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((output (meghanada--send-request-sync "fc" (meghanada--write-tmpfile))))
        (when output
          (let ((patchbuf (get-buffer-create "*meghanada-fmt patch*"))
                (tmpfile  output))
            (save-excursion
              (widen)
              (with-current-buffer patchbuf
                (erase-buffer))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already formatted")
                  (meghanada--apply-rcs-patch patchbuf)
                  (message "Applied format"))))
            (kill-buffer patchbuf)
            (delete-file tmpfile))))
    (message "client connection not established")))

(defun meghanada-code-beautify-before-save ()
  "Beautify code before save."
  (when (meghanada-alive-p)
    (meghanada-code-beautify)))

;;
;; meghanada reference api
;;

(defun meghanada--reference-callback (messages)
  "Show reference result."
  (if messages
      (with-current-buffer (get-buffer-create meghanada--ref-buf-name)
        (setq buffer-read-only nil)
        (save-excursion
          (dolist (msg messages)
            (insert (format "%s\n" (nth 0 msg)))))
        (compilation-mode))
    (progn
      (meghanada--kill-buf meghanada--ref-buf-name)
      (message "no reference found"))))

(defun meghanada--reference-prepare ()
  (meghanada--kill-buf meghanada--ref-buf-name)
  (pop-to-buffer meghanada--ref-buf-name)
  (message "searching ..."))

(defun meghanada-reference ()
  "Searches for a reference to a field or method on the cursor."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((sym (meghanada--what-symbol))
            (buf (buffer-file-name))
            (line (meghanada--what-line))
            (col (meghanada--what-column)))
        (when sym
          (progn
            (funcall meghanada-reference-prepare)
            (meghanada--send-request "re" meghanada-reference-callback
                                     (format "\"%s\"" buf)
                                     line
                                     col
                                     (format "\"%s\"" sym)))))
    (message "client connection not established")))

;;
;; meghanada type-info api
;;

(defun meghanada--typeinfo-callback (messages)
  (let ((fqcn (nth 0 messages))
        (classes (nth 1 messages))
        (interfaces (nth 2 messages))
        (members (nth 3 messages))
        (indent 0))
    (if (and fqcn (not (string-empty-p fqcn)))
        (with-help-window (get-buffer-create meghanada--typeinfo-buf-name)
          (save-excursion
            (insert (propertize (format "Class: ")
                                'face '(:weight bold)))
            (insert (format "%s\n\n" fqcn))
            (dolist (c classes)
              (dotimes (number indent 0)
                (insert " "))
              (insert (format "%s\n" c))
              (setq indent (+ indent 2)))
            (when (> (length interfaces) 0)
              (insert "\n")
              (insert (propertize (format "Implements:\n")
                                  'face '(:weight bold)))
              (dolist (it interfaces)
                (insert (format "  %s\n" it))))
            (when (> (length members) 0)
              (insert "\n")
              (insert (propertize (format "Members:\n")
                                  'face '(:weight bold)))
              (dolist (m members)
                (insert (format "  %s\n" (nth 1 m)))))

            (setq buffer-read-only t)))
      (progn
        (meghanada--kill-buf meghanada--typeinfo-buf-name)
        (message "no type found")))))

(defun meghanada--typeinfo-prepare ()
  (meghanada--kill-buf meghanada--typeinfo-buf-name)
  (pop-to-buffer meghanada--typeinfo-buf-name)
  (message "searching ..."))

(defun meghanada-typeinfo ()
  "Show information about the class on the cursor."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((sym (meghanada--what-symbol))
            (buf (buffer-file-name))
            (line (meghanada--what-line))
            (col (meghanada--what-column)))

        (progn
          (funcall meghanada-typeinfo-prepare)
          (meghanada--send-request "ti" meghanada-typeinfo-callback
                                   (format "\"%s\"" buf)
                                   line
                                   col
                                   (format "\"%s\"" sym))))
    (message "client connection not established")))

;;
;; meghanada search everywhere api
;;

(defun meghanada--search-callback (messages)
  "Show search everywhere result."
  (if messages
      (with-current-buffer (get-buffer-create meghanada--search-buf-name)
        (setq buffer-read-only nil)
        (let* ((classes (nth 0 messages))
               (methods (nth 1 messages))
               (symbols (nth 2 messages))
               (usages (nth 3 messages))
               (contents (nth 4 messages)))
          (save-excursion
            (when (> (length classes) 0)
              (insert (propertize (format "Classes: ")
                                  'face '(:weight bold)))
              (insert "\n")
              (dolist (c classes)
                (insert (format "%s\n" c)))
              (insert "\n"))
            (when (> (length methods) 0)
              (insert (propertize (format "Methods: ")
                                  'face '(:weight bold)))
              (insert "\n")
              (dolist (c methods)
                (insert (format "%s\n" c)))
              (insert "\n"))
            (when (> (length symbols) 0)
              (insert (propertize (format "Symbols: ")
                                  'face '(:weight bold)))
              (insert "\n")
              (dolist (c symbols)
                (insert (format "%s\n" c)))
              (insert "\n"))
            (when (> (length usages) 0)
              (insert (propertize (format "Usages: ")
                                  'face '(:weight bold)))
              (insert "\n")
              (dolist (c usages)
                (insert (format "%s\n" c)))
              (insert "\n"))
            (when (> (length contents) 0)
              (insert (propertize (format "Code:\n")
                                  'face '(:weight bold)))
              (dolist (c contents)
                (insert (format "%s\n" c))))
            (compilation-mode))))
    (progn
      (meghanada--kill-buf meghanada--search-buf-name)
      (message "not found"))))

(defun meghanada--search-prepare ()
  (meghanada--kill-buf meghanada--search-buf-name)
  (pop-to-buffer meghanada--search-buf-name))

(defun meghanada--call-search-everywhere (query)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (progn
        (funcall meghanada-search-prepare)
        (meghanada--send-request
         "se"
         meghanada-search-callback
         query))
    (message "client connection not established")))

(defvar meghanada--search-everywhere-last nil)

(defun meghanada--check-searcheverywhere-input ()
  (when (string-prefix-p "*Minibuf" (string-trim (buffer-name)))
    (let* ((content (minibuffer-contents))
           (len (length content)))
      (when (and
             (> len 1)
             (not (string= content meghanada--search-everywhere-last)))
        (setq meghanada--search-everywhere-last content)
        (meghanada--call-search-everywhere content)))))

(defun meghanada--read-search ()
  (let* (timer is-input)
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             0.5
                             'repeat
                             (lambda ()
                               (meghanada--check-searcheverywhere-input)))))
          (prog1 (read-from-minibuffer "Search: ")
            (setq is-input t)))
      (when timer
        (cancel-timer timer)
        (setq timer nil)))))

(defun meghanada--make-search-query (q)
  (format "\"class:*%s* OR method:*%s* OR symbol:%s\"" q q q))

(defun meghanada-search-everywhere ()
  "Search everywhere."
  (interactive)
  (let* ((input (read-from-minibuffer "Search: "))
         (query (funcall meghanada-make-search-query input)))
    (meghanada--call-search-everywhere query)))

(defun meghanada-search-everywhere-ex ()
  "Search everywhere expert mode."
  (interactive)
  (let* ((input (read-from-minibuffer "Lucene query: "))
         (query (format "\"%s\"" input)))
    (meghanada--call-search-everywhere query)))

;;
;; meghanada show project api
;;

(defun meghanada--show-project-callback (msg)
  (with-help-window (get-buffer-create meghanada--show-project-buf-name)
    (pop-to-buffer meghanada--show-project-buf-name)
    (save-excursion
      (setq buffer-read-only nil)
      (insert (format "%s\n" msg))
      (compilation-mode)
      (setq buffer-read-only t))))

(defun meghanada--show-project-prepare ()
  (meghanada--kill-buf meghanada--show-project-buf-name))

(defun meghanada-show-project ()
  "Show more information about the current project."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (progn
        (meghanada--show-project-prepare)
        (meghanada--send-request
         "sp"
         #'meghanada--show-project-callback))
    (message "client connection not established")))


;;
;; meghanada-mode
;;

(defvar meghanada-mode-map
  (let* ((map (make-sparse-keymap))
         (prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "C-c c") 'meghanada-compile-project)
    (define-key prefix-map (kbd "C-c C-c") 'meghanada-compile-file)
    (define-key prefix-map (kbd "C-r o") 'meghanada-optimize-import)
    (define-key prefix-map (kbd "C-r i") 'meghanada-import-all)
    (define-key prefix-map (kbd "C-r r") 'meghanada-local-variable)
    (define-key prefix-map (kbd "C-c t") 'meghanada-run-junit-test-case)
    (define-key prefix-map (kbd "C-c C-t") 'meghanada-run-junit-class)
    (define-key prefix-map (kbd "C-v t") 'meghanada-run-task)
    (define-key map (kbd "C-M-,") 'meghanada-switch-testcase)
    (define-key map meghanada-mode-key-prefix prefix-map)

    map)
  "Keymap for Meghanada-mode.")


(easy-menu-define meghanada-mode-menu meghanada-mode-map
  "Menu for Meghanada mode"
  '("Meghanada"
    ("Test"
     ["Run junit class" meghanada-run-junit-class]
     ["Run junit testcase" meghanada-run-junit-test-case])

    ("Navigation"
     ["Goto declaration" meghanada-jump-declaration]
     ["Go back" meghanada-back-jump]
     ["Goto testcase" meghanada-switch-testcase])

    ("Project"
     ["Run task" meghanada-run-task])

    ("Compile"
     ["Compile file" meghanada-compile-file]
     ["Compile project" meghanada-compile-project])

    ("Refactor"
     ["Format code" meghanada-code-beautify]
     ["Optimize import" meghanada-optimize-import]
     ["Import all" meghanada-import-all]
     ["Introduce local variable" meghanada-local-variable])))


(defun meghanada-change-project ()
  "Change project root."
  (when (meghanada-alive-p)
    (if (and meghanada--client-process (process-live-p meghanada--client-process))
        (meghanada--send-request "pc" #'identity (format "\"%s\"" (buffer-file-name)))
      (message "client connection not established"))))

;;;###autoload
(define-minor-mode meghanada-mode
  "A better java development mode for Emacs (minor-mode).
\\{meghanada-mode-map}"
  nil
  nil
  meghanada-mode-map
  (progn
    (meghanada-install-server)
    (when meghanada-use-company
      (meghanada-company-enable))
    (when meghanada-use-flycheck
      (meghanada-flycheck-enable))
    (when meghanada-use-eldoc
      (meghanada-eldoc-enable))
    (when meghanada-auto-start
      (meghanada-client-connect))
    (meghanada-change-project)
    (add-hook 'xref-backend-functions 'meghanada-xref-backend nil t)
    (run-at-time 2 nil
                 #'(lambda ()
                     (when (member meghanada--install-err-buf-name
                                   (mapcar #'(lambda (b)
                                               (format "%s" b))
                                           (buffer-list)))
                       (pop-to-buffer meghanada--install-err-buf-name))))))

(remove-hook 'java-mode-hook 'wisent-java-default-setup)

(add-to-list 'minor-mode-alist
             '(meghanada-mode (:eval (meghanada-modeline-string))))

(defun meghanada-modeline-string ()
  "Return modeline string."
  (cond ((not (meghanada-alive-p))
         " MEGHANADA:Disconnected")
        ((meghanada-alive-p)
         " MEGHANADA")))

(defun meghanada-xref-backend ()
  "Meghanada xref backend."
  'meghanada)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql meghanada)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql meghanada)))
;;   (if (and meghanada--server-process (process-live-p meghanada--server-process))
;;       (let* ((sym (meghanada--what-symbol))
;;              (buf (buffer-file-name))
;;              (line (meghanada--what-line))
;;              (col (meghanada--what-column))
;;              (output (meghanada--send-request-sync "ti" (format "\"%s\"" buf) line col (format "\"%s\"" sym))))
;;         (let ((members (nth 3 output)))
;;           (mapcar #'(lambda(m) (nth 0 m)) members)))
;;     (message "client connection not established")))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql meghanada))))

(cl-defmethod xref-backend-definitions ((_backend (eql meghanada)) symbol)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (when symbol
        (let ((output (meghanada--send-request-sync "jd"
                                 (format "\"%s\"" (buffer-file-name))
                                 (meghanada--what-line)
                                 (meghanada--what-column)
                                 (format "\"%s\"" symbol))))
          (when output
              (let ((file (nth 0 output))
                    (line (nth 1 output))
                    (column (nth 2 output)))
                (list (xref-make file (xref-make-file-location file line column)))))))
        (message "client connection not established")))

(cl-defmethod xref-backend-references ((_backend (eql meghanada)) symbol)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (let ((buf (buffer-file-name))
            (line (meghanada--what-line))
            (col (meghanada--what-column)))
        (when symbol
          (let ((output (meghanada--send-request-sync "re"  (format "\"%s\"" buf) line col (format "\"%s\"" symbol))))
            (mapcar #'(lambda (m)
                        (let* ((info (nth 1 m))
                               (desc (nth 0 info))
                               (file (nth 1 info))
                               (line (nth 2 info))
                               (column (nth 3 info)))
                          (xref-make desc (xref-make-file-location file line column)))) output))))
  (message "client connection not established")))

;; (cl-defmethod xref-backend-apropos ((_backend (eql meghanada)) pattern)
;;   (message (format "xref-backend-apropos %s" pattern)))

(provide 'meghanada)

;;; meghanada.el ends here
