# Meghanada-Mode [![MELPA](https://melpa.org/packages/meghanada-badge.svg)](https://melpa.org/#/meghanada) [![MELPA](http://melpa-stable.milkbox.net/packages/meghanada-badge.svg)](http://melpa-stable.milkbox.net/#/meghanada)

[![Join the chat at https://gitter.im/mopemope/meghanada-emacs](https://badges.gitter.im/mopemope/meghanada-emacs.svg)](https://gitter.im/mopemope/meghanada-emacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## A Better Java Development Environment for Emacs

`Meghanada` is a new minor-mode (`meghanada-mode`) that aims at improving the editing
experience for the Java. It works by using a combination of an Emacs
package and [meghanada-server][].

**Features:**

* Easy install
* Auto update server module
* [Gradle][] and [Maven][] project support
* No need build tool's plugin
* Run build tool task
* Compile your project
* Analyze java source
* Support `Generics`
* Code completion with [company-mode][] (`company-meghanada`)
* Optimize import
* Jump declaration
* Run [Junit][] test (include test runner)
* Diagnostic reporting with [flycheck][] (`flycheck-meghanada`)
* Show symbol's type info with `el-doc`

`Meghanada` tested only `linux` (maybe macOS OK). windows not support.

(Welcome contributions !)

## Dependencies

Meghanada has been developed Emacs 25.1.1

### Elisp dependencies

| Package              | Version                           |
| -------------------- | --------------------------------- |
| `cl-lib`             | 0.5                               |
| [yasnippet][]        | 0.6.1                             |
| [company-mode][]     | 0.9.0                             |
| [flycheck][]         | 0.23                              |

### Meghanada-Server

`meghanada-mode` require [meghanada-server][] and JDK (not JRE).
[meghanada-server][] provides an interface to `meghanada-mode`. It uses a simple protocol
based on S-expressions. This server, written in java, requires the following
packages to be installed on your system:

* JDK 1.8

The Meghanada architecture is almost the same as `ensime`. It is client server model.

Meghanada updates any information when saving and compile the java file.

If the completion candidate and others are incorrect, please fix the compile error.


## Installation

### Elisp

Install meghanada from melpa.

#### Configuration

```elisp
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
```

### Meghanada-Server

The server will be automatically installed to `~/.emacs/meghanada/meghanada-x.x.jar`.

If you open a java file and set `meghanada-mode`, a [meghanada-server][] process starts automatically and connects to your emacs.

### Updating

The server will be automatically updated.To manually update bellows, type

```
M-x meghanada-update-server
```

## Auto completion

Auto completion is supported by `comapany-mode`.

## Syntax/error checking

Error checking is supported by `flycheck`.

## Customize

To customize other aspects of its behavior, type `M-x customize-group RET meghanada` .

## Usage

Here is a list of available interactive commands.

### meghanada-install-server

Download and install [meghanada-server] jar.

### meghanada-update-server

Download and update [meghanada-server] jar. and restart server process.

### meghanada-server-start

Start [meghanada-server][] process and connect to server.

`meghanada-mode` call this function by default. when open java file and set `meghanada-mode`.

### meghanada-restart

Restart [meghanada-server][] process and client process.

### meghanada-server-kill

Kill [meghanada-server][] process.

### meghanada-client-direct-connect

Connect to other [meghanada-server][] directly.

### meghanada-client-connect

Connect to [meghanada-server][] . Call automatically at the start `meghanada-mode`.

### meghanada-client-disconnect

Disconnect from [meghanada-server][] .

### meghanada-client-ping

Send ping [meghanada-server][]

### meghanada-clear-cache

Clear [meghanada-server][]'s project cache data.

### meghanada-import-all (C-c C-r i)

Add import all unqualified classes.

### meghanada-optimize-import (C-c C-r o)

Import all and remove unused import statement.

The format of the import statement will be in `google-java-format`.

### meghanada-local-variable (C-c C-r r)

Introduce local variable. (need expression is end `;`)

### meghanada-compile-file (C-c C-c C-c)

Compile file (and related files).

### meghanada-project-compile (C-c C-c c)

Compile project (full build).

### meghanada-switch-testcase (C-M-,)

Switch testcase or source.

### meghanada-exec-main

Run main class.

### meghanada-reference

Find usage (method call, field access, class).

### meghanada-run-junit-class (C-c C-c C-t)

Run Junit test class.

### meghanada-run-junit-test-case (C-c C-c t)

Run Junit testcase (select from imenu).

### meghanada-run-task (C-v t)

Run build tool task.

### meghanada-jump-declaration (M-.)

Goto declaration.

### meghanada-back-jump (M-,)

Go back from declaration position.

### meghanada-code-beautify

Format buffer code. The default formatter is `google-java-format`

## Customize code formatter

`meghanada-mode` uses `google-java-format` by default for formatter.

It also supports eclipse formatter.
If you want to customize the formatter, you uses the exported eclipse format settings.
To import the settings, change the name of the exported file to `meghanadaFormatter.xml` and put it in the project root.

## Troubleshooting

See `*meghanada-server-log*` buffer. or `/tmp/meghanada_server.log`.

Please press `C-g` when emacs seems to hang.

## Setting example

```elisp
(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")

  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("C-z" . hydra-meghanada/body))

  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
"
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _r_: meghanada-run-junit-recent
_g_: magit-status
_l_: helm-ls-git-ls
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)

  ("g" magit-status)
  ("l" helm-ls-git-ls)

  ("t" meghanada-run-task)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("r" meghanada-run-junit-recent)

  ("q" exit)
  ("z" nil "leave"))
```

## TODO

* Refactoring

## Contributing

Contributions are extremely welcome! Please push PR to `dev` branch.

## License

GPL v3, See [LICENSE](LICENSE) file.


[meghanada-server]: https://github.com/mopemope/meghanada-server
[Maven]: http://maven.apache.org/
[Gradle]: https://gradle.org
[company-mode]: http://company-mode.github.io/
[flycheck]: http://flycheck.org
[Junit]: http://www.junit.org/
[yasnippet]: http://joaotavora.github.io/yasnippet/
