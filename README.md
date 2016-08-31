# Meghanada-Mode

## A Java Develop Environment for Emacs

`meghanada-mode` is a new java-mode that aims at improving the editing
experience for the Java. It works by using a combination of an Emacs
package and `meghanada-server`.

**Features:**

* `Gradle` and `Maven`(Beta) project support
* Run build tool task
* Compile your project
* Analyze java source
* Support Generics
* Code completion with company-mode (`company-meghanada`)
* Optimize import
* Jump declaration
* Run junit test (include test runner)
* Diagnostic reporting with flycheck (`flycheck-meghanada`)

`meghanada-mode` tested only `linux` (maybe macOS OK). windows not support.

## Dependencies


### Elisp dependencies

| Package              | Comment                           |
| -------------------- | --------------------------------- |
| `cl-lib`             | Built-in since Emacs 24.3         |
| `company-mode`       | Optional                          |
| `flycheck`           | Optional                          |

### Meghanada-Server

`meghanada-server` provides interface to `meghanada-mode`.It uses a simple protocol
based on S-expression. This server, written in java and requires the following
packages to be installed on your system:

* jdk >= 1.8

## Installation

### Elisp

Add `meghanada-mode.el` your elisp load path and write settings.
And add `company-meghanada.el` and `flycheck-meghanada.el` load path.

#### Configuration

```
(load-file "path/to/meghanada-mode.el")
;; optional
(load-file "path/to/company-meghanada.el")
;; optional
(load-file "path/to/flycheck-meghanada.el")

(require 'meghanada-mode)
(require 'company-meghanada)
(require 'flycheck-meghanada)

(add-hook 'meghanada-mode-hook
  (lambda ()
    (add-to-list 'company-backends '(company-meghanada :with company-dabbrev-code))
    (setq company-transformers '(company-sort-by-backend-importance))
    (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . meghanada-mode))
```

### Meghanada-Server

To do so, type `M-x meghanada-install-server RET`. server program is installed `~/.meghanada/meghanada.jar`.

If you open java file and set major-mode `meghanada-mode`, `meghanada-server` process autostart and connect to your editor.

## Usage

Here is a list of available interactive commands.

### meghanada-install-server

TODO

### meghanada-server-start

TODO

### meghanada-server-kill

TODO

### meghanada-client-direct-connect

TODO

### meghanada-client-connect

TODO

### meghanada-client-disconnect

TODO

### meghanada-client-ping

TODO

### meghanada-clear-cache

TODO

### meghanada-import-all

TODO

### meghanada-optimize-import

TODO

### meghanada-local-variable

TODO

### meghanada-parse-file

TODO

### meghanada-compile-file

TODO

### meghanada-project-compile

TODO

### meghanada-switch-testcase

TODO

### meghanada-run-junit-class

TODO

### meghanada-run-junit-test-case

TODO

### meghanada-run-junit-recent

TODO

### meghanada-run-task

TODO

### meghanada-jump-declaration

TODO

### meghanada-back-jump


TODO


## TODO

* Fully support `maven` project.
* Fully support `lambda expression`
* Fully support `method reference`
* Reformat source (IntelliJ Rule)
* Refactoring

## License

GPL v3, See [LICENSE](LICENSE) file.
