# Meghanada-Mode

## A Java Develop Environment for Emacs

`meghanada` is a new java-mode (`meghanada-mode`) that aims at improving the editing
experience for the Java. It works by using a combination of an Emacs
package and [meghanada-server][].

<i>WARNING! This is a project beta quality. Under heavy development now.</i>

**Features:**

* [Gradle][] and [Maven][](Beta) project support
* Run build tool task
* Compile your project
* Analyze java source
* Support `Generics`
* Code completion with [company-mode][] (`company-meghanada`)
* Optimize import
* Jump declaration
* Run [Junit][] test (include test runner)
* Diagnostic reporting with [flycheck][] (`flycheck-meghanada`)

`meghanada` tested only `linux` (maybe macOS OK). windows not support.

## Dependencies


### Elisp dependencies

| Package              | Comment                           |
| -------------------- | --------------------------------- |
| `cl-lib`             | Built-in since Emacs 24.3         |
| [company-mode][]     | Optional (company "0.9.0")        |
| [flycheck][]         | Optional (flycheck "0.23")        |

### Meghanada-Server

`meghanada-mode` require [meghanada-server][] and JDK .
[meghanada-server][] provides interface to `meghanada-mode`. It uses a simple protocol
based on S-expression. This server, written in java and requires the following
packages to be installed on your system:

* JDK >= 1.8

## Installation

### Elisp

First add project directory to emacs `load-path`. and write configuration.

TODO install from melpa

#### Configuration

```
(require 'meghanada)

(add-hook 'meghanada-mode-hook
          (lambda ()
            ;; customize
            (setq company-transformers '(company-sort-by-backend-importance))
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . meghanada-mode))
```

### Meghanada-Server

To do so, type `M-x meghanada-install-server RET`. Server program is installed `~/.meghanada/meghanada.jar`.

If you open java file and set major-mode `meghanada-mode`, [meghanada-server][] process autostart and connect to your emacs.

## Usage

Here is a list of available interactive commands.

### meghanada-install-server

Download and install [meghanada-server] jar.

### meghanada-server-start

Start [meghanada-server][] process and connect to server.

`meghanada-mode` call this function by default. when open java file and set `meghanada-mode`.

### meghanada-server-kill

Kill [meghanada-server][] process.

### meghanada-client-direct-connect

Connect to other [meghanada-server][] directly.

### meghanada-client-connect

Connect to [meghanada-server][] .

### meghanada-client-disconnect

Disconnect from [meghanada-server][] .

### meghanada-client-ping

Send ping [meghanada-server][]

### meghanada-clear-cache

Clear [meghanada-server][]'s project cache data.

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

* Fully support [Maven][] project.
* Fully support `lambda expression`
* Fully support `method reference`
* Reformat source (IntelliJ Rule)
* Refactoring

## License

GPL v3, See [LICENSE](LICENSE) file.


[meghanada-server]: https://github.com/mopemope/meghanada-server
[Maven]: http://maven.apache.org/
[Gradle]: https://gradle.org
[company-mode]: http://company-mode.github.io/
[flycheck]: http://flycheck.org
[Junit]: http://www.junit.org/
