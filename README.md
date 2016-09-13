# Meghanada-Mode

## A Better Java Development Environment for Emacs

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

Meghanada-Mode has been developed Emacs 24.5.

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
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))
```

### Meghanada-Server

To do so, type `M-x meghanada-install-server RET`. Server program is installed `~/.meghanada/meghanada.jar`.

If you open java file and set `meghanada-mode`, [meghanada-server][] process start automatically and connect to your emacs.

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

### meghanada-import-all (C-c C-r i)

Add import all unqualified classes.

### meghanada-optimize-import (C-c C-r o)

Import all and remove unused import statement.

### meghanada-local-variable (C-c C-r r)

Introduce local variable. (need expression is end `;`)


### meghanada-compile-file (C-c C-c C-c)

Compile file.

### meghanada-project-compile (C-c C-c c)

Compile project.

### meghanada-switch-testcase (M-,)

Switch testcase or source.

### meghanada-run-junit-class (C-c C-c C-t)

Run Junit .

### meghanada-run-junit-test-case (C-c C-c t)

Run Junit testcase (select from imenu).

### meghanada-run-task (C-v t)

Run build tool task.

### meghanada-jump-declaration (M-.)

Goto declaration.

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
