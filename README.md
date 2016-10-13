# Meghanada-Mode [![MELPA](https://melpa.org/packages/meghanada-badge.svg)](https://melpa.org/#/meghanada) [![MELPA](http://melpa-stable.milkbox.net/packages/meghanada-badge.svg)](http://melpa-stable.milkbox.net/#/meghanada)

[![Join the chat at https://gitter.im/mopemope/meghanada-emacs](https://badges.gitter.im/mopemope/meghanada-emacs.svg)](https://gitter.im/mopemope/meghanada-emacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## A Better Java Development Environment for Emacs

`Meghanada` is a new minor-mode (`meghanada-mode`) that aims at improving the editing
experience for the Java. It works by using a combination of an Emacs
package and [meghanada-server][].

**Features:**

* Easy Install
* [Gradle][] and [Maven][] project support
* No need build tool's plugin
* Run build tool task
* Compile your project
* Analyze java source
* Support `Generics`
* Code completion with [company-mode][] (`company-meghanada`)
* Optimize import
* Jump declaration (only project source)
* Run [Junit][] test (include test runner)
* Diagnostic reporting with [flycheck][] (`flycheck-meghanada`)

`Meghanada` tested only `linux` (maybe macOS OK). windows not support.

## Dependencies

Meghanada has been developed Emacs 24.5.

### Elisp dependencies

| Package              | Version                           |
| -------------------- | --------------------------------- |
| `cl-lib`             | 0.5                               |
| [yasnippet][]        | 0.6.1                             |
| [company-mode][]     | 0.9.0                             |
| [flycheck][]         | 0.23                              |

### Meghanada-Server

`meghanada-mode` require [meghanada-server][] and JDK .
[meghanada-server][] provides interface to `meghanada-mode`. It uses a simple protocol
based on S-expression. This server, written in java and requires the following
packages to be installed on your system:

* JDK >= 1.8

## Installation

### Elisp

Install meghanada from melpa.

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

### Updating

Meghanadauses does not support automatically updade.

If you update `meghanada-mode`, you should update server by typing:

```
M-x meghanada-update-server
```

## Customize

type `M-x customize-group RET meghanada` .

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

### meghanada-switch-testcase (C-M-,)

Switch testcase or source.

### meghanada-run-junit-class (C-c C-c C-t)

Run Junit .

### meghanada-run-junit-test-case (C-c C-c t)

Run Junit testcase (select from imenu).

### meghanada-run-task (C-v t)

Run build tool task.

### meghanada-jump-declaration (M-.)

Goto declaration.

### meghanada-back-jump (M-,)

Go back from declaration position.


## Troubleshooting

See `*meghanada-server-log` buffer.

## TODO

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
