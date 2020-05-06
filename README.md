# Meghanada-Mode [![MELPA](https://melpa.org/packages/meghanada-badge.svg)](https://melpa.org/#/meghanada) [![MELPA](http://melpa-stable.milkbox.net/packages/meghanada-badge.svg)](http://melpa-stable.milkbox.net/#/meghanada)

[![Patreon](https://img.shields.io/badge/patreon-become%20a%20patron-red.svg)](https://www.patreon.com/mopemope)

## A Better Java Development Environment for Emacs

`Meghanada` is a new minor-mode (`meghanada-mode`) that aims at improving the editing
experience for Java. It works by using a combination of an Emacs
package and [meghanada-server][].

**Features:**

- Easy install
- Auto-update server module
- [Gradle][] and [Maven][] and [Eclipse][] project support
- No need build tool's plugin
- Run build tool task
- Compile your project
- Syntax check and analyze java source (`flycheck-meghanada`)
- Support `Generic Types` and `Lambda`
- Code completion with [company-mode][] (`company-meghanada`)
- Optimize import and sort
- Find definitions and references using builtin [xref][]
- Run [JUnit][] test (include test runner)
- Diagnostic reporting with [flycheck][] (`flycheck-meghanada`)
- Show symbol's type info with `el-doc`
- Full-featured text search

`Meghanada` is tested under `Linux`, `Windows` and `macOS`.

(Welcome contributions !)

## Dependencies

Meghanada has been developed Emacs 25.1.1

### Elisp dependencies

| Package          | Version |
| ---------------- | ------- |
| `cl-lib`         | 0.5     |
| [yasnippet][]    | 0.6.1   |
| [company-mode][] | 0.9.0   |
| [flycheck][]     | 0.23    |

### Language Server

`meghanada-mode` require [meghanada-server][] and JDK (not JRE).

[meghanada-server][] is The language server.
The language server provides a simple interface between `meghanada-mode` and It is a protocol based on a simple S-expression.
This server is developed in Java and supports Java version 8 or later.

The architecture between the server and the client is very similar to the language server protocol.
The server retrieves and updates all information in the Java project at file save time and at compile time.
If the autocompletion information is not displayed correctly, please try to fix the compilation error first.

## Installation

### Elisp

Install meghanada from melpa.

#### Configuration

An example is shown below.

```elisp
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))
```

### Meghanada-Server

The server will be automatically installed to `~/.emacs.d/meghanada/meghanada-x.x.jar`.

If you open a java file and set `meghanada-mode`, a [meghanada-server][] process starts automatically and connects to your Emacs.

### Updating

The server will be automatically updated. To manually update bellows, type

```
M-x meghanada-update-server
```

### Manual Server Installation

The server will be installed and updated automatically.

If you set it up manually, please follow the instructions below.

#### Download meghanada-setup.jar

A meghanada-setup.jar is a small program that automatically updates the server.

This can be downloaded from:

https://github.com/mopemope/meghanada-server/releases/download/v1.0.13/meghanada-setup-0.0.2.jar

Move the downloaded setup.jar to .emacs.d/meghanada/.
Start Emacs and enable meghanada-mode.

## Autocompletion

Autocompletion is supported by company-mode.
Supported autocompletions are as follows

- Import Class
- Class
- Method
- Field
- Local Variable

If the completion candidates are not displayed correctly, try to fix the compilation error first.
Auto-completion matching is optional. Try typing the following to find out more.

`M-x customize-group RET meghanada` and `M-x customize-group RET company-meghanada`.

## Syntax/error checking

Syntax checks, such as compile errors, are supported by `flycheck`.
This includes `-Xlint` warnings.

## Code formatter

`meghanada-mode` uses `google-java-format` by default for formatter.

It also support [Eclipse][] formatter.

If you want to customize and use the [Eclipse][] formatter please export the formatter settings from [Eclipse][] to a file.

Then rename the file to `meghanadaFormatter.xml` and place it in the project root or subproject root.
`Meghanada` will automatically load and import it.

If you want to use a system wide formatter, deploy the file to `$HOME/.emacs.d/meghanada/meghanadaFormatter.xml`.

## Debug

`meghanada-mode` has no debugger yet. However, the external debugger is supported.

if you want to debug your code, execute `meghanada-debug-xxxx` function and then use [realgud][], etc.

```
# in Emacs now:
 M-x meghanada-debug-junit-class
 Please wait ...
 M-x realgud:jdb
 Run jdb (like this): jdb -attach 6006
 Set uncaught java.lang.Throwable
   Set deferred uncaught java.lang.Throwable
   Initializing jdb ...
   >
   VM Started: No frames on the current call stack

   main[1] stop at example.MyClass:100 // set breakpoint
   main[1] run
```

## Customize

To customize other aspects of its behavior, type `M-x customize-group RET meghanada` and `M-x customize-group RET company-meghanada`.

## Troubleshooting

See `*meghanada-server-log*` buffer. or `(temporary-file-directory)meghanada_server_(user-uid).log`, e.g. `/tmp/meghanada_server_1000.log`.

Please press `C-g` when emacs seems to hang.

## Usage

Here is a list of available interactive commands.

Definitions and references can be found from the [xref][] command.

### meghanada-install-server

Download and install [meghanada-server] jar.

### meghanada-update-server

Download and update [meghanada-server] jar. and restart the server process.

### meghanada-server-start

Start [meghanada-server][] process and connect to the server.

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

### meghanada-import-at-point

Add import statement from symbol at point.

### meghanada-local-variable (C-c C-r r)

Introduce local variable. (need expression is end `;`)

### meghanada-compile-file (C-c C-c C-c)

Compile file (and related files).

### meghanada-project-show

Show project details.

### meghanada-project-compile (C-c C-c c)

Compile project (full build and reindex).

### meghanada-switch-testcase (C-M-,)

Switch test case or source.

### meghanada-exec-main

Run main class.

### meghanada-debug-main

Debug main class.

### meghanada-search-everywhere

It does a full-text search based search (class, method, symbol (field)).

### meghanada-search-everywhere-ex

It does a full-text search expert-mode (Enter a query for Lucene).

Here is a list of fields that can be used in the search.

- package
- class
- method
- symbol
- usage
- dc (method's or field's declaring class)

ex. Seach class name contains `Search`.

```
class:*Search*
```

It can also use `AND operator`.

```
dc:*Optional* AND usage:get
```

### meghanada-typeinfo

Show type hierarchies and implements interfaces.

### meghanada-run-junit-class (C-c C-c C-t)

Run JUnit test class.

### meghanada-run-junit-test-case (C-c C-c t)

Run JUnit test case (select from imenu).

### meghanada-debug-junit-class (C-c C-c C-t)

Debug JUnit test class.

### meghanada-debug-junit-test-case (C-c C-c t)

Debug JUnit test case (select from imenu).

### meghanada-run-task (C-c C-v t)

Run build tool task.

### meghanada-code-beautify

Format buffer code. The default formatter is `google-java-format`

## TODO

- Refactoring

## Contributing

Contributions are extremely welcome! Please push PR to `dev` branch.

## License

GPL v3, See [LICENSE](LICENSE) file.

[meghanada-server]: https://github.com/mopemope/meghanada-server
[maven]: http://maven.apache.org/
[gradle]: https://gradle.org
[eclipse]: https://www.eclipse.org/
[company-mode]: http://company-mode.github.io/
[flycheck]: https://www.flycheck.org/
[junit]: http://www.junit.org/
[yasnippet]: http://joaotavora.github.io/yasnippet/
[realgud]: https://github.com/realgud/realgud
[xref]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
