# Version 0.8.2 (2017-06-29)

## Highlights

* Add type information command (meghanada-typeinfo)

# Version 0.8.1 (2017-06-26)

* Fix non-escaped code string used for reference.
* Fix implicit type conversion bugs

# Version 0.8.0 (2017-06-26)

## Highlights

* Support main class execution (meghanada-exec-main).
* Support on the fly syntax checking (and analyze, compile) .
* Support search reference.
* Fix bugs and improve stability.

# Version 0.7.13 (2017-06-12)

* Fix cache update bugs

# Version 0.7.12 (2017-06-11)

* Add customize variable `meghanada-server-jvm-option`

# Version 0.7.11 (2017-06-09)

* Change to use fast-serialization.
* Change to use xodous for cache backend.
* Change not to build gradle subproject by default
* Fix auto change project.
* Fix jump to references to super class's field.
* Fix optimize import is broken.
* Fix bugs and improve stability.

# Version 0.7.10 (2017-05-26)

* Change meghanada-project-compile to fully build.
* Change diagnostic use meghanada-compile-file.
* Fix incorrect compile target file.
* Fix import completion.
* Fix compilation sort.
* Fix bugs and improve stability.

# Version 0.7.9 (2017-05-24)

* Change completion sort to a better sort.
* Fix Sexp parse error.
* Fix crash when displaying eldoc.
* Fix an empty line on optimize import.
* Fix many bugs and improve stability.

# Version 0.7.8 (2017-05-22)

* Fix many bugs and improve stability.

# Version 0.7.7 (2017-05-18)

* Fix many bugs and improve stability.

# Version 0.7.6 (2017-05-16)

* Change sort order of the candidates to more better
* Support maven parent pom
* Change default formatter to google-java-format
* Support make import from eclipse code format settings (meghanadaFormatter.xml)

# Version 0.7.5 (2017-04-27)

* Exclude anonymous class from candidates

# Version 0.7.4 (2017-04-13)

* Fix import all and optimize import bugs
* Fix jump to enum declaration bugs

# Version 0.7.3 (2017-04-03)

* Fix optimize import
* Fix el-doc API bugs

# Version 0.7.2 (2017-03-28)

* Fix NPE
* Improve code fomatter

# Version 0.7.1 (2017-03-23)

* Fix jump to declaration bugs

# Version 0.7.0 (2017-03-18)

## Highlights

* Support eldoc.
* Add server remote-debug flag.

# Version 0.6.6 (2017-03-01)

* Fix jump to declarations bugs.

# Version 0.6.5 (2017-02-28)

* Fix jump to declarations for variadic method.
* Fix parse try resources.
* Supported jump to third party library source.(from sources.jar or decompiled file)

# Version 0.6.4 (2017-02-24)

* Fix jump from method refernce.
* Supported jump to java standard lib source.

# Version 0.6.3 (2017-02-22)

* Fix jump to declarations for overload method.
* Fix wildcard type completion.
* Fix cache error log.

# Version 0.6.2 (2017-02-20)

* Fix Send column number count from 1.
* Fix Send nil symbol.

# Version 0.6.1 (2017-02-17)

* Fix auto-import bugs.
* Fix optimize-import bugs.
* Fix var completion bugs.
* Fix genecrics method completion bugs.
* Fix some bugs.

# Version 0.6.0 (2017-02-13)

## Highlights

* Support android plugin for gradle. (experimental. supported only 2.2.0+)
* Add clear cache mode. (-c option)
* Add class index auto reload.
* Add project auto reload.
* Change cache format.
* Fix nested class completion.
* Fix some bugs.

# Version 0.5.0 (2017-01-31)

## Highlights

* Add code beautify (meghanada-code-beautify-before-save)
* Improve incremental build.
* Fix method reference bugs.

### Code Fomatter

Meghanada uses eclipse code formatter.

Formatter rule file is `meghanadaFormatter.properties`.
`meghanadaFormatter.properties` location should be project root.

Rule detail see below url.

http://help.eclipse.org/neon/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fformatter%2Fpackage-summary.html

# Version 0.4.0 (2017-01-25)

## Highlights

* Improve gradle integration. build with dependency modules (experimental)
* Improve incremental build.
* Fix maven compilation-error-regexp-alist.
* Fix some cache bugs.

# Version 0.3.1 (2017-01-21)

## Highlights

* Fix local variable completion.

# Version 0.3.0 (2017-01-20)

## Highlights

* Use compiler API and improve lambda and method reference support.
* Improve compile and analyze performance.
* Decrease memory usage.
* Fix some bugs.

# Version 0.2.4 (2016-10-18)

## Highlights

* To begin supporting annotaion completion.
* Add autoupdate server module.
