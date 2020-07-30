;;; ag-popup-core.el --- The core libraries for ag-popup -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The core libraries for ag-popup.

;;; Code:

(require 'transient)
(require 'ag-popup-file-types)

(defun ag-popup--map-args (transient-args)
  "Convert TRANSIENT-ARGS to a list of args."
  (mapcar
   (lambda (arg)
     (if (listp arg)
         (let ((args (cdr arg)))
           (mapconcat (lambda (x) (concat "--" x)) args " "))
       arg))
   transient-args))

(defvar ag-popup-file-type-list
  '("actionscript" "ada" "asciidoc" "asm" "batch" "bitbake" "bro" "cc" "cfmx"
    "chpl" "clojure" "coffee" "cpp" "crystal" "csharp" "css" "cython" "delphi"
    "dot" "ebuild" "elisp" "elixir" "elm" "erlang" "factor" "fortran" "fsharp"
    "gettext" "glsl" "go" "groovy" "haml" "handlebars" "haskell" "haxe" "hh"
    "html" "ini" "ipython" "jade" "java" "js" "json" "jsp" "julia" "kotlin"
    "less" "liquid" "lisp" "log" "lua" "m4" "make" "mako" "markdown" "mason"
    "matlab" "mathematica" "md" "mercury" "nim" "nix" "objc" "objcpp" "ocaml"
    "octave" "org" "parrot" "perl" "php" "pike" "plist" "plone" "proto" "puppet"
    "python" "qml" "racket" "rake" "restructuredtext" "rs" "r" "rdoc" "ruby"
    "rust" "salt" "sass" "scala" "scheme" "shell" "smalltalk" "sml" "sql"
    "stylus" "swift" "tcl" "tex" "tt" "toml" "ts" "twig" "vala" "vb" "velocity"
    "verilog" "vhdl" "vim" "wix" "wsdl" "wadl" "xml" "yaml")
  "List of supported file types.")

(defun ag-popup-read-file-types (prompt def history)
  "Prompt for Ag file type with PROMPT DEF HISTORY."
  (completing-read-multiple
   prompt
   ag-popup-file-type-list
   nil nil
   nil
   history
   def))

(defun ag-popup-read-pattern (prompt initial-input history)
  "Read a pattern from the minibuffer, prompting with string PROMPT.

If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, if non-nil, specifies a history."
  (read-string prompt initial-input history))

(transient-define-argument ag-popup:-- ()
  "Restrict the search to certain types of files."
  :description "Limit to file types"
  :class 'ag-popup-file-types
  :key "--"
  :argument "--"
  :prompt "Limit to file type(s): "
  :multi-value t
  :reader #'ag-popup-read-file-types)

(transient-define-argument ag-popup:-A ()
  :description "Print lines after match"
  :class 'transient-option
  :shortarg "-A"
  :argument "--after="
  :reader 'transient-read-number-N+)

(transient-define-argument ag-popup:-B ()
  :description "Print lines before match"
  :class 'transient-option
  :shortarg "-B"
  :argument "--before="
  :reader 'transient-read-number-N+)

(transient-define-argument ag-popup:-C ()
  :description "Print lines before and after matches"
  :class 'transient-option
  :shortarg "-C"
  :argument "--context="
  :reader 'transient-read-number-N+)

(transient-define-argument ag-popup:-f ()
  :description "Follow symlinks"
  :shortarg "-f"
  :argument "--follow")

(transient-define-argument ag-popup:-G ()
  :description "Limit search to filenames matching PATTERN"
  :shortarg "-G"
  :argument "--file-search-regex="
  :reader 'ag-popup-read-pattern)

(transient-define-argument ag-popup:-i ()
  :description "Match case insensitively"
  :shortarg "-i"
  :argument "--ignore-case")

(transient-define-argument ag-popup:-m ()
  :description "Skip the rest of a file after NUM matches"
  :class 'transient-option
  :shortarg "-m"
  :argument "--max-count="
  :reader 'transient-read-number-N+)

(transient-define-argument ag-popup:-Q ()
  :description "Don't parse PATTERN as a regular expression"
  :shortarg "-Q"
  :argument "--literal")

(transient-define-argument ag-popup:-s ()
  :description "Match case sensitively"
  :shortarg "-s"
  :argument "--case-sensitive")

(transient-define-argument ag-popup:-S ()
  :description "Match case insensitively unless PATTERN contains uppercase characters"
  :shortarg "-S"
  :argument "--smart-case")

(transient-define-argument ag-popup:-v ()
  :description "Invert match"
  :shortarg "-v"
  :argument "--invert-match")

(transient-define-argument ag-popup:-w ()
  :description "Only match whole words"
  :shortarg "-w"
  :argument "--word-regexp")

(provide 'ag-popup-core)
;;; ag-popup-core.el ends here
