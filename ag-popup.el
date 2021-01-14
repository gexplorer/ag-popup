;;; ag-popup.el --- Interactive search with ag -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; URL: https://github.com/gexplorer/ag-popup
;; Keywords: convenience, matching, tools
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0"))

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

;; This package provides a transient popup to configure the
;; arguments of "ag - The Silver Searcher".

;; When calling `ag-popup' you will get a popup to easily configure
;; the arguments and performing the search will execute the function
;; defined in `ag-popup-function'.

;; This means that this package doesn't perform the actual search
;; but the good news is that it will be compatible with all ag
;; implementations.

;; For example, to use `counsel-ag':

;; (defun ag-popup-counsel (directory args)
;;   (counsel-ag "" directory (mapconcat 'identity args " ")))

;; (setq ag-popup-function #'ag-popup-counsel)

;;; Code:

(require 'transient)
(require 'ag-popup-core)

(defgroup ag-popup nil
  "Interactive search with ag."
  :group 'tools
  :group 'matching)

(defcustom ag-popup-function 'ignore
  "Function to perform the search.

This function must take two parameters: the first one is the
directory, the second one is a list of args for the search."
  :type 'function)

;;;###autoload
(defun ag-popup-search-here ()
  "Search using `ag-popup-function' in the current directory with selected args."
  (interactive)
  (ag-popup-search default-directory))

;;;###autoload
(defun ag-popup-search (directory)
  "Search using `ag-popup-function' in a given DIRECTORY with selected args."
  (interactive "DDirectory: ")
  (let ((ag-args (ag-popup--map-args (transient-args 'ag-popup))))
    (funcall ag-popup-function directory ag-args)))

;;;###autoload
(transient-define-prefix ag-popup ()
  "Search popup using `ag'."
  ["Output options"
   (ag-popup:-A)
   (ag-popup:-B)
   (ag-popup:-C)]
  ["Search options"
   (ag-popup:-f)
   (ag-popup:=h)
   (ag-popup:-G)
   (ag-popup:-i)
   (ag-popup:-m)
   (ag-popup:-Q)
   (ag-popup:-s)
   (ag-popup:-S)
   (ag-popup:-v)
   (ag-popup:-w)
   (ag-popup:--)]
  ["Search"
   ("s" "in current directory" ag-popup-search-here)
   ("o" "in other directory" ag-popup-search)])

(provide 'ag-popup)
;;; ag-popup.el ends here
