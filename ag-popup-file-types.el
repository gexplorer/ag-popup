;;; ag-popup-file-types.el --- Class used for the "--" argument of ag-popup -*- lexical-binding: t; -*-

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

;; Class used for the "--" argument of ag-popup.

;;; Code:

(require 'transient)

(defclass ag-popup-file-types (transient-infix) ()
  "Class used for the \"--\" argument.
All remaining arguments are treated as file types.
They become the value of this argument.")

(cl-defmethod transient-format-value ((obj ag-popup-file-types))
  "Format OBJ's value for display and return the result."
  (let ((argument (oref obj argument)))
    (if-let ((value (oref obj value)))
        (propertize (mapconcat (lambda (f) (concat argument f))
                               (oref obj value) " ")
                    'face 'transient-argument)
      (propertize argument 'face 'transient-inactive-argument))))

(cl-defmethod transient-init-value ((obj ag-popup-file-types))
  "Set the initial value of the object OBJ."
  (oset obj value
        (cdr (assoc "--" (oref transient--prefix value)))))

(cl-defmethod transient-infix-value ((obj ag-popup-file-types))
  "Return (concat ARGUMENT VALUE) or nil.

ARGUMENT and VALUE are the values of the respective slots of OBJ.
If VALUE is nil, then return nil.  VALUE may be the empty string,
which is not the same as nil."
  (when-let ((value (oref obj value)))
    (cons (oref obj argument) value)))

(provide 'ag-popup-file-types)
;;; ag-popup-file-types.el ends here
