;;; org-block-capf.el --- Org block `completion-at-point' function -*- lexical-binding: t; -*-

;; Author: Alvaro Ramirez
;; Package-Requires: ((emacs "25.1") (org "9.2.0"))
;; URL: https://github.com/xenodium/org-block-capf
;; Version: 0.1

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `completion-at-point' for org blocks using "<" as a trigger.
;;
;; To enable, add `org-block-capf' to `completion-at-point-functions'.
;;
;; Configure edit style via `org-block-capf-edit-style'.
;;
;; Completion candidates are drawn from `org-babel-load-languages'.

;;; Code:

(require 'map)
(require 'org)
(require 'org-element)
(require 'seq)

(defcustom org-block-capf-complete-at-bol t
  "If t, detect completion only at the beginning of lines."
  :type 'boolean)

(defcustom org-block-capf-explicit-lang-defaults t
  "If t, insert org-babel-default-header-args:lang into block header."
  :type 'boolean)

(defcustom org-block-capf-edit-style 'auto
  "Customize how to enter edit mode after block is inserted."
  :type '(choice
          (const :tag "inline: no edit mode invoked after insertion" inline)
          (const :tag "prompt: ask before entering edit mode" prompt)
          (const :tag "auto: automatically enter edit mode" auto)))

(defcustom org-block-capf-auto-indent t
  "If t, automatically indent source block using `org-indent-line'.
Otherwise, insert block at cursor position."
  :type 'boolean)

(defvar org-block-capf--regexp "\\(<\\)\\([^ ]*\\)")

;;;###autoload
(defun org-block-capf ()
  "Function used for `completion-at-point-functions'."
  (when-let* ((activated (looking-back (org-block-capf--regexp)
                                       (line-beginning-position)))
              (range (or (bounds-of-thing-at-point 'symbol)
                         (cons (point) (point))))
              (end (cdr range))
              (start (car range)))
    (list
     start
     end
     (completion-table-dynamic
      (lambda (_)
        (org-block-capf--all-candidates)))
     :exclusive 'no
     :exit-function
     (lambda (insertion _status)
       (when (seq-contains-p (org-block-capf--all-candidates) insertion)
         (org-block-capf--expand insertion t))))))

(defun org-block-capf-add-to-completion-at-point-functions ()
  "Add `org-block-capf' to `completion-at-point-functions'."
  (let ((capf #'org-block-capf))
    (unless (memq capf completion-at-point-functions)
      (add-hook 'completion-at-point-functions capf nil 'local))))

(defun org-block-capf--regexp ()
  (if org-block-capf-complete-at-bol
      (concat "^[[:space:]]*" org-block-capf--regexp)
    org-block-capf--regexp))

(defun org-block-capf--all-candidates ()
  (seq-sort
   #'string-lessp
   (seq-uniq
    (append
     (org-block-capf--languages)
     (org-block-capf--templates)
     (org-block-capf--languages-from-extensions)))))

(defun org-block-capf--languages ()
  "Get language names."
  (mapcar #'prin1-to-string
          ;; Filter out non-symbols.
          (seq-filter
           (lambda (item)
             (symbolp item))
           (map-keys org-babel-load-languages))))

(defun org-block-capf--templates ()
  "Get template names."
  ;; Filter out non-strings (pre org 9.2 templates)
  ;; https://github.com/xenodium/org-block-capf/issues/7
  (seq-filter
   #'stringp
   (map-values org-structure-template-alist)))

(defun org-block-capf--languages-from-extensions ()
  "Get language names from extensions."
  (seq-filter
   #'stringp
   (map-keys org-babel-tangle-lang-exts)))

(defun org-block-capf--template-p (template)
  "Check if there is a TEMPLATE available for completion."
  (seq-contains (map-values org-structure-template-alist)
                template))

(defun org-block-capf--expand (insertion query-lang)
  "Replace INSERTION with generated source block."
  (delete-region (point) (- (point) (1+ ;; Include "<" in length.
                                     (length insertion))))
  ;; If < trigger generated a matching >, delete it.
  (when (looking-at ">")
    (delete-char 1))
  (cond ((and query-lang (string-equal insertion "src"))
         ;; src templates have no associated language. Ask user for one.
         (org-block-capf--wrap-point (format "src %s%s"
                                                (read-string "Language: ")
                                                (if org-block-capf-explicit-lang-defaults
                                                    (org-block-capf--lang-header-defaults insertion)
                                                  ""))
                                        "src"))
        ((org-block-capf--template-p insertion)
         (org-block-capf--wrap-point insertion
                                        ;; May be multiple words.
                                        ;; Take the first one.
                                        (nth 0 (split-string insertion))))
        (t
         (org-block-capf--wrap-point (format "src %s%s"
                                                insertion
                                                (if org-block-capf-explicit-lang-defaults
                                                    (org-block-capf--lang-header-defaults insertion)
                                                  ""))
                                        "src"))))

(defun org-block-capf--wrap-point (begin end)
  "Wrap point with block using BEGIN and END.  For example:
#+begin_BEGIN
  |
#+end_END"
  (when org-block-capf-auto-indent
    (org-indent-line))
  (insert (format "#+begin_%s\n\n" begin))
  (insert (format "#+end_%s" end))
  (beginning-of-line)
  (org-indent-line)
  (line-move -1)
  (insert (make-string org-edit-src-content-indentation ?\s))
  (cond ((and (eq org-block-capf-edit-style 'auto)
              (org-block-capf--edit-src-code-p))
         ;; Only enter major mode if there's a language recognized for it.
         (when (org-element-property :language (org-element-at-point))
           (org-edit-src-code)))
        ((and (eq org-block-capf-edit-style 'prompt)
              (org-block-capf--edit-src-code-p)
              (yes-or-no-p "Edit now?"))
         (org-edit-src-code))))

(defun org-block-capf--edit-src-code-p ()
  "Return t if `edit-src-code' can edit in a separate major mode."
  (memq (org-element-type (org-element-at-point))
        '(example-block src-block)))

(defun org-block-capf--lang-header-defaults (lang)
  "Resolve and concatenate all header defaults for LANG.

For example: \"python\" resolves to:

\((:exports . \"both\")
  (:results . \"output\"))

and returns:

\" :exports both :results output\""
  (let ((lang-headers-var (intern
                           (concat "org-babel-default-header-args:" lang))))
    (if (boundp lang-headers-var)
        (seq-reduce (lambda (value element)
                      (format "%s %s %s"
                              value
                              (car element)
                              (cdr element)))
                    (eval lang-headers-var t) "")
      "")))

(provide 'org-block-capf)

;;; org-block-capf.el ends here
