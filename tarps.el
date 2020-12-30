;;; tarps.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 neeasade

;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/tarps
;; Package-Requires: (color-tools base16-theme ht s)
;; Version: 0.1

;;; Commentary:
;; <void>

;;; Code:

(defcustom tarp-org-level-resizing t
  "Set to non-nil for `org-level-*' faces to be different larger
  than the default font height."
  :type 'boolean
  :group 'tarps)

;; idea for another defcustom: a hook for tarp/edit before it's mapped and set

(defun tarp/edit (transform-function)
  "edit the hashtable tarp/theme with a function taking KEY VALUE that returns VALUE"
  (-->
    (lambda (k v)
      (list k (funcall transform-function k v)))
    (ht-map it tarp/theme)
    (eval `(ht ,@ it))
    (setq tarp/theme it)))

(defun tarp/nth (index coll)
  "a version of nth that counts from the end if the input is negative"
  ;; this just makes it easier to sort through color collections in real time.
  (if (< index 0)
    (car (seq-subseq coll index
           (if (= 0 (+ index 1)) nil (+ index 1))))
    (nth index coll)))

(defmacro ht-with-context (table &rest content)
  (-tree-map
    (lambda (tree)
      (-tree-map-nodes (lambda (node) t)
        (lambda (node)
          (if (and
                (s-starts-with-p ":" (prin1-to-string node))
                ;; if the table doesn't exist, don't sanity check the key
                (if (boundp table)
                  (-contains-p (ht-keys (eval table)) node)
                  t))
            (list 'ht-get table node)
            node))
        tree))
    (cons 'progn content)))

(defun tarp/show-contrasts ()
  "message the contrast ratios of colors in the loaded theme against it's background"
  (-map
    (fn
      (message
        (format "%s: %s"
          <>
          (ct/contrast-ratio
            (ht-get tarp/theme <>)
            (ht-get tarp/theme :background)
            ))))
    (ht-keys tarp/theme)))

(defun tarp/map-to-base16 (theme-table)
  (ht-with-context theme-table
    (list
      ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
      ;; what the emacs base16 theme package follows.

      ;; guidelines location: http://chriskempson.com/projects/base16/
      ;; I've also noted some faces I care about

      :base00 :background ;; Default Background

      ;; ivy-current-match background, isearch match foreground, inactive modeline background
      :base01 :background+ ;; Lighter Background (Used for status bars)
      ;; :base01 :background__ ;; Lighter Background (Used for status bars)

      ;; region, active modeline background
      :base02 :background+ ;; Selection Background

      :base03 :foreground_ ;; Comments, Invisibles, Line Highlighting
      :base04 :foreground_ ;; Dark Foreground (Used for status bars)
      :base05 :foreground  ;; Default Foreground, Caret, Delimiters, Operators
      :base06 :foreground_ ;; Light Foreground (Not often used)
      :base07 :foreground_ ;; Light Background (Not often used)

      ;; org-todo, variables
      ;; :base08 :accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
      :base08 :accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

      ;; ivy-current-match foreground
      :base09 :foreground ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

      ;; types
      ;; :base0A :accent1 ;; Classes, Markup Bold, Search Text Background
      :base0A :accent2 ;; Classes, Markup Bold, Search Text Background

      ;; strings
      :base0B :accent2_ ;; Strings, Inherited Class, Markup Code, Diff Inserted

      ;; :base0C :foreground_  ;; Support, Regular Expressions, Escape Characters, Markup Quotes
      :base0C :accent1_ ;; Support, Regular Expressions, Escape Characters, Markup Quotes

      ;; prompt, function-name, search match foreground
      :base0D :accent1 ;; Functions, Methods, Attribute IDs, Headings

      ;; keyword-face, org-date
      :base0E :accent1_ ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

      :base0F :foreground_ ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
      ))
  )

(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory
      (file-name-directory load-file-name))))

(provide 'tarps)
