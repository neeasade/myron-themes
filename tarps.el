;;; tarps.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 neeasade

;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/tarps
;; Package-Requires: (color-tools base16-theme ht s helpful fn)
;; Version: 0.1

;;; Commentary:
;; <void>

;;; Code:

(require 'cl-macs)

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
  (->>
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
      )
    (-partition 2)
    (-map (lambda (pair)
            (cl-destructuring-bind (label key) pair
              (list label (ht-get theme-table key)))))
    (-flatten)))

(defun tarp/get-function-sexp (sym)
  "get SYM as a quoted list, using helpful.el"
  (read
    (helpful--source sym t
      (first (helpful--definition sym t))
      (second (helpful--definition sym t)))))

(defun tarp/base16-theme-define (theme-table theme-name)
  ;; get the whitespace face:
  (require 'whitespace)
  (let*
    (
      ;; add our theme colors to the color plist
      (theme-colors
        (append
          (tarp/map-to-base16 theme-table)
          (ht-to-plist theme-table)))

      ;; steal the list that's hardcoded in base16-theme-define
      (original-theme (->>
                        (tarp/get-function-sexp 'base16-theme-define)
                        (nth 4)
                        (nth 3)
                        (second)))

      ;; note individual changes
      (theme-changes
        `(
           (avy-lead-face :background accent1_)
           (avy-lead-face-0 :background accent1)
           (avy-lead-face-2 :background accent2)

           ;; face pace-part value
           ;; value may be a key from ns/theme
           (font-lock-comment-delimiter-face :foreground foreground_)
           (isearch :foreground background+)
           (isearch :background foreground)
           (comint-highlight-prompt :foreground foreground)
           (fringe :background nil)
           ;; (mode-line :background nil)
           (font-lock-comment-face :background nil)
           (magit-diff-context-highlight :background
             ,(ct/lessen 4 (ht-get tarp/theme :background)))
           (window-divider :foreground foreground_)

           ;; match variables to functions
           ;; (font-lock-function-name-face :foreground :accent2)
           (font-lock-variable-name-face :foreground accent1)

           ;; consider nulling out and using flat newlines org links
           ;; (org-link :foreground :accent1_)
           ;; (font-lock-type-face :foreground :accent1)

           (org-todo :background background_)
           (org-done :background background_)

           (org-todo :foreground accent2_)
           (org-done :foreground accent2)

           (org-date :underline nil)
           (org-date :foreground accent1_)

           (org-drawer :foreground accent1_)
           (org-block-begin-line :foreground foreground_)
           (org-block-end-line :foreground foreground_)

           (org-level-1 :foreground foreground)
           (org-level-2 :foreground foreground)
           (org-level-3 :foreground foreground)
           (org-level-4 :foreground foreground)
           (org-level-5 :foreground foreground)
           (org-level-6 :foreground foreground)

           (org-headline-done :foreground foreground)
           (org-headline-done :background nil)
           (org-special-keyword :foreground foreground_)

           (whitespace-space :background nil)
           (whitespace-tab :background nil)
           ;; (whitespace-newline background nil)
           (flycheck-warning :underline nil)
           (flycheck-info :underline nil)
           ))

      (new-theme
        ;; apply our individual changes to the original theme
        (-reduce-from
          (lambda (state theme-change)
            (cl-destructuring-bind (face key value) theme-change
              (if (-contains-p (-map 'first state) face)
                (-map
                  (lambda (entry)
                    (if (eq (first entry) face)
                      (append
                        (list face)
                        (plist-put (cdr entry) key value))
                      entry))
                  state)
                (cons theme-change state))))
          original-theme
          theme-changes)))

    ;; do the thing
    (base16-set-faces theme-name theme-colors new-theme)

    ;; other (the rest of the original base16-theme-define):

    ;; Anything leftover that doesn't fall neatly into a face goes here.
    (let ((base00 (plist-get theme-colors :base00))
           (base01 (plist-get theme-colors :base01))
           (base02 (plist-get theme-colors :base02))
           (base03 (plist-get theme-colors :base03))
           (base04 (plist-get theme-colors :base04))
           (base05 (plist-get theme-colors :base05))
           (base06 (plist-get theme-colors :base06))
           (base07 (plist-get theme-colors :base07))
           (base08 (plist-get theme-colors :base08))
           (base09 (plist-get theme-colors :base09))
           (base0A (plist-get theme-colors :base0A))
           (base0B (plist-get theme-colors :base0B))
           (base0C (plist-get theme-colors :base0C))
           (base0D (plist-get theme-colors :base0D))
           (base0E (plist-get theme-colors :base0E))
           (base0F (plist-get theme-colors :base0F)))
      (custom-theme-set-variables
        theme-name
        `(ansi-color-names-vector
           ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
           [,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05]))

      ;; Emacs 24.3 changed ’ansi-term-color-vector’ from a vector of colors
      ;; to a vector of faces.
      (when (version< emacs-version "24.3")
        (custom-theme-set-variables
          theme-name
          `(ansi-term-color-vector
             ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
             [unspecified ,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05]))))
    ))

(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory
      (file-name-directory load-file-name))))

(provide 'tarps)
