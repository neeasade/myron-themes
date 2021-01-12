;;; tarps.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 neeasade

;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/tarps
;; Package-Requires: (color-tools base16-theme ht s helpful fn)
;; Version: 0.1

;;; Commentary:
;; This code has evolved "organically" -- tarp/theme* and tarp/theme are mutable globals assumed to be set by the themes.

;;; Code:

(require 'helpful)
(require 'ht)
(require 'fn)
(require 's)
(require 'base16-theme)
(require 'ct "colort-tools.el")

(defcustom tarp/tweak-function nil
  "A function hook that allows you to tweak the colorscheme before it is mapped to faces"
  :type 'function
  :group 'tarps)

(defun tarp/get (label &optional emphasis)
  (ht-get* tarp/theme* (or emphasis :normal) label))

(defun tarp/set (label emphasis value)
  (ht-set! (ht-get tarp/theme* emphasis) label value))

(defun tarp/nth (index coll)
  "a version of nth that counts from the end if the input is negative"
  ;; this just makes it easier to sort through color collections in real time.
  (if (< index 0)
    (nth (- (abs index) 1) (reverse coll))
    (nth index coll)))

(defun tarp/show-contrasts ()
  (interactive)
  "message the contrast ratios of colors in the loaded theme against it's backgrounds"

  (defun tarp/show-contrast-against (level)
    "show contrast levels of fg colors in tarp/theme against BG."
    (-map (fn (format "%s: %s" <>
                (ct-contrast-ratio
                  (tarp/get <> level) (tarp/get :background level))))
      '(:foreground :faded :primary :assumed :alt :strings)))

  (->>
    '(:normal :weak :strong :focused)

    (-mapcat
      (fn
        `(,(format "against background %s %s" <> (tarp/get :background <>))
           ,@(tarp/show-contrast-against (tarp/get <>))
           "------")))
    (s-join "\n")
    (message)))


(defun tarp/map-to-base16 (&optional level-in)
  (let ((level (or level-in :normal)))
    (list
      ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
      ;; what the emacs base16 theme package follows.

      ;; guidelines location: http://chriskempson.com/projects/base16/
      ;; I've also noted some faces I care about

      :base00 (tarp/get :background level) ;; Default Background

      ;; ivy-current-match background, isearch match foreground, inactive modeline background
      :base01 (tarp/get :background :focused) ;; Lighter Background (Used for status bars)
      ;; :base01 :background__ ;; Lighter Background (Used for status bars)

      ;; region, active modeline background
      :base02 (tarp/get :background :focused) ;; Selection Background

      :base03 (tarp/get :faded level)      ;; Comments, Invisibles, Line Highlighting
      :base04 (tarp/get :faded level)      ;; Dark Foreground (Used for status bars)
      :base05 (tarp/get :foreground level) ;; Default Foreground, Caret, Delimiters, Operators
      :base06 (tarp/get :faded level)      ;; Light Foreground (Not often used)

      ;; note: This is just used for company background -- maybe change it to a background value
      :base07 (tarp/get :faded level) ;; Light Background (Not often used)

      ;; org-todo, variables
      ;; :base08 :accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
      :base08 (tarp/get :alt level) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

      ;; ivy-current-match foreground
      :base09 (tarp/get :foreground level) ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

      ;; types
      ;; :base0A :accent1 ;; Classes, Markup Bold, Search Text Background
      :base0A (tarp/get :alt level) ;; Classes, Markup Bold, Search Text Background

      ;; strings
      :base0B (tarp/get :strings level) ;; Strings, Inherited Class, Markup Code, Diff Inserted

      ;; :base0C :foreground_  ;; Support, Regular Expressions, Escape Characters, Markup Quotes
      :base0C (tarp/get :assumed level) ;; Support, Regular Expressions, Escape Characters, Markup Quotes

      ;; prompt, function-name, search match foreground
      :base0D (tarp/get :primary level) ;; Functions, Methods, Attribute IDs, Headings

      ;; keyword-face, org-date
      :base0E (tarp/get :assumed level) ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

      :base0F (tarp/get :faded level) ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
      )))

(defun tarp/get-function-sexp (sym)
  "get SYM as a quoted list, using helpful.el"
  (read
    (helpful--source sym t
      (first (helpful--definition sym t))
      (second (helpful--definition sym t)))))

(defun tarp/theme-face (face back-label &optional fore-label)
  `((,face :inverse-video nil)
     (,face :background ,(tarp/get :background back-label))
     (,face :foreground ,(tarp/get (or fore-label :foreground) back-label))))

(defun tarp/theme-make-faces (theme-colors)
  (let*
    (
      ;; steal the list that's hardcoded in base16-theme-define
      (original-theme (->>
                        (tarp/get-function-sexp 'base16-theme-define)
                        (nth 4)
                        (nth 3)
                        (second)))

      ;; note individual changes
      (theme-changes
        `(
           (magit-diff-context-highlight :background
             ,(tarp/get :background :weak))

           (font-lock-comment-delimiter-face :foreground faded)

           (comint-highlight-prompt :foreground foreground)

           (fringe :background nil)

           (font-lock-comment-face :background nil)


           (window-divider :foreground faded)

           ;; match variables to functions
           (font-lock-function-name-face :foreground primary)
           (font-lock-variable-name-face :foreground primary)

           (org-date :underline nil)
           (org-level-1 :foreground foreground)
           (org-level-2 :foreground foreground)
           (org-level-3 :foreground foreground)
           (org-level-4 :foreground foreground)
           (org-level-5 :foreground foreground)
           (org-level-6 :foreground foreground)

           ;; todo: review:

           ;; (org-block-begin-line :foreground foreground_)
           ;; (org-block-end-line :foreground foreground_)
           ;; (org-special-keyword :foreground foreground_)

           ;; (org-date :foreground accent1_)
           ;; (org-drawer :foreground accent1_)
           ;; (font-lock-type-face :foreground :accent1)

           (whitespace-space :background nil)
           (whitespace-tab :background nil)

           ;; (whitespace-newline background nil)
           (flycheck-warning :underline nil)
           (flycheck-info :underline nil)

           (org-link :box (:line-width 1
                            :color ,(ct-lessen (tarp/get :faded :normal) 30)
                            ;; :style released-button
                            :style nil
                            ;; (:line-width -1 :style released-button)
                            ))

           ,@(-mapcat
               (lambda (ok) (apply 'tarp/theme-face ok))
               '(
                  ;; org-headline-done
                  (avy-lead-face :strong :primary)
                  (avy-lead-face-0 :strong :assumed)
                  (avy-lead-face-1 :strong :alt)
                  (avy-lead-face-2 :strong :strings)

                  (org-todo :strong :strings)
                  (org-headline-todo :normal)

                  (org-done :weak :faded)
                  (org-headline-done :normal :faded)

                  (isearch :focused)
                  (lazy-highlight :strong)
                  (ivy-match :focused)
                  (org-link :weak :alt)
                  (org-code :weak)
                  ))
           ))

      (new-theme
        ;; apply our individual changes to the original theme
        (-reduce-from
          (lambda (state theme-change)
            (let
              ((face (first theme-change))
                (key (second theme-change))
                (value (third theme-change)))
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
    new-theme))

(defun tarp/base16-theme-define (theme-name)
  (when (-non-nil tarp/tweak-function)
    (funcall tarp/tweak-function))

  (let ((theme-colors (append
                        (tarp/map-to-base16)
                        (ht-to-plist tarp/theme))))

    (base16-set-faces
      theme-name
      theme-colors
      (tarp/theme-make-faces theme-colors))

    (when (boundp 'evil-normal-state-cursor)
      (let ((c (plist-get theme-colors :assumed)))
        (setq
          evil-normal-state-cursor `(,c box)
          evil-insert-state-cursor `(,c bar)
          evil-visual-state-cursor `(,c box))))

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
             [unspecified ,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05]))))))

(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory
      (file-name-directory load-file-name))))

(provide 'tarps)
