;;; tarps.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 neeasade

;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/tarps
;; Package-Requires: ((emacs "26.1") (ct "0.1") (helpful "0.19") (fn "0.1.2") (s "1.12.0") (ht "2.3") (base16-theme "1.1"))
;; Version: 0.1

;;; Commentary:

;;; Code:
;; This code has evolved "organically" -- tarp/theme* and tarp/theme are mutable globals assumed to be set by the themes.

(require 'ct)
(require 'helpful)
(require 'ht)
(require 'fn)
(require 's)
(require 'base16-theme)

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
    (-map (fn (format "%s: %s %s"
                <>
                (ct-contrast-ratio (tarp/get <> level) (tarp/get :background level))
                (tarp/get <> level)))
      '(:foreground :faded :primary :assumed :alt :strings)))

  (->>
    '(:normal :weak :strong :focused)

    (-mapcat
      (lambda (level)
        `(,(format "against background %s %s" level (tarp/get :background level))
           ,@(tarp/show-contrast-against level)
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

      ;; this is only used for company scrollbar background
      :base07 (tarp/get :background :strong) ;; Light Background (Not often used)
      ;; :base07 (tarp/get :background :weak) ;; Light Background (Not often used)

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
  `(
     (,face :inverse-video nil)
     (,face :background ,(tarp/get :background back-label))
     (,face :foreground ,(tarp/get (or fore-label :foreground) back-label))))

(defun tarp/theme-get-parent (face label theme-faces)
  ;; search up throught parents of FACE (via :inherit) that firstly has a non-nil LABEL (or return nil)
  ;; theme-faces looks like ((face <plist spec>) (face <plist-spec>)..)
  (let* ((match (first (-filter (fn (eq (first <>) face)) theme-faces)))
          (face (car match))
          (spec (cdr match)))
    (if (plist-member spec label)
      (plist-get spec label)
      (if (plist-member spec :inherit)
        (tarp/theme-get-parent (plist-get spec :inherit) label theme-faces)
        nil))))

(defun tarp/theme-make-faces (theme-colors)
  (let*
    (
      ;; steal the list that's hardcoded in base16-theme-define
      ;; cf https://github.com/belak/base16-emacs/blob/93b1513a9994355492314e809cdbfb0d21f1e30d/base16-theme.el#L186
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

           (whitespace-space :background nil)
           (whitespace-tab :background nil)

           (flycheck-warning :underline nil)
           (flycheck-info :underline nil)

           (secondary-selection :background ,(tarp/get :background :strong))

           ;; TODO: make this optional, it's pretty aggresive
           (org-link :box (:line-width 1
                            :color ,(ct-lessen (tarp/get :faded :normal) 30)
                            ;; :style released-button
                            :style nil
                            ;; (:line-width -1 :style released-button)
                            ))

           ,@(-mapcat (-applify 'tarp/theme-face)
               '(
                  (avy-lead-face :strong :primary)
                  (avy-lead-face-0 :strong :assumed)
                  (avy-lead-face-1 :strong :alt)
                  (avy-lead-face-2 :strong :strings)

                  (completions-common-part :normal :primary)

                  (tooltip :weak)

                  ;; all the org builtin stuff:
                  ;; this assumes sort of a soft alt,
                  ;; faded might be more appropriate,
                  ;; but then intent gets mixed with commenting
                  (org-drawer :normal :alt)
                  (org-meta-line :normal :alt)
                  (org-document-info-keyword :normal :alt)

                  (org-todo :strong :strings)
                  (org-headline-todo :normal)

                  (org-done :weak :faded)
                  (org-headline-done :normal :faded)

                  (isearch :focused)
                  (lazy-highlight :strong)
                  (ivy-match :focused)

                  (org-link :weak)
                  (org-code :weak)
                  ))

           ;; TODO: There should be a user option here to override faces with intents if wanted
           ))

      (new-theme
        ;; apply our individual changes to the original theme
        (-reduce-from
          (lambda (state theme-change)
            (seq-let (face key value) theme-change
              (if (-contains-p (-map 'first state) face)
                (-map
                  (lambda (entry)
                    (if (eq (first entry) face)
                      `(,face ,@(plist-put (cdr entry) key value))
                      entry)
                    )
                  state)
                (cons theme-change state))))
          original-theme
          theme-changes))

      (new-theme-experiment
        ;; idea: auto-conform foreground faces based on found background
        ;; this way we don't have to find where to adjust intensity further
        (-map
          (lambda (face-spec)
            (let*
              ((face (first face-spec))
                (spec (cdr face-spec))

                (background (tarp/theme-get-parent face :background new-theme))
                (foreground (tarp/theme-get-parent face :foreground new-theme))

                (background-color
                  (if (symbolp background)
                    (plist-get theme-colors (intern (format ":%s" (prin1-to-string background))))
                    background))

                (background-symbol
                  (if background-color
                    (plist-get
                      (-flatten
                        (-map
                          (lambda (level)
                            (list (intern (tarp/get :background level)) level))
                          '(:normal :weak :strong :focused)))
                      (intern background-color))
                    :normal))

                (foreground-color
                  (if (symbolp foreground)
                    (plist-get theme-colors (intern (format ":%s" (prin1-to-string foreground))))
                    foreground))

                (foreground-symbol
                  (if foreground-color
                    (plist-get
                      (-flatten
                        (-map
                          (lambda (kind)
                            (list (intern (tarp/get kind)) kind))
                          '(:foreground :faded :primary :alt :assumed :strings)))
                      (intern foreground-color))
                    :foreground))

                (new-foreground
                  (when foreground-symbol
                    (tarp/get foreground-symbol background-symbol))))

              (if (plist-member spec :background)
                `(,face ,@(plist-put spec :foreground (or new-foreground (plist-get spec :foreground))))
                ;; if no background was ever set, just return the OG:
                face-spec)))
          new-theme)))

    ;; original-theme
    ;; new-theme
    new-theme-experiment
    ))

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
