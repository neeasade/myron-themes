;;; myron-themes.el --- A collection of light color themes -*- coding: utf-8; lexical-binding: t -*-
;; Copyright (C) 2025 neeasade
;; SPDX-License-Identifier: MIT
;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/myron-themes
;; Package-Requires: ((emacs "25.1") (ct "0.3") (helpful "0.19") (ht "2.3") (base16-theme "3.0"))
;; Version: 0.1

;;; Commentary:
;; A set of color themes built on top of the base16-themes.

;;; Code:

(require 'base16-theme)
(require 'ct)
(require 'helpful)
(require 'ht)
(require 'myron-themes-cache)

(defvar myron-themes-colors nil "The color values for the loaded myron theme.")

(defgroup myron-themes nil
  "Myron color themes for EMACS."
  :group 'myron-themes
  :prefix "myron-themes")

(defcustom myron-themes-use-cache t
  "A toggle to use the colors as authored.

To compute the colors on your machine, set to nil."
  :type 'boolean
  :group 'myron)

(defun myron-themes-get (label &optional background-emphasis)
  "Get the color value of LABEL on some BACKGROUND-EMPHASIS."
  (or (ht-get* myron-themes-colors (or background-emphasis :normal) label)
    (when (not background-emphasis) (ht-get* myron-themes-colors :meta label))))

(defun myron-themes--show-contrast-against (bg-level)
  "Show contrast levels of fg colors in myron-themes-colors against BG-LEVEL."
  (-map (lambda (fg-label)
          (-let* ((foreground (myron-themes-get fg-label bg-level))
                   (background (myron-themes-get :background bg-level))
                   (contrast-ratio (ct-contrast-ratio foreground background)))
            (format "%s %s %s" contrast-ratio foreground fg-label)))
    '(:foreground :faded :primary :assumed :alt :strings)))

(defun myron-themes-show-contrasts ()
  "Message the contrast ratios of colors of different background emphasis levels."
  (interactive)
  (->> '(:normal :weak :strong :focused)
    (-map (-juxt 'identity (-partial 'myron-themes-get :background)))
    (-mapcat
      (-lambda ((bg-level bg-color))
        `(,(format "Against background %s %s" bg-level bg-color)
           ,@(myron-themes--show-contrast-against bg-level)
           "------")))
    (--map (concat it "\n"))
    (apply 'concat)
    (message)))

(defun myron-themes-to-base16 (&optional emphasis)
  "Return `myron-themes-colors' colors as a base16 plist using EMPHASIS."
  (-let* ((emphasis (or emphasis :normal))
           (background-f (myron-themes-get :background :focused))
           ((&hash :alt :strings :assumed :primary :faded :foreground :background) (ht-get myron-themes-colors emphasis)))
    ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
    ;; what the emacs base16 theme package follows (observations commented following ":").
    ;; guidelines location: http://chriskempson.com/projects/base16/
    (list
      :base00 background   ;; Default Background
      :base01 background-f ;; search match, Lighter Background (Used for status bars)           : search bg
      :base02 background-f ;; Selection Background
      :base03 faded        ;; Comments, Invisibles, Line Highlighting
      :base04 faded        ;; Dark Foreground (Used for status bars)                            : paren-face
      :base05 foreground   ;; Default Foreground, Caret, Delimiters, Operators
      :base06 faded        ;; Light Foreground (Not often used)
      :base07 background   ;; Light Background (Not often used)                                 : nb. unused
      :base08 alt          ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted : org-todo
      :base09 foreground   ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url
      :base0A alt          ;; Types, Classes, Markup Bold, Search Text Background
      :base0B strings      ;; Strings, Inherited Class, Markup Code, Diff Inserted
      :base0C assumed      ;; Support, Regular Expressions, Escape Characters, Markup Quotes
      :base0D primary      ;; Functions, Methods, Attribute IDs, Headings                       : search fg, prompt
      :base0E assumed      ;; Keywords, Storage, Selector, Markup Italic, Diff Changed          : org-date, keyword
      :base0F faded        ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?> : used as fg in various smol places
      )))

(defun myron-themes--get-function-sexp (sym)
  "Get SYM as a quoted list, using helpful.el."
  (read (seq-let (buffer point _) (helpful--definition sym t)
          (helpful--source sym t buffer point))))

(defun myron-themes--markup-map ()
  "List of org faces to markup-faces for non-org modes."
  '(
     ;; HEADINGS
     org-level-1 (adoc-title-0-face markdown-header-face-1 rst-level-1)
     org-level-2 (adoc-title-1-face markdown-header-face-2 rst-level-2)
     org-level-3 (adoc-title-2-face markdown-header-face-3 rst-level-3)
     org-level-4 (adoc-title-3-face markdown-header-face-4 rst-level-4)
     org-level-5 (adoc-title-4-face markdown-header-face-5 rst-level-5)

     ;; the leading #'s of the headings in markdown
     ;; using org-level-4 so it's not thicc on nested markdown headings
     org-level-4 (markdown-header-delimiter-face)

     ;; INLINE CODE
     ;; nb: rst-literal applies for both inline-code and blocks of code
     org-code (adoc-typewriter-face markdown-inline-code-face)

     ;; BLOCKS OF CODE
     org-block (adoc-verbatim-face adoc-native-code-face adoc-code-face markdown-code-face rst-literal)

     ;; BUILTINS/META
     bold (adoc-bold-face markdown-bold-face rst-emphasis2)
     italic (adoc-emphasis-face rst-emphasis1)

     org-meta-line (adoc-complex-replacement-face
                     adoc-meta-hide-face
                     markdown-markup-face
                     adoc-markup-face   ; maybe?
                     adoc-meta-face
                     rst-directive
                     rst-adornment)

     ;; non-org modes make the distinction between labels and destinations
     ;; org mode when showing link markup treats url/dest as default face, so I'm choosing to match that
     default (adoc-internal-reference-face markdown-url-face)
     org-link (adoc-reference-face
                markdown-link-face
                rst-reference
                rst-external
                lui-button-face
                button)

     ;; org list elements have no face
     default (markdown-list-face adoc-list-face rst-block)

     org-checkbox markdown-gfm-checkbox-face))

(defun myron-themes--make-faces (&optional theme-overrides)
  "Make the face->color map for the current theme.

Optionally transform colors with a THEME-OVERRIDES function."
  (let*
    (
      ;; steal the list that's hardcoded in base16-theme-define
      ;; cf https://github.com/belak/base16-emacs/blob/93b1513a9994355492314e809cdbfb0d21f1e30d/base16-theme.el#L186
      (original-theme (->> (myron-themes--get-function-sexp 'base16-theme-define)
                        (nth 4)
                        (nth 3)
                        (-second-item)))

      ;; todo: consider it might be more natural to create normal_faded,
      ;; strong_background aliases instead of calling myron-themes-get througout
      ;; this section (eg consistency with theme-overrides)

      ;; note individual changes
      (theme-changes
        `(
           (cursor :background primary)
           (fringe :background unspecified)

           (font-lock-comment-delimiter-face :foreground faded)
           (font-lock-comment-face :background unspecified)

           ((window-divider vertical-border) :foreground faded)

           ;; ｉｄｅｎｔｉｔｙ
           (font-lock-function-name-face :foreground primary)
           (font-lock-variable-name-face :foreground primary)

           ((outline-1 outline-2 outline-3 outline-4 outline-5) :foreground foreground)
           ((whitespace-space whitespace-tab) :background unspecified)
           ((org-date flycheck-warning flycheck-info) :underline unspecified)

           (secondary-selection :background ,(myron-themes-get :background :strong))

           (parenthesis :foreground faded)

           (prescient-primary-highlight :foreground alt)
           ;; maybe this should be assumed or primary
           (prescient-secondary-highlight :foreground strings)

           ((orderless-match-face-0 orderless-match-face-1 orderless-match-face-2 orderless-match-face-3)
             :foreground alt)

           (completions-common-part :foreground ,(myron-themes-get :alt :weak)) ; weak for corfu popup

           (magit-diff-context-highlight :background ,(myron-themes-get :background :weak))

           ;; todo: this appears to not be doing anything
           ;; (magit-diff-file-heading :extend t)
           (corfu-bar :background faded)
           (company-tooltip-scrollbar-thumb :background faded)
           (company-tooltip-scrollbar-track :background ,(myron-themes-get :background :weak))

           (completions-annotations :foreground faded)

           ((magit-diff-hunk-heading magit-diff-hunk-heading-highlight) :extend unspecified)

           (magit-diff-hunk-heading :background ,(myron-themes-get :background :strong))
           (magit-diff-hunk-heading-highlight :background ,(myron-themes-get :background :focused))
           (magit-diff-added :background ,(myron-themes-get :diff-add))
           (magit-diff-removed :background ,(myron-themes-get :diff-remove))
           (magit-diff-added-highlight :background ,(myron-themes-get :diff-add-highlight))
           (magit-diff-removed-highlight :background ,(myron-themes-get :diff-remove-highlight))

           ,@(-map
               (-lambda ((face back-label fore-label))
                 (->> `(,face :inverse-video nil
                         :background ,(myron-themes-get :background back-label)
                         :foreground ,(myron-themes-get (or fore-label :foreground) back-label))))
               `(
                  ;; (avy-lead-face :strong :primary)
                  ;; (avy-lead-face-0 :strong :assumed)
                  ;; (avy-lead-face-1 :strong :alt)
                  ;; (avy-lead-face-2 :strong :strings)

                  ;; maybe button? a button-active notion?
                  (avy-lead-face :strong :assumed)
                  (avy-lead-face-0 :strong :assumed)
                  (avy-lead-face-1 :strong :assumed)
                  (avy-lead-face-2 :strong :assumed)

                  (eros-result-overlay-face :strong)
                  (cider-result-overlay-face :strong)

                  (comint-highlight-prompt :normal :assumed)

                  (tooltip :weak)

                  (lsp-ui-sideline-global :weak :alt)
                  (lsp-ui-sideline-current-symbol :weak :alt)

                  (magit-blame-heading :weak)

                  ;; focus bois
                  ,@(--map (list it :focused)
                      '(show-paren-match show-paren-match-expression
                         line-number-current-line
                         corfu-current ivy-current-match isearch))

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

                  (mode-line-inactive :weak)
                  (mode-line :strong)

                  (lazy-highlight :strong)

                  (org-link :weak :alt)
                  (org-code :weak)

                  (org-block :normal)
                  (org-block-begin-line :normal)
                  (org-block-end-line :normal)

                  ;; todo: only want this if we can get (:extend t) to work above
                  ;; (magit-diff-file-heading :strong)

                  (diff-header :strong)
                  (diff-hunk-header :strong)
                  (diff-function :strong)
                  (diff-file-header :weak)

                  (line-number :weak :faded)))

           ;; inheritors
           ,@(-mapcat
               (-lambda ((parent children))
                 (--map (apply 'list it :inherit parent
                          (--mapcat (list it 'unspecified)
                            '(:foreground :background :underline :box :height)))
                   (-list children)))
               (-partition 2
                 `(
                    ,@(myron-themes--markup-map)

                    tooltip (corfu-default)

                    ;; diff consistency:
                    default smerge-markers

                    magit-diff-added (diff-added smerge-upper)
                    magit-diff-removed (diff-removed smerge-lower)

                    magit-diff-added-highlight (diff-refine-added smerge-refined-added)
                    magit-diff-removed-highlight (diff-refine-removed smerge-refined-removed)

                    ;; abuse diff colors in other contexts for consistency:

                    ;; success:
                    magit-diff-added cider-test-success-face

                    ;; failure:
                    magit-diff-removed (whitespace-line)
                    magit-diff-removed-highlight (cider-error-overlay-face
                                                   cider-test-error-face
                                                   cider-test-failure-face
                                                   error
                                                   show-paren-mismatch))))

           ,@theme-overrides))

      ;; allow multi-face, multi-attr conf
      (theme-changes
        (->> theme-changes
          (-mapcat (-lambda ((faces . kvs))
                     (-map (lambda (face) `(,face ,@kvs))
                       (-list faces))))
          (-mapcat (-lambda ((face . kvs))
                     (-map (lambda (kv) `(,face ,@kv))
                       (-partition 2 kvs))))))

      (new-theme
        ;; apply our individual changes to the original theme
        (-reduce-from
          (-lambda (state (face key value))
            (if (-contains-p (-map #'-first-item state) face)
              (-map (lambda (entry)
                      (if (eq (-first-item entry) face)
                        `(,face ,@(plist-put (cdr entry) key value))
                        entry))
                state)
              (cons (list face key value) state)))
          original-theme
          theme-changes)))

    ;; original-theme
    new-theme))



(defun myron-themes-termcolors ()
  "Export current myron theme to a list of terminal colors."
  ;; stealing default base16 export order for now
  (--map (plist-get (myron-themes-to-base16) it)
    '(:base00 :base08 :base0B :base0A :base0D :base0E :base0C :base05
       :base03 :base09 :base01 :base02 :base04 :base06 :base0F :base07)))

(defun myron-themes--create-meta-colors (colors)
  "Add the meta color table to COLORS."

  (ht-set! colors
    :meta
    (-let* (((bg bg-weak _) (--map (ht-get* colors it :background) '(:normal :weak :strong)))
             ;; (color-strings (ht-get* colors :normal :strings))
             ;; (strings-hue (ct-get-hsluv-h (ht-get* colors :normal :strings)))
             (green (ct-make-hsluv 120 70 (ct-get-hsluv-l bg)))
             ;; a little oomf
             (red (ct-make-hsluv 0 70 (- (ct-get-hsluv-l bg) 5)))
             (light-delta (apply '- (-map 'ct-get-hsluv-l (list bg bg-weak))))
             (light-delta (* 0.7 light-delta))
             (dark-green (ct-edit-hsluv-l-dec green light-delta))
             (dark-red (ct-edit-hsluv-l-dec red light-delta))
             ((green red dark-green dark-red) (--map (ct-edit-hsluv-l-dec it 0.5)
                                                (list green red dark-green dark-red))))
      (ht<-plist
        `(:diff-add ,green
           :diff-remove ,red
           :diff-add-highlight ,dark-green
           :diff-remove-highlight ,dark-red
           ;; :interactive-background ,(ct-aedit-hsluv bg-weak (list strings-hue 5 l))
           ;; :interactive-background-highlight ,(ct-aedit-hsluv bg-strong (list strings-hue 5 l))
           ))))
  colors)

(defun myron-themes--define (theme-name &optional theme-overrides)
  "Define theme THEME-NAME, optionally change faces with function THEME-OVERRIDES."
  (setq myron-themes-colors (if myron-themes-use-cache
                              (plist-get myron-themes-cache theme-name)
                              (myron-themes--create-meta-colors
                                (funcall (intern (format "%s-create" theme-name))))))

  (base16-theme-set-faces theme-name
    (append (myron-themes-to-base16) (ht-to-plist (ht-get myron-themes-colors :normal)))
    (myron-themes--make-faces theme-overrides))

  (custom-theme-set-variables theme-name
    `(ansi-color-names-vector ,(apply 'vector (-take 8 (myron-themes-termcolors))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
          (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
      (or (and (file-directory-p dir) dir)
        base))))

(provide 'myron-themes)
;;; myron-themes.el ends here
