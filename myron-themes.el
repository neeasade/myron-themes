;;; myron-themes.el --- color themes for emacs -*- coding: utf-8; lexical-binding: t -*-
;; Copyright (C) 2022 neeasade
;; SPDX-License-Identifier: MIT
;; Author: neeasade <neeasade@gmail.com>
;; URL: https://github.com/neeasade/myron-themes
;; Package-Requires: ((emacs "26.1") (ct "0.2") (fn "0.1.2") (helpful "0.19") (s "1.12.0") (ht "2.3") (base16-theme "3.0"))
;; Version: 0.1

;;; Commentary:
;; A set of color themes built on top of the base16-themes.

;;; Code:

(require 'base16-theme)
(require 'ct)
(require 'helpful)
(require 'ht)
(require 'fn)
(require 's)

(defgroup myron nil
  "Myron color themes for EMACS."
  :group 'myron
  :prefix "myron-")

(defcustom myron-use-cache t
  "Using the cache means you get the colors as authored -- turning it off means compute them on your machine."
  :type 'boolean
  :group 'myron)

(defun myron-get (label &optional emphasis)
  (ht-get* myron-theme* (or emphasis :normal) label))

(defun myron-set (label emphasis value)
  (ht-set! (ht-get myron-theme* emphasis) label value))

(defun myron-show-contrast-against (bg-level)
  "Show contrast levels of fg colors in myron-theme* against BG-LEVEL."
  (-map (lambda (fg-label)
          (-let* ((foreground (myron-get fg-label bg-level))
                   (background (myron-get :background bg-level))
                   (contrast-ratio (ct-contrast-ratio foreground background)))
            (format "%s %s %s" contrast-ratio foreground fg-label)))
    '(:foreground :faded :primary :assumed :alt :strings)))

(defun myron-show-contrasts ()
  "Message the contrast ratios of colors of different background emphasis levels."
  (interactive)
  (->> '(:normal :weak :strong :focused)
    (-map (-juxt 'identity (-partial 'myron-get :background)))
    (-mapcat
      (-lambda ((bg-level bg-color))
        `(,(format "Against background %s %s" bg-level bg-color)
           ,@(myron-show-contrast-against bg-level)
           "------")))
    (s-join "\n")
    (message)))

(defun myron-theme-to-base16 (&optional emphasis)
  "Turn myron-theme* into a base16 plist using EMPHASIS (default :normal)."
  (-let* ((emphasis (or emphasis :normal))
           (background-f (myron-get :background :focused))
           ((&hash :alt :strings :assumed :primary :faded :foreground :background) (ht-get myron-theme* emphasis)))
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

(defun myron--face-derive-from (parent face)
  "Make FACE derive from PARENT."
  (->> `(:foreground nil
          :background nil
          :underline nil
          :height unspecified
          :inherit ,parent
          :box unspecified)
    (-partition 2)
    (-map (-partial 'cons face))))

(defun myron--get-function-sexp (sym)
  "Get SYM as a quoted list, using helpful.el."
  (read
    (seq-let (buffer point _) (helpful--definition sym t)
      (helpful--source sym t buffer point))))

(defun myron--markup-map ()
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

(defun myron-theme-make-faces (theme-colors)
  (let*
    (
      ;; steal the list that's hardcoded in base16-theme-define
      ;; cf https://github.com/belak/base16-emacs/blob/93b1513a9994355492314e809cdbfb0d21f1e30d/base16-theme.el#L186
      (original-theme (->> (myron--get-function-sexp 'base16-theme-define)
                        (nth 4)
                        (nth 3)
                        (-second-item)))

      ;; note individual changes
      (theme-changes
        `(
           (fringe :background nil)

           (font-lock-comment-delimiter-face :foreground faded)
           (font-lock-comment-face :background nil)

           ((window-divider vertical-border) :foreground faded)

           ;; ｉｄｅｎｔｉｔｙ
           (font-lock-function-name-face :foreground primary)
           (font-lock-variable-name-face :foreground primary)

           ((outline-1 outline-2 outline-3 outline-4 outline-5) :foreground foreground)
           ((whitespace-space whitespace-tab) :background nil)
           ((org-date flycheck-warning flycheck-info) :underline nil)

           (secondary-selection :background ,(myron-get :background :strong))

           (parenthesis :foreground ,(myron-get :faded))

           (magit-diff-context-highlight :background ,(myron-get :background :weak))

           ;; magit fiddling is much harder than you would expect
           ;; magit-diff-hunk-heading is more like a "sections" concept, with an active one.
           (magit-diff-hunk-heading-highlight :background
             ,(-> (myron-get :background :strong)
                (ct-edit-hsluv-h (ct-get-hsluv-h (myron-get :strings)))
                (ct-edit-hsluv-s 5)))

           (magit-diff-hunk-heading :background
             ,(-> (myron-get :background :weak)
                (ct-edit-hsluv-h (ct-get-hsluv-h (myron-get :strings)))
                (ct-edit-hsluv-s 5)))

           ,@(-let* ((green (ct-make-hsluv 120 70 (ct-get-hsluv-l (myron-get :background))))
                      ;; a little oomf
                      (red (ct-make-hsluv 0 70 (- (ct-get-hsluv-l (myron-get :background)) 5)))
                      (light-delta (apply '- (-map 'ct-get-hsluv-l
                                               (list
                                                 (myron-get :background :normal)
                                                 (myron-get :background :weak)))))
                      (light-delta (* 0.7 light-delta))
                      (dark-green (ct-edit-hsluv-l-dec green light-delta))
                      (dark-red (ct-edit-hsluv-l-dec red light-delta))
                      ((green red dark-green dark-red) (--map (ct-edit-hsluv-l-dec it 0.5) (list green red dark-green dark-red))))
               `((magit-diff-added :background ,green)
                  (magit-diff-removed :background ,red)
                  (magit-diff-added-highlight :background ,dark-green)
                  (magit-diff-removed-highlight :background ,dark-red)))

           ,@(-mapcat
               (-lambda ((face back-label fore-label))
                 (->> `(:inverse-video nil
                         :background ,(myron-get :background back-label)
                         :foreground ,(myron-get (or fore-label :foreground) back-label))
                   (-partition 2)
                   (-map (-partial 'cons face))))
               `(
                  (avy-lead-face :strong :primary)
                  (avy-lead-face-0 :strong :assumed)
                  (avy-lead-face-1 :strong :alt)
                  (avy-lead-face-2 :strong :strings)

                  (eros-result-overlay-face :strong)
                  (cider-result-overlay-face :strong)

                  (completions-common-part :normal :primary)
                  (comint-highlight-prompt :normal :assumed)

                  (tooltip :weak)

                  (lsp-ui-sideline-global :weak :alt)
                  (lsp-ui-sideline-current-symbol :weak :alt)

                  (company-scrollbar-bg :strong :faded)

                  (magit-blame-heading :weak)

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

                  (isearch :focused)
                  (ivy-current-match :focused)
                  (lazy-highlight :strong)

                  (org-link :weak :alt)
                  (org-code :weak)

                  (org-block :weak)
                  (org-block-begin-line :normal)
                  (org-block-end-line :normal)

                  (show-paren-match :focused)
                  (show-paren-match-expression :focused)

                  (line-number :weak :faded)
                  (line-number-current-line :focused)))

           ;; inheritors
           ,@(-mapcat
               (-lambda ((parent children))
                 (-mapcat (-partial #'myron--face-derive-from parent)
                   (-list children)))
               (-partition 2
                 `(
                    ,@(myron--markup-map)

                    magit-diff-removed (whitespace-line)

                    ;; make cider inline test faces similar to magit
                    ;; (abusing for consistency)
                    magit-diff-removed (cider-test-failure-face cider-test-error-face)
                    magit-diff-added cider-test-success-face

                    default smerge-markers

                    magit-diff-added (diff-added smerge-upper)
                    magit-diff-removed (diff-removed smerge-lower)

                    magit-diff-added-highlight (diff-refine-added smerge-refined-added)
                    magit-diff-removed-highlight (diff-refine-removed smerge-refined-removed)

                    ;; todo: this doesn't look the best/needs a revisit
                    magit-diff-hunk-heading-highlight (diff-function diff-file-header)
                    magit-diff-hunk-heading (diff-header diff-hunk-header)
                    )))))

      ;; allow multi-face conf
      (theme-changes (-mapcat (-lambda ((faces . kvs))
                                (-map (lambda (face) `(,face ,@kvs))
                                  (-list faces)))
                       theme-changes))

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

(defun myron-evil-cursor-color (color)
  "Syncronize the the evil cursor COLOR across cursor states."
  (setq evil-normal-state-cursor `(,color box)
    evil-insert-state-cursor `(,color bar)
    evil-visual-state-cursor `(,color box)))

(defun myron-theme-define (theme-name create-fn cached-colors)
  "Implementation of `base16-theme-define` with myron face list"
  (setq myron-theme* (if myron-use-cache cached-colors (funcall create-fn)))

  (-let* ((colors (append (myron-theme-to-base16) (ht-to-plist (ht-get myron-theme* :normal))))
           (faces (myron-theme-make-faces colors)))

    (base16-theme-set-faces theme-name colors faces)
    (myron-evil-cursor-color (plist-get colors :primary))

    ;; Anything leftover that doesn't fall neatly into a face goes here.
    (custom-theme-set-variables theme-name
      `(ansi-color-names-vector
         ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
         ,(->> '(:base00 :base08 :base0B :base0A :base0D :base0E :base0D :base05)
            (-map (-partial #'plist-get colors))
            (apply 'vector))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
          (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
      (or (and (file-directory-p dir) dir)
        base))))

(provide 'myron-themes)
;;; myron-themes.el ends here
