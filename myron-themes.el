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
  (-let [emphasis (or emphasis :normal)]
    (list
      ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
      ;; what the emacs base16 theme package follows.

      ;; guidelines location: http://chriskempson.com/projects/base16/
      ;; I've also noted some faces I care about

      :base00 (myron-get :background emphasis) ;; Default Background

      ;; ivy-current-match background, isearch match foreground, inactive modeline background
      :base01 (myron-get :background :focused) ;; Lighter Background (Used for status bars)
      ;; :base01 :background__ ;; Lighter Background (Used for status bars)

      ;; region, active modeline background
      :base02 (myron-get :background :focused) ;; Selection Background

      :base03 (myron-get :faded emphasis)      ;; Comments, Invisibles, Line Highlighting
      :base04 (myron-get :faded emphasis)      ;; Dark Foreground (Used for status bars)
      :base05 (myron-get :foreground emphasis) ;; Default Foreground, Caret, Delimiters, Operators
      :base06 (myron-get :faded emphasis)      ;; Light Foreground (Not often used)

      ;; this is only used for company scrollbar background
      :base07 (myron-get :background :strong) ;; Light Background (Not often used)
      ;; :base07 (myron-get :background :weak) ;; Light Background (Not often used)

      ;; org-todo, variables
      ;; :base08 :accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
      :base08 (myron-get :alt emphasis) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

      ;; ivy-current-match foreground
      :base09 (myron-get :foreground emphasis) ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

      ;; types
      ;; :base0A :accent1 ;; Classes, Markup Bold, Search Text Background
      :base0A (myron-get :alt emphasis) ;; Classes, Markup Bold, Search Text Background

      ;; strings
      :base0B (myron-get :strings emphasis) ;; Strings, Inherited Class, Markup Code, Diff Inserted

      ;; :base0C :foreground_  ;; Support, Regular Expressions, Escape Characters, Markup Quotes
      :base0C (myron-get :assumed emphasis) ;; Support, Regular Expressions, Escape Characters, Markup Quotes

      ;; prompt, function-name, search match foreground
      :base0D (myron-get :primary emphasis) ;; Functions, Methods, Attribute IDs, Headings

      ;; keyword-face, org-date
      :base0E (myron-get :assumed emphasis) ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

      :base0F (myron-get :faded emphasis) ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
      )))

(defun myron--face-get-parent (face label theme-faces)
  ;; search up throughout parents of FACE (via :inherit) that firstly has a non-nil LABEL (or return nil)
  ;; theme-faces looks like ((face <plist spec>) (face <plist-spec>)..)
  (let* ((match (-first (lambda (theme-face)
                          (eq (-first-item theme-face) face))
                  theme-faces))
          (face (car match))
          (spec (cdr match)))
    (if (plist-member spec label)
      (plist-get spec label)
      (if (plist-member spec :inherit)
        (myron--face-get-parent (plist-get spec :inherit) label theme-faces)
        nil))))

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

           (window-divider :foreground faded)
           (vertical-border :foreground faded)

           ;; ｉｄｅｎｔｉｔｙ
           (font-lock-function-name-face :foreground primary)
           (font-lock-variable-name-face :foreground primary)

           (org-level-1 :foreground foreground)
           (org-level-2 :foreground foreground)
           (org-level-3 :foreground foreground)
           (org-level-4 :foreground foreground)
           (org-level-5 :foreground foreground)
           (org-level-6 :foreground foreground)

           (whitespace-space :background nil)
           (whitespace-tab :background nil)

           (org-date :underline nil)
           (flycheck-warning :underline nil)
           (flycheck-info :underline nil)

           ;; these are outside the below b/c they are just bg emphasis, keep fg color
           (secondary-selection :background ,(myron-get :background :strong))
           (magit-diff-context-highlight :background ,(myron-get :background :weak))

           (parenthesis :foreground ,(myron-get :assumed))

           ;; todo: revisit this
           ,@(when (ct-light-p (myron-get :background))
               ;; only handling light for now
               ;; (main motivation for adding these here is terminal emacs
               ;; failed to detect the light background)
               ;;
               ;; theory: steal the hue from the green background used in
               ;; vanilla magit-diff-added and then apply it to background
               ;; levels used in theme
               (let*
                 (
                   (og-green "#ddffdd")
                   (og-green-hue (ct-get-hsluv-h og-green))

                   (bg-green-strong (-> (myron-get :background :normal)
                                      (ct-edit-hsluv-h og-green-hue)))

                   (bg-green-weak (ct-edit-hsluv-h (myron-get :background :strong)
                                    og-green-hue))

                   (og-red "#ffdddd")
                   (og-red-hue (ct-get-hsluv-h og-red))

                   (bg-red-strong (ct-edit-hsluv-h (myron-get :background :weak)
                                    og-red-hue))

                   (bg-red-weak (ct-edit-hsluv-h (myron-get :background :strong)
                                  og-red-hue)))
                 `(
                    ;; ignore the above and use the builtin colors for now

                    (magit-diff-added :foreground "#22aa22" )
                    (magit-diff-added-highlight :foreground  "#22aa22")

                    (magit-diff-added :background "#ddffdd")
                    (magit-diff-added-highlight :background  "#cceecc")

                    (magit-diff-removed :foreground "#aa2222")
                    (magit-diff-removed-highlight :foreground "#aa2222")

                    (magit-diff-removed :background "#ffdddd")
                    (magit-diff-removed-highlight :background "#eecccc"))))

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

                  (eros-result-overlay-face :strong :foreground)
                  (cider-result-overlay-face :strong :foreground)

                  (completions-common-part :normal :primary)

                  (comint-highlight-prompt :normal :assumed)

                  (tooltip :weak)

                  (lsp-ui-sideline-global :weak :alt)
                  (lsp-ui-sideline-current-symbol :weak :alt)

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
                  (lazy-highlight :strong)
                  (ivy-match :focused)

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

                    ;; make cider inline test faces similar to magit
                    ;; (abusing for consistency)
                    magit-diff-removed-highlight (cider-test-failure-face cider-test-error-face)
                    magit-diff-added-highlight cider-test-success-face

                    magit-diff-removed smerge-upper
                    magit-diff-added smerge-lower
                    default smerge-markers

                    ;; todo?:
                    ;; diff-refined-removed
                    ;; diff-refined-added
                    )))))

      (new-theme
        ;; apply our individual changes to the original theme
        (-reduce-from
          (-lambda (state (face key value))
            (if (-contains-p (-map #'-first-item state) face)
              (-map
                (lambda (entry)
                  (if (eq (-first-item entry) face)
                    `(,face ,@(plist-put (cdr entry) key value))
                    entry))
                state)
              (cons (list face key value) state)))
          original-theme
          theme-changes))

      (new-theme-experiment
        ;; idea: auto-conform foreground faces based on found background
        ;; this way we don't have to find where to adjust intensity further
        (-map
          (lambda (face-spec)
            (let*
              ((face (car face-spec))
                (spec (cdr face-spec))

                (background (myron--face-get-parent face :background new-theme))
                (foreground (myron--face-get-parent face :foreground new-theme))

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
                            (list (intern (myron-get :background level)) level))
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
                            (list (intern (myron-get kind)) kind))
                          '(:foreground :faded :primary :alt :assumed :strings)))
                      (intern foreground-color))
                    :foreground))

                (new-foreground
                  (when foreground-symbol
                    (myron-get foreground-symbol background-symbol))))

              (if (plist-member spec :background)
                `(,face ,@(plist-put spec :foreground (or new-foreground (plist-get spec :foreground))))
                ;; if no background was ever set, just return the OG:
                face-spec)))
          new-theme)))

    ;; original-theme
    ;; new-theme
    new-theme-experiment
    ))

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
