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

;; to generate:
;; (--mapcat (list (intern (format "myron-%s" it))
;;             (myron--create-meta-colors (funcall (intern (format "myron-%s-create" it)))))
;;   '(struan mcfay storm dogman grayscale))

(defcustom myron--cache
  '(myron-struan #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#667400" :alt "#916156" :assumed "#2a7783" :primary "#8f5d7f" :faded "#a35a29" :foreground "#544b45" :background "#e1c5c0")) :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#667400" :alt "#916156" :assumed "#2a7783" :primary "#8f5d7f" :faded "#a35a29" :foreground "#544b45" :background "#f2e9e3")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#5b6600" :alt "#845449" :assumed "#1d6a76" :primary "#825072" :faded "#a93b2f" :foreground "#483f3e" :background "#ded5d4")) :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#545f00" :alt "#7d4d42" :assumed "#15626e" :primary "#7b496b" :faded "#a1276b" :foreground "#40373c" :background "#d2c9ce")) :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:interactive-background-highlight "#cacbc5" :interactive-background "#d5d7d0" :diff-remove-highlight "#f0c0c8" :diff-add-highlight "#9aef7b" :diff-remove "#f4d2d7" :diff-add "#bbf8a9")))) myron-mcfay #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#a6d0ed" :foreground "#0c3653" :faded "#335d7a" :primary "#ad0066" :assumed "#0055b8" :alt "#005f87" :strings "#006c00")) :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#e8ebec" :foreground "#444748" :faded "#6a6d6e" :primary "#c6007f" :assumed "#0065c8" :alt "#006e96" :strings "#007c00")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#d6d7d8" :foreground "#3a3b3c" :faded "#606162" :primary "#bb0074" :assumed "#005ec1" :alt "#006890" :strings "#007500")) :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#cdcacb" :foreground "#353233" :faded "#5b5859" :primary "#ae0067" :assumed "#0055b8" :alt "#005f87" :strings "#006d00")) :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:interactive-background-highlight "#c4ccc4" :interactive-background "#d0d8d0" :diff-remove-highlight "#f0c0c8" :diff-add-highlight "#9bef7b" :diff-remove "#f4d2d7" :diff-add "#bbf8a7")))) myron-storm #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#e3f0ed" :foreground "#697673" :faded "#7e8b88" :primary "#a356a4" :assumed "#108082" :strings "#0e8618" :alt "#a95e5b")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#d0ddda" :foreground "#5d6a67" :faded "#717e7b" :primary "#954b96" :assumed "#087376" :strings "#077911" :alt "#9b534e")) :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#c3d0cd" :foreground "#55625f" :faded "#687572" :primary "#8b418d" :assumed "#026a6f" :strings "#006f0a" :alt "#924942")) :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#aee7af" :foreground "#3a733b" :faded "#4e874f" :primary "#02717e" :assumed "#007371" :strings "#007800" :alt "#a34c4d")) :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:interactive-background-highlight "#c7cec7" :interactive-background "#d3dbd3" :diff-remove-highlight "#f0c4cb" :diff-add-highlight "#9bf37a" :diff-remove "#f4d5da" :diff-add "#c5f9b5")))) myron-dogman #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#026d00" :alt "#026d00" :faded "#006b47" :primary "#6d34e3" :assumed "#d144a7" :foreground "#064d51" :background "#92d9dd")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef" :assumed "#e14db9" :foreground "#005953" :background "#92ede7")) :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#1a8500" :alt "#1a8500" :faded "#00825e" :primary "#854cfb" :assumed "#f256c9" :foreground "#036458" :background "#9efff3")) :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#0e7900" :alt "#0e7900" :faded "#007753" :primary "#7940ef" :assumed "#e14db9" :foreground "#524b6a" :background "#e0d9f8")) :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:interactive-background-highlight "#c6cec6" :interactive-background "#d6ded6" :diff-remove-highlight "#f2c7ce" :diff-add-highlight "#9ef57d" :diff-remove "#f5d6db" :diff-add "#c7f9b7")))) myron-grayscale #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#5d5d5d" :alt "#707070" :assumed "#4e4e4e" :primary "#5d5d5d" :faded "#707070" :foreground "#434343" :background "#cacaca")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#656565" :alt "#797979" :assumed "#575757" :primary "#656565" :faded "#797979" :foreground "#4b4b4b" :background "#d7d7d7")) :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#686868" :alt "#7c7c7c" :assumed "#595959" :primary "#686868" :faded "#7c7c7c" :foreground "#4e4e4e" :background "#dbdbdb")) :normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:strings "#717171" :alt "#858585" :assumed "#626262" :primary "#717171" :faded "#858585" :foreground "#565656" :background "#e9e9e9")) :meta #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:interactive-background-highlight "#c5caca" :interactive-background "#d2d8d8" :diff-remove-highlight "#efc0c8" :diff-add-highlight "#99ef78" :diff-remove "#f3d0d6" :diff-add "#b6f8a0")))))
  "Cache value for the themes. Internal use only."
  :type 'sexp
  :group 'myron)

(defun myron-get (label &optional emphasis)
  (or (ht-get* myron-theme* (or emphasis :normal) label)
    (when (not emphasis) (ht-get* myron-theme* :meta label))))

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

(defun myron-theme-make-faces ()
  "Make the myron-theme-faces from the current `myron-theme*' table "
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

           (parenthesis :foreground faded)

           (prescient-primary-highlight :foreground alt)

           ;; maybe this should be assumed or primary
           (prescient-secondary-highlight :foreground strings)

           ((orderless-match-face-0 orderless-match-face-1 orderless-match-face-2 orderless-match-face-3)
             :foreground alt)

           (magit-diff-context-highlight :background ,(myron-get :background :weak))


           ;; todo: this appears to not be doing anything
           ;; (magit-diff-file-heading :extend t)

           ((magit-diff-hunk-heading magit-diff-hunk-heading-highlight) :extend nil)

           (magit-diff-hunk-heading :background ,(myron-get :background :strong))
           (magit-diff-hunk-heading-highlight :background ,(myron-get :background :focused))
           (magit-diff-added :background ,(myron-get :diff-add))
           (magit-diff-removed :background ,(myron-get :diff-remove))
           (magit-diff-added-highlight :background ,(myron-get :diff-add-highlight))
           (magit-diff-removed-highlight :background ,(myron-get :diff-remove-highlight))

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

                  (corfu-default :weak)
                  (corfu-current :focused)

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

                  ;; todo: only want this if we can get (:extend t) to work above
                  ;; (magit-diff-file-heading :strong)

                  (diff-header :strong)
                  (diff-hunk-header :strong)
                  (diff-function :strong)
                  (diff-file-header :weak)

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
                    magit-diff-removed-highlight (diff-refine-removed smerge-refined-removed))))))

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

(defun myron-termcolors ()
  "Export current myron theme to 16 colors"
  ;; stealing default base16 export order for now
  (--map (plist-get (myron-theme-to-base16) it)
    '(:base00 :base08 :base0B :base0A :base0D :base0E :base0C :base05
       :base03 :base09 :base01 :base02 :base04 :base06 :base0F :base07)))

(defun myron--create-meta-colors (colors)
  (ht-set! colors
    :meta
    (-let* (((bg bg-weak bg-strong) (--map (ht-get* colors it :background) '(:normal :weak :strong)))
             (color-strings (ht-get* colors :normal :strings))
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
           :interactive-background ,(-> bg-weak
                                      (ct-edit-hsluv-h (ct-get-hsluv-h color-strings))
                                      (ct-edit-hsluv-s 5))
           :interactive-background-highlight ,(-> bg-strong
                                                (ct-edit-hsluv-h (ct-get-hsluv-h color-strings))
                                                (ct-edit-hsluv-s 5))))))
  colors)

(defun myron-theme-define (theme-name)
  "Implementation of `base16-theme-define` with myron face list"
  (setq myron-theme*
    (if myron-use-cache
      (plist-get myron--cache theme-name)
      (myron--create-meta-colors
        (funcall (intern (format "%s-create" theme-name))))))

  (base16-theme-set-faces theme-name
    (append (myron-theme-to-base16) (ht-to-plist (ht-get myron-theme* :normal)))
    (myron-theme-make-faces))
  (myron-evil-cursor-color (myron-get :primary))
  (custom-theme-set-variables theme-name
    `(ansi-color-names-vector ,(apply 'vector (-take 8 (myron-termcolors))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
          (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
      (or (and (file-directory-p dir) dir)
        base))))

(provide 'myron-themes)
;;; myron-themes.el ends here
