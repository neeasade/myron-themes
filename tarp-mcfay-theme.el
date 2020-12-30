;; -*- lexical-binding: t; -*-

(require 'tarps)
(require 'base16-theme)
(require 'color-tools)

(let*
  (
    ;; /slightly/ cool
    (background (ct/make-lab 93 -0.5 -1))


    (foreground (ct/tint-ratio background background 10))
    (foreground_ (ct/tint-ratio background background 6))

    ;; LCH rotate -45 from blue (hue 270)
    (accent-rotations
      (let ((color-start
              (ct/transform-hsluv
                foreground_
                (lambda (H S L)
                  (list 270 75 L))))
             (interval -45))
        (-map
          (lambda (step)
            (ct/transform-lch-h color-start
              (fn (+ <> (* step interval)))))
          (range (/ 360 (abs interval))))))

    (accent1  (tarp/nth -1 accent-rotations))
    (accent1_ (tarp/nth 1 accent-rotations))
    (accent2  (tarp/nth 2 accent-rotations))
    (accent2_ (tarp/nth 4 accent-rotations))

    ;; active BG (selections)
    ;; take an accent color, fade it until you reach a minimum contrast against foreground_
    (background+
      (ct/iterate
        (ct/transform-lch-c accent2 (-partial '* 0.5))
        ;; (ct/transform-lch-c accent2 (lambda (_) 33))
        ;; (fn (ct/lab-lighten <> 0.5))
        'ct/lab-lighten
        (fn (> (ct/contrast-ratio <> foreground_)
              5
              ))))

    ;; new idea: these could be contrast based as well in relation to foreground
    (background_
      (-> background
        (ct/transform-lch-h (ct/get-lch-h accent2))
        (ct/transform-lch-l (ct/get-lch-l foreground))
        ((lambda (c) (ct/tint-ratio foreground c 9)))))

    (background__
      (-> background
        (ct/transform-lch-h (ct/get-lch-h accent2))
        (ct/transform-lch-l (ct/get-lch-l foreground))
        ((lambda (c) (ct/tint-ratio foreground c 8)))))

    ;; (background__ (-> background_ (ct/transform-hsluv-l (-rpartial '- 6))))
    )

  (setq tarp/theme
    (ht
      (:foreground foreground)          ; regular text
      (:foreground_ foreground_)        ; comments
      (:foreground+ foreground)         ; foreground of a focused/highlighted thing

      (:background background)          ; regular canvas
      (:background_ background_)        ; emphasis?
      (:background__ background__)      ; inactive modeline
      (:background+ background+)  ; background of a focused/highlighted thing (also active modeline)

      (:accent1 accent1)                ; identifiers
      (:accent1_ accent1_)              ; builtins
      (:accent2 accent2)                ; types
      (:accent2_ accent2_)              ; strings
      ))

  ;; perform transforms to accent colors:

  (tarp/edit
    (lambda (k v)
      (cond
        ((or
           (s-starts-with-p ":accent" (prin1-to-string k))
           (s-starts-with-p ":foreground_" (prin1-to-string k)))
          ;; messing around:
          ;; don't tweak anything
          ;; v

          ;; ensure all colors have some minimum contrast ratio
          (ct/tint-ratio
            ;; conform lightness -- this lightness was just from a green for strings I liked
            (ct/transform-hsluv-l v 43.596)
            ;; (ct/transform-hsluv-l v 50)
            ;; v

            ;; against, ratio
            background 4.5))
        (t v))))

  ;; do this transform after messing with accent colors above.
  (ht-set tarp/theme :background+
    (ct/iterate
      (ct/transform-lch-c
        (ht-get tarp/theme :accent2)
        ;; (ht-get tarp/theme :foreground_)
        (-partial '* 0.5))
      ;; (ct/transform-lch-c accent2 (lambda (_) 33))
      'ct/lab-lighten
      (fn (> (ct/contrast-ratio <> foreground_) 5))))

  ;; white point const meanings
  ;; color-d65-xyz ;; | Noon Daylight: Television, sRGB color space (standard assumption)
  ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
  ;; color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
  ;; color-d75-xyz ;; | North sky Daylight
  ;; -------------------------------------
  ;; the definition of 'white' as displayed on the screen your viewing it on
  ;; picture you are a photographer, taking pictured in different lighting conditions -- sunlight and
  ;; incandescent lighting conditions are very different, for example.
  ;; well here, your monitor is like the photograph, and this point represents "white" on your screen.

  ;; some lighting ideas to try these out in:
  ;; - outside
  ;; - a dark room
  ;; - low screen brightness
  ;; - high screen brightness
  ;; (->>
  ;;   (fn (ct/tint-with-light <> ns/theme-white-point color-d55-xyz))
  ;;   ;; (fn (ct/tint-with-light <> ns/theme-white-point color-d50-xyz))
  ;;   (ht-transform-v ns/theme)
  ;;   (setq ns/theme))
  )

(deftheme tarp-mcfay)
(base16-theme-define 'tarp-mcfay
  (ht-with-context tarp/theme
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
      )))

(provide-theme 'tarp-mcfay)
