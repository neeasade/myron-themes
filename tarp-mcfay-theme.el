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
  )

(deftheme tarp-mcfay)
(tarp/base16-theme-define tarp/theme 'tarp-mcfay)

(provide-theme 'tarp-mcfay)
