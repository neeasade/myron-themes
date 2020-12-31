;; -*- lexical-binding: t; -*-

(require 'tarps)
(require 'base16-theme)
(require 'color-tools)

(defun tarp/struan-get-accents (background foreground foreground_)
  ;; return a list accent1, accent1_, accent2, accent2_
  (-->
    (ct/rotation-hsluv
      (ct/make-hsluv 265 60 40)
      60)
    (-map (fn (tarp/nth <> it))
      '(1 -1 2 3))
    (-map
      (fn (ct/tint-ratio <> background 4.5 ))
      it)))

(let*
  (
    (background (ct/make-lab 93 2 4))

    (foreground (ct/tint-ratio background background 8.5))
    (foreground_ (ct/tint-ratio background background 5.5))

    (accents (tarp/struan-get-accents background foreground foreground_))

    (accent1  (nth 0 accents))
    (accent1_ (nth 1 accents))
    (accent2  (nth 2 accents))
    (accent2_ (nth 3 accents))

    ;; active BG (selections)
    ;; take an accent color, fade it until you reach a minimum contrast against foreground_
    (background+
      (ct/iterate
        ;; accent2
        ;; (ct/transform-lch-c accent2 (-partial '* 0.5))
        (ct/transform-lch-c accent2 (lambda (_) 33))
        'ct/lab-lighten
        (fn (> (ct/contrast-ratio <> foreground_) 4.0))
        ;; (fn (> (ct/contrast-ratio <> foreground_) 3.5))
        )
      )

    ;; new idea: these could be contrast based as well in relation to foreground
    (background_
      (-> background
        (ct/transform-lch-h (ct/get-lch-h accent2))
        (ct/transform-lch-l (ct/get-lch-l foreground))
        ((lambda (c) (ct/tint-ratio foreground c 7)))))

    (background__
      (-> background
        (ct/transform-lch-h (ct/get-lch-h accent2))
        (ct/transform-lch-l (ct/get-lch-l foreground))
        ((lambda (c) (ct/tint-ratio foreground c 6))))))

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

  (ht-set tarp/theme :foreground_
    (ct/tint-ratio
      (ct/transform-hsl accent2 (lambda (h s l) (list h 80 70)))
      background
      4.5
      ))

  ;; let's play MAX, THAT, CHROMA!
  (ht-set tarp/theme :accent2_ (ct/transform-lch-c accent2_ 100))
  )

(deftheme tarp-struan)
(tarp/base16-theme-define tarp/theme 'tarp-struan)

(provide-theme 'tarp-struan)
