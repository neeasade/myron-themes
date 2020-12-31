;; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/mcfay-get-accents (background foreground foreground_)
  ;; return a list accent1, accent1_, accent2, accent2_
  (-->
    (ct/make-hsluv 270 75 (ct/get-hsluv-l foreground_))
    (ct/rotation-lch it -45)

    (-map (fn (tarp/nth <> it))
      '(-1 1 2 4))

    (-map (fn (ct/transform-hsluv-l <> 43.596)) it)
    (-map (fn (ct/tint-ratio <> background 4.5)) it)))

(let*
  (
    ;; /slightly/ cool
    (background (ct/make-lab 93 -0.5 -1))

    (foreground (ct/tint-ratio background background 10))
    (foreground_ (ct/tint-ratio background background 6))

    (accents (tarp/mcfay-get-accents background foreground foreground_))

    ;; XXX: order matters (this happens after accent setting)
    (foreground_ (ct/transform-hsluv-l foreground_ 43.596))
    (foreground_ (ct/tint-ratio foreground_ background 4.5))

    (accent1  (nth 0 accents))
    (accent1_ (nth 1 accents))
    (accent2  (nth 2 accents))
    (accent2_ (nth 3 accents))

    (background+
      (ct/iterate
        (ct/transform-lch-c accent2 (-partial '* 0.5))
        'ct/lab-lighten
        (fn (> (ct/contrast-ratio <> foreground_)
              4.0
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
  )

(deftheme tarp-mcfay)
(tarp/base16-theme-define tarp/theme 'tarp-mcfay)

(provide-theme 'tarp-mcfay)
