
;; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/marlowe-get-accents (background foreground foreground_)
  ;; return a list accent1, accent1_, accent2, accent2_

  (->>
    (ct/rotation-hsluv
      (ct/make-hsluv 346 80 60)
      180)

    (reverse)
    (-map
      (fn
        (list <>
          (ct/transform-hsv-v <> (lambda (s) (* .7 s)))
          )
        )
      )
    (-flatten)
    (reverse)
    ))

(let*
  (
    (background
      ;; (ct/make-lab 93 2 4)
      (ct/make-lab 90 10 -10)
      )

    (foreground (ct/tint-ratio background background 5.5))
    (foreground_ (ct/tint-ratio background background 3.5))

    (accents (tarp/marlowe-get-accents background foreground foreground_))

    (accent1  (nth 0 accents))
    (accent1_ (nth 1 accents))
    (accent2  (nth 2 accents))
    (accent2_ (nth 3 accents))

    ;; active BG (selections)
    ;; take an accent color, fade it until you reach a minimum contrast against foreground_
    (background+

      (ct/iterate
        ;; flip the bg AB values
        (ct/make-lab 90 -10 10)

        'ct/lab-darken
        (fn (> (ct/contrast-ratio <> foreground_) 4.0))
        ;; (fn (> (ct/contrast-ratio <> foreground_) 3.5))
        )

      )

    ;; new idea: these could be contrast based as well in relation to foreground
    (background_
      (ct/lab-darken 5)
      )

    (background__
      (ct/lab-darken 10)
      ))

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

(deftheme tarp-marlowe)
(tarp/base16-theme-define tarp/theme 'tarp-marlowe)

(provide-theme 'tarp-marlowe)
