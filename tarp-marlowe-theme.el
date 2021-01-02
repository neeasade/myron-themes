;; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/marlowe-get-accents (background foreground foreground_)
  ;; return a list accent1, accent1_, accent2, accent2_

  (->>
    (list
      "#f1a0c0"
      "#5dc4ea"
      "#bf843e"
      "#9ec453"
      )
    (-map (fn (ct/tint-ratio <> background 3.0)))
    )

  ;; remnants:
  ;; (->>
  ;;   (ct/rotation-hsluv
  ;;     (ct/make-hsluv 346 80 75)
  ;;     ;; (ct/make-hsluv 306 80 60)
  ;;     ;; (ct/make-hsluv 240 80 60)
  ;;     ;; (ct/make-hsluv 180 80 60)
  ;;     ;; (ct/make-hsluv 270 80 60)
  ;;     ;; (ct/make-hsluv 45 80 75)
  ;;     ;; 180
  ;;     120
  ;;     )
  ;;   (reverse)
  ;;   ;; (-map
  ;;   ;;   (fn
  ;;   ;;     (list <>
  ;;   ;;       (ct/transform-hsv-v <> (lambda (s) (* .8 s)))
  ;;   ;;       )
  ;;   ;;     )
  ;;   ;;   )
  ;;   ;; (-flatten)
  ;;   ;; (reverse)
  ;;   )

  )

(let*
  (
    (background
      ;; (ct/make-lab 95 10 -10)
      (ct/make-lab 95 -10 10)
      )

    (foreground (ct/tint-ratio background background 5.5))
    (foreground_ (ct/tint-ratio background background 3.5))

    (accents (tarp/marlowe-get-accents background foreground foreground_))

    (accent1  (nth 0 accents))
    (accent1_ (nth 1 accents))
    (accent2  (nth 2 accents))
    (accent2_ (nth 3 accents))

    ;; active BG (selections)
    (background+
      (ct/transform-hsv-s background 30)
      )

    ;; new idea: these could be contrast based as well in relation to foreground
    (background_
      (ct/lab-darken background 5)
      )

    (background__
      (ct/lab-darken background 10)
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
