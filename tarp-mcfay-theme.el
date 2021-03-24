;; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/mcfay-get-colors (background)
  "get the foreground colors against a specific background"
  (let*
    ((colors
       (-->
         (ct-make-hsluv 270 75 43.596)
         (ct-rotation-lch it -45)
         (-map (fn (tarp/nth <> it)) '(-1 1 2 4))
         (-map (fn (ct-transform-hsluv-l <> 43.596)) it)
         (-map (fn (ct-tint-ratio <> background 4.3)) it))))

    (ht
      (:background background)

      (:foreground (ct-tint-ratio background background 7.7))
      (:faded (ct-tint-ratio background background 4.3))

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings (ct-transform-lch-c (nth 3 colors) 100)))))

(let*
  (
    ;; /slightly/ cool
    (background (ct-make-lab 93 -0.5 -1))

    (normal-parts (tarp/mcfay-get-colors background))

    (alt (ht-get normal-parts :alt))
    (assumed (ht-get normal-parts :assumed))
    (primary (ht-get normal-parts :primary))
    (faded (ht-get normal-parts :faded))
    (foreground (ht-get normal-parts :foreground))

    (background>
      (-> background
        (ct-iterate
          'ct-lab-darken
          (fn (> (ct-name-distance <> background) 4)))
        (ct-transform-hsluv-h (ct-get-hsluv-h alt))))

    (background>>
      (-> background
        (ct-iterate
          'ct-lab-darken
          (fn (> (ct-name-distance <> background) 7)))
        (ct-transform-hsluv-h (ct-get-hsluv-h primary))
        ))

    (background+
      (-> alt
        (ct-transform-lch-c 20)
        (ct-transform-hsluv-l
          (ct-get-hsluv-l background>>))))
    )

  (setq tarp/theme*
    (ht
      ;; focused/selected emphasis
      (:focused
        (tarp/mcfay-get-colors background+)
        ;; (ht-merge
        ;;   normal-parts
        ;;   (ht (:background background+)))
        )

      ;; normal emphasis
      (:normal normal-parts)

      ;; weak emphasis
      (:weak (tarp/mcfay-get-colors background>))

      ;; strong emphasis
      (:strong (tarp/mcfay-get-colors background>>))))

  ;; todo: conform hue of all foregrounds:
  ;; (let ((h (ct-get-hsl-l (tarp/get :foreground))))
  ;;   (ht-set tarp/theme* :weak))

  ;; shim:
  (setq tarp/theme
    (ht-merge
      (ht-get tarp/theme* :normal)
      (ht
        ;; compat
        (:foreground+ (tarp/get :foreground :focused))
        (:background+ (tarp/get :background :focused))
        (:background_ (tarp/get :background :weak))
        (:background__ (tarp/get :background :strong))
        (:accent1  (tarp/get :primary))
        (:accent1_ (tarp/get :assumed))
        (:accent2  (tarp/get :alt))
        (:accent2_ (tarp/get :strings))
        (:foreground_ (tarp/get :faded)))))
  )

(deftheme tarp-mcfay)
(tarp/base16-theme-define 'tarp-mcfay)

(provide-theme 'tarp-mcfay)
