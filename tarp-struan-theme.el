;; -*- lexical-binding: t; -*-
;; temporary experimental version of struan, while approaching tarp 2.0

(require 'tarps)

(defun tarp/struan-get-colors (background)
  "get the foreground colors against a specific background"
  (let*
    (
      (colors (-->
                (ct-rotation-hsluv
                  (ct-make-hsluv 265 60 40) 60)
                (-map (fn (tarp/nth <> it))
                  '(1 -1 2 3))
                (-map
                  (fn (ct-tint-ratio <> background 4.5 ))
                  it)))

      (foreground (ct-tint-ratio background background 8.5))

      (faded
        (ct-tint-ratio
          (ct-transform-hsl
            (ct-tint-ratio background background 5.5)
            ;; (nth 2 colors)
            (lambda (h s l) (list h 80 70)))
          background 4.5)
        ))

    (ht
      (:background background)
      (:foreground foreground)
      (:faded faded)

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings (ct-transform-lch-c (nth 3 colors) 100)))))

(let*
  (
    (background (ct-make-lab 93 2 4))
    (normal-parts (tarp/struan-get-colors background))

    (alt (ht-get normal-parts :alt))
    (assumed (ht-get normal-parts :assumed))
    (faded (ht-get normal-parts :faded))
    (foreground (ht-get normal-parts :foreground))

    (background+
      (ct-iterate
        (ct-transform-lch-c alt 33)
        'ct-lab-lighten
        (fn (> (ct-contrast-ratio <> faded) 3.5))))

    (background>
      (-> background
        (ct-transform-lch-h (ct-get-lch-h alt))
        (ct-transform-lch-l (ct-get-lch-l foreground))
        ((lambda (c) (ct-tint-ratio foreground c 7)))))

    (background>>
      (-> background
        (ct-transform-lch-h (ct-get-lch-h alt))
        (ct-transform-lch-l (ct-get-lch-l foreground))
        ((lambda (c) (ct-tint-ratio foreground c 6))))))

  (setq tarp/theme*
    (ht
      ;; focused/selected emphasis
      (:focused
        (ht-merge
          normal-parts
          (ht (:background background+))))

      ;; normal emphasis
      (:normal normal-parts)

      ;; weak emphasis
      (:weak (tarp/struan-get-colors background>))

      ;; strong emphasis
      (:strong (tarp/struan-get-colors background>>))))

  ;; convienence
  (defun tarp/get (label &optional emphasis )
    (ht-get* tarp/theme* (or emphasis :normal) label))

  ;; shim:
  (setq tarp/theme
    (ht-merge
      (ht-get tarp/theme* :normal)
      (ht
        (:foreground+ (tarp/get :foreground :focused))
        (:background+ (tarp/get :background :focused))
        (:background_ (tarp/get :background :weak))
        (:background__ (tarp/get :background :strong))
        (:accent1  (tarp/get :primary))
        (:accent1_ (tarp/get :assumed))
        (:accent2  (tarp/get :alt))
        (:accent2_ (tarp/get :strings))
        (:foreground_ (tarp/get :faded))))))

(deftheme tarp-struan)
(tarp/base16-theme-define 'tarp-struan)

(provide-theme 'tarp-struan)
