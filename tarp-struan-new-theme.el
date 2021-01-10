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
          background
          4.5
          )
        )

      )

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
    (background-parts (tarp/struan-get-colors background))

    (alt (ht-get background-parts :alt))
    (faded (ht-get background-parts :faded))
    (foreground (ht-get background-parts :foreground))

    ;; active BG (selections)
    ;; take an accent color, fade it until you reach a minimum contrast against foreground_
    (background+
      (ct-iterate
        ;; accent2
        ;; (ct-transform-lch-c accent2 (-partial '* 0.5))
        (ct-transform-lch-c
          alt
          (lambda (_) 33))
        'ct-lab-lighten
        (fn (> (ct-contrast-ratio <>
                 faded
                 ) 4.0))
        ;; (fn (> (ct-contrast-ratio <> foreground_) 3.5))
        ))

    ;; new idea: these could be contrast based as well in relation to foreground
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
      (:normal background-parts)
      (:focused
        (ht-merge
          background-parts
          (ht (:background background+))))

      (:medium (tarp/struan-get-colors background>))
      (:heavy (tarp/struan-get-colors background>>))))

  ;; shim:
  (setq tarp/theme
    (ht-merge
      (ht-get tarp/theme* :normal)
      (ht
        (:foreground+ (ht-get* tarp/theme* :focused :foreground))
        (:background+ (ht-get* tarp/theme* :focused :background))
        (:background_ (ht-get* tarp/theme* :medium :background))
        (:background__ (ht-get* tarp/theme* :heavy :background))
        (:accent1  (ht-get* tarp/theme* :normal :primary))
        (:accent1_ (ht-get* tarp/theme* :normal :assumed))
        (:accent2  (ht-get* tarp/theme* :normal :alt))
        (:accent2_ (ht-get* tarp/theme* :normal :strings))
        (:accent2_ (ht-get* tarp/theme* :normal :strings))

        (:foreground_ (ht-get* tarp/theme* :normal :faded)))))
  )

(ht-get* tarp/theme* :medium :background)

;; "#dcd3cd"

(deftheme tarp-struan-new)
(tarp/base16-theme-define tarp/theme 'tarp-struan-new)

(provide-theme 'tarp-struan-new)
