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
                (-map (fn (ct-transform-hsluv-l <> 80)) it)
                (-map
                  (fn (ct-tint-ratio <> background 4.3))
                  it)))

      (foreground (ct-tint-ratio background background 7))

      (faded
        (ct-tint-ratio
          (ct-transform-hsl
            (ct-tint-ratio background background 5.5)
            ;; (nth 2 colors)
            (lambda (h s l) (list h 80 70)))
          background 4.3)))

    (ht
      (:background background)
      (:foreground foreground)
      (:faded faded)

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings
        (-> (nth 3 colors)
          (ct-transform-lch-l (-partial '+ 10))
          (ct-transform-lch-c 100)
          (ct-tint-ratio background 4.3))))))

(let*
  (
    (background (ct-make-lab 93 2 4))
    (normal-parts (tarp/struan-get-colors background))

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
        (ct-transform-hsluv-h (ct-get-hsluv-h primary))))

    (background+
      (-> alt
        (ct-transform-lch-c 25)
        (ct-transform-hsluv-l
          (ct-get-hsluv-l background>>)))))

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
