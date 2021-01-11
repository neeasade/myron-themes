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
         (-map (fn (ct-tint-ratio <> background 4.5)) it)
         )))

    (ht
      (:background background)

      ;; note: original was 10
      (:foreground (ct-tint-ratio background background 8.5))
      (:faded (ct-tint-ratio background background 6))

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
    (faded (ht-get normal-parts :faded))
    (foreground (ht-get normal-parts :foreground))

    ;; XXX: order matters (this happens after accent setting)
    ;; (foreground_ (ct-transform-hsluv-l foreground_ 43.596))
    ;; (foreground_ (ct-tint-ratio foreground_ background 4.5))

    (background+
      (ct-iterate
        (ct-transform-lch-c alt 20)
        'ct-lab-lighten
        (fn (> (ct-contrast-ratio <> faded) 3.5))))

    ;; new idea: these could be contrast based as well in relation to foreground
    (background>
      (-> background
        (ct-transform-lch-h (ct-get-lch-h alt))
        (ct-transform-lch-l (ct-get-lch-l foreground))
        ((lambda (c) (ct-tint-ratio foreground c 9)))))

    (background>>
      (-> background
        (ct-transform-lch-h (ct-get-lch-h alt))
        (ct-transform-lch-l (ct-get-lch-l foreground))
        ((lambda (c) (ct-tint-ratio foreground c 8)))))

    )

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
        (:foreground_ (tarp/get :faded)))))
  )

(deftheme tarp-mcfay)
(tarp/base16-theme-define tarp/theme 'tarp-mcfay)

(provide-theme 'tarp-mcfay)
