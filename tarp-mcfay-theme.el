;; -*- lexical-binding: t; -*-
;; Namesake: Jamie McFay, from the James Clavell novel Gai-Jin

(require 'tarps)

(defun tarp/mcfay-get-colors (background)
  "get the foreground colors against a specific background"
  (let*
    ((colors (-->
               (ct-make-hsluv 270 75 43.596)
               (ct-rotation-lch it -45)
               (-map (fn (tarp/nth <> it)) '(-1 1 2 4))
               (-map (fn (ct-edit-hsluv-l <> 43.596)) it)
               (-map (fn (ct-tint-ratio <> background 4.3)) it))))

    (ht
      (:background background)

      (:foreground (ct-tint-ratio background background 7.7))
      (:faded (ct-tint-ratio background background 4.3))

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings (ct-edit-lch-c (nth 3 colors) 100)))))

(-let*
  (
    ;; /slightly/ cool
    (background (ct-make-lab 93 -0.5 -1))
    (normal-parts (tarp/mcfay-get-colors background))
    ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

    (background>
      (-> background
        (ct-iterate 'ct-edit-lab-l-dec
          (fn (> (ct-name-distance <> background) 4)))
        (ct-edit-hsluv-h (ct-get-hsluv-h alt))))

    (background>>
      (-> background
        (ct-iterate 'ct-edit-lab-l-dec
          (fn (> (ct-name-distance <> background) 7)))
        (ct-edit-hsluv-h (ct-get-hsluv-h primary))))

    (background+
      (-> alt
        (ct-edit-lch-c 20)
        (ct-edit-hsluv-l
          (ct-get-hsluv-l background>>)))))

  (setq tarp/theme*
    (ht
      ;; focused/selected emphasis
      (:focused (tarp/mcfay-get-colors background+))

      ;; normal emphasis
      (:normal normal-parts)

      ;; weak emphasis
      (:weak (tarp/mcfay-get-colors background>))

      ;; strong emphasis
      (:strong (tarp/mcfay-get-colors background>>)))))

(deftheme tarp-mcfay)
(tarp/base16-theme-define 'tarp-mcfay)

(provide-theme 'tarp-mcfay)
