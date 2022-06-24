;; -*- lexical-binding: t; -*-
;; namesake: the Struan family, from James Clavell's Asian Saga series

(require 'tarps)

(defun tarp/struan-get-colors (background)
  "get the foreground colors against a specific background"
  (let*
    ((colors (--> (ct-make-hsluv 265 60 40)
               (ct-rotation-hsluv it 60)
               (-map (fn (tarp/nth <> it))
                 '(1 -1 2 3))
               (-map (fn (ct-edit-hsluv-l <> 80)) it)
               (-map (fn (ct-tint-ratio <> background 4.3)) it)))

      (foreground (ct-tint-ratio background background 7))

      (faded
        (ct-tint-ratio
          (ct-edit-hsl
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
          (ct-edit-lch-l (-partial '+ 10))
          (ct-edit-lch-c 100)
          (ct-tint-ratio background 4.3))))))

(-let*
  ((background (ct-make-lab 93 2 4))
    (normal-parts (tarp/struan-get-colors background))
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
        (ct-edit-lch-c 25)
        (ct-edit-hsluv-l
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
      (:strong (tarp/struan-get-colors background>>)))))

(deftheme tarp-struan)
(tarp/base16-theme-define 'tarp-struan)

(provide-theme 'tarp-struan)
