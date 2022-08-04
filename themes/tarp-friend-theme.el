;; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/friend-get-colors (background)
  "get the foreground colors against a specific background"
  (let*
    ((colors
       (->>
         (ct-rotation-hsv background 3)
         )))

    (ht
      (:background background)

      (:foreground (ct-tint-ratio background background 7.7))
      (:faded (ct-tint-ratio background background 4.3))

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings (ct-edit-lch-c (nth 3 colors) 100)))))

(let*
  (
    (background
      (ct-make-lab 93 -0.5 -1)
      )

    (normal-parts (tarp/friend-get-colors background))

    (alt (ht-get normal-parts :alt))
    (assumed (ht-get normal-parts :assumed))
    (primary (ht-get normal-parts :primary))
    (faded (ht-get normal-parts :faded))
    (foreground (ht-get normal-parts :foreground))

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
          (ct-get-hsluv-l background>>))))
    )

  (setq tarp/theme*
    (ht
      ;; focused/selected emphasis
      (:focused
        (tarp/friend-get-colors background+)
        ;; (ht-merge
        ;;   normal-parts
        ;;   (ht (:background background+)))
        )

      ;; normal emphasis
      (:normal normal-parts)

      ;; weak emphasis
      (:weak (tarp/friend-get-colors background>))

      ;; strong emphasis
      (:strong (tarp/friend-get-colors background>>))))

  ;; todo?: conform hue of all foregrounds:
  ;; (let ((h (ct-get-hsl-l (tarp/get :foreground))))
  ;;   (ht-set tarp/theme* :weak))

  (setq tarp/theme (ht-get tarp/theme* :normal)))

(deftheme tarp-friend)
(tarp/base16-theme-define 'tarp-friend)

(provide-theme 'tarp-friend)
