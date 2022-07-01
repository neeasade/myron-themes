;; ; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/take (indices colors)
  (-map (lambda (i) (nth i colors)) indices))

(defun tarp/storm-get-colors (bg)
  (let ((fg-ratio 5.0)
         (return (ht)))
    (->> (ct-rotation-hsluv bg 60)
      (-map (lambda (c)
              (ct-iterate c
                (-compose 'ct-edit-hsluv-l-dec 'ct-edit-hsluv-s-inc)
                (lambda (step) (> (ct-contrast-ratio step bg) fg-ratio)))))
      ;; todo: revisit builtin color
      (tarp/take '(2 1 0 4))
      ;; (tarp/take '(3 2 1 0))
      (-interleave '(:primary :assumed :strings :alt))
      (-partition 2)
      (-map (-lambda ((k v)) (ht-set return k v))))

    (ht-set return :background bg)
    (ht-set return :foreground (ct-tint-ratio bg bg fg-ratio))
    (ht-set return :faded (ct-tint-ratio bg bg (- fg-ratio 1)))
    return
    ))

(-let*
  ((background (ct-make-lab 94 -5 0))

    (background> (ct-iterate background 'ct-edit-hsluv-l-dec
                   (fn (> (ct-name-distance <> background) 4))))

    (background>> (ct-iterate background 'ct-edit-hsluv-l-dec
                    (fn (> (ct-name-distance <> background) 7))))

    (normal-parts (tarp/storm-get-colors background))
    ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

    (background+ (ct-edit-hsluv-l primary (ct-get-hsluv-l background>>))))

  (setq tarp/theme* (ht))

  (->> (list background background> background>> background+)
    (-interleave '(:normal :weak :strong :focused))
    (-partition 2)
    (-map
      (-lambda ((label bg))
        (ht-set tarp/theme* label (tarp/storm-get-colors bg))))))

(deftheme tarp-storm)
(tarp/base16-theme-define 'tarp-storm)

(provide-theme 'tarp-storm)
