;; ; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/sky-get-colors (bg)
  (let ((fg-ratio 2.5)
         (return (ht)))

    (ht-set return :background bg)
    (ht-set return :foreground (ct-tint-ratio bg bg fg-ratio))
    (ht-set return :faded (ct-tint-ratio bg bg (- fg-ratio 1)))

    (->> (ct-rotation-hsluv bg 60)
      (-map (lambda (c)
              (ct-iterate c
                (-compose 'ct-edit-hsluv-l-dec 'ct-edit-hsluv-s-inc)
                (lambda (step) (> (ct-contrast-ratio step bg) fg-ratio)))))
      ;; todo: revisit assumed color
      (tarp/take '(2 1 0 4))
      ;; (tarp/take '(3 2 1 0))
      (-interleave '(:primary :assumed :strings :alt))
      (-partition 2)
      (-map (-lambda ((k v)) (ht-set return k v))))

    return))

(let ((theme-colors (append (tarp/map-to-base16) (ht-to-plist tarp/theme))))
  (tarp/theme-make-faces theme-colors))


(tarp/theme-b)
(-let*
  (
    ;; (background (ct-make-lab 94 -5 0))
    (background "#a6d0ed")

    (background> (ct-iterate background 'ct-edit-hsluv-l-dec
                   (fn (> (ct-name-distance <> background) 4))))

    (background>> (ct-iterate background 'ct-edit-hsluv-l-dec
                    (fn (> (ct-name-distance <> background) 7))))

    (normal-parts (tarp/sky-get-colors background))
    ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

    (background+ (ct-edit-hsluv-l primary (ct-get-hsluv-l background>))))

  (setq tarp/theme*
    (ht
      (:normal normal-parts)
      (:weak (tarp/sky-get-colors background>))
      (:strong (tarp/sky-get-colors background>>))
      (:focused (tarp/sky-get-colors background+)))))

(deftheme tarp-sky)
(tarp/base16-theme-define 'tarp-sky)

(provide-theme 'tarp-sky)
