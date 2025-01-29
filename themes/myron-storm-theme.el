;; -*- lexical-binding: t; -*-
;; authored during a storm
;; lower contrast

(require 'myron-themes)

(defun myron-storm-colors (bg)
  "Get the storm foreground colors against a specific BG."
  (let ((fg-ratio 4)
         (return (ht)))

    (ht-set return :background bg)
    (ht-set return :foreground (ct-contrast-min bg bg fg-ratio))
    (ht-set return :faded (ct-contrast-min bg bg (- fg-ratio 1)))

    (->> (ct-rotation-hsluv 6 bg)
      (-map (lambda (c)
              (ct-iterate c
                (-compose 'ct-edit-hsluv-l-dec 'ct-edit-hsluv-s-inc)
                (lambda (step) (> (ct-contrast-ratio step bg) fg-ratio)))))
      ;; todo: revisit assumed color
      (-select-by-indices '(2 1 0 4))
      (-interleave '(:primary :assumed :strings :alt))
      (-partition 2)
      (-map (-lambda ((k v)) (ht-set return k v))))

    return))

(defun myron-storm-create ()
  "Create the colors for the storm theme."
  (-let*
    ((background    (ct-make-lab 94 -5 0))
      (background>  (ct-change background 5 'ct-edit-hsluv-l-dec))
      (background>> (ct-change background 7 'ct-edit-hsluv-l-dec))
      (normal-parts (myron-storm-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background+ (ct-edit-hsluv-l primary (ct-get-hsluv-l background>))))

    (ht
      (:normal normal-parts)
      (:weak (myron-storm-colors background>))
      (:strong (myron-storm-colors background>>))
      (:focused (myron-storm-colors background+)))))

(deftheme myron-storm)
(myron-themes--define 'myron-storm)

(provide-theme 'myron-storm)
(provide 'myron-storm-theme)
;;; myron-storm-theme.el ends here
