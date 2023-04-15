;; -*- lexical-binding: t; -*-
;; authored during a storm
;; lower contrast
;; todo: revisit assumed color in this theme

(require 'myron-themes)

(defun myron-storm-colors (bg)
  (let ((fg-ratio 4)
         (return (ht)))

    (ht-set return :background bg)
    (ht-set return :foreground (ct-tint-ratio bg bg fg-ratio))
    (ht-set return :faded (ct-tint-ratio bg bg (- fg-ratio 1)))

    (->> (ct-rotation-hsluv bg 60)
      (-map (lambda (c)
              (ct-iterate c
                (-compose 'ct-edit-hsluv-l-dec 'ct-edit-hsluv-s-inc)
                (lambda (step) (> (ct-contrast-ratio step bg) fg-ratio)))))
      (-select-by-indices '(2 1 0 4))
      ;; (myron-take '(3 2 1 0))
      (-interleave '(:primary :assumed :strings :alt))
      (-partition 2)
      (-map (-lambda ((k v)) (ht-set return k v))))

    return))

(defun myron-storm-create ()
  (-let*
    ((background (ct-make-lab 94 -5 0))

      (background> (ct-iterate background 'ct-edit-hsluv-l-dec
                     (fn (> (ct-name-distance <> background) 4))))

      (background>> (ct-iterate background 'ct-edit-hsluv-l-dec
                      (fn (> (ct-name-distance <> background) 7))))

      (normal-parts (myron-storm-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background+ (ct-edit-hsluv-l primary (ct-get-hsluv-l background>))))

    (ht
      (:normal normal-parts)
      (:weak (myron-storm-colors background>))
      (:strong (myron-storm-colors background>>))
      (:focused (myron-storm-colors background+)))))

(deftheme myron-storm)
(myron-theme-define 'myron-storm 'myron-storm-create
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:normal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#e3f0ed" :foreground "#697673" :faded "#7e8b88" :primary "#a356a4" :assumed "#108082" :strings "#0e8618" :alt "#a95e5b")) :weak #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#d0ddda" :foreground "#5d6a67" :faded "#717e7b" :primary "#954b96" :assumed "#087376" :strings "#077911" :alt "#9b534e")) :strong #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#c3d0cd" :foreground "#55625f" :faded "#687572" :primary "#8b418d" :assumed "#026a6f" :strings "#006f0a" :alt "#924942")) :focused #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:background "#eed1ee" :foreground "#7b5e7b" :faded "#8f728f" :primary "#a94b0c" :assumed "#a9494b" :strings "#a23ba2" :alt "#007470")))))

(provide-theme 'myron-storm)
(provide 'myron-storm-theme)
;;; myron-storm-theme.el ends here
