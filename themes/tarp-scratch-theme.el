;; -*- lexical-binding: t; -*-

(require 'tarps)

(defun tarp/scratch-get-colors (&optional bg)
  (let ((bg (or bg (ct-make-lab 94 0 4)))
         (fg-ratio 3.0)
         (return (ht)))

    (ht-set return :background bg)
    (ht-set return :foreground (ct-tint-ratio bg bg fg-ratio))
    (ht-set return :faded (ct-tint-ratio bg bg (- fg-ratio 1)))

    (->>
      (ct-rotation-hsluv
        (ct-edit-hsluv-h-inc bg 10)
        120)
      (-map (lambda (c)
              (ct-iterate c
                (apply '-compose
                  `(
                     ;; ,@(-repeat 3 'ct-edit-hsluv-l-dec)
                     ;; ct-edit-hsluv-dec
                     ct-edit-lab-b-inc
                     )

                  ;; 'ct-edit-lab-b-inc


                  )
                (lambda (step) (> (ct-contrast-ratio step bg) fg-ratio)))))
      ;; todo: revisit builtin color
      ;; (tarp/take '(2 1 0 4))
      ;; (tarp/take '(3 2 1 0))
      ;; (-interleave '(:primary :assumed :strings :alt))
      ;; (-partition 2)
      ;; (-map (-lambda ((k v)) (ht-set return k v)))
      )

    ;; return
    )
  )

(ct-make-lab 60 -100 100)

"#00b100"

"#af8d00"

(tarp/scratch-get-colors)

("#f05243" "#e05f5f" "#f05054")

("#9f8172" "#8f8484" "#9f7f83")

("#8c7f70" "#7c8282" "#8c7d81")

("#8c897a" "#7c8c8c" "#8c878b")

("#282416" "#182728" "#282227")

("#8c847a" "#7c878c" "#8c828b")


("#9c7d8a" "#8f849f" "#9c7b9b")

("#b373a1" "#a478b4" "#b371b2")

("#c56c88" "#b6729c" "#c56a99")

("#bf727f" "#bd709c" "#c56a97")


(-let*
  (
    (colors-normal (tarp/scratch-get-colors))
    ((&hash :background :alt :strings :assumed :primary :faded :foreground) colors-normal)

    (background> (ct-iterate background 'ct-edit-hsluv-l-dec
                   (fn (> (ct-name-distance <> background) 4))))

    (background>> (ct-iterate background 'ct-edit-hsluv-l-dec
                    (fn (> (ct-name-distance <> background) 7))))

    (background+ (ct-edit-hsluv-l primary (ct-get-hsluv-l background>>))))

  (setq tarp/theme*
    (ht
      (:normal colors-normal)
      (:weak (tarp/scratch-get-colors background>))
      (:strong (tarp/scratch-get-colors background>>))
      (:focused (tarp/scratch-get-colors background+)))))

(deftheme tarp-scratch)
(tarp/base16-theme-define 'tarp-scratch)

(provide-theme 'tarp-scratch)
