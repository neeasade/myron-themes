;; -*- lexical-binding: t; -*-
;; namesake: the Struan family, from James Clavell's Asian Saga series

(require 'myron-themes)

(defun myron-struan-colors (background)
  "get the foreground colors against a specific background"
  (let* ((colors (->> (ct-rotation-hsluv (ct-make-hsluv 265 60 40) 60)
                   (-select-by-indices '(1 5 2 3))
                   (-map (fn (ct-edit-hsluv-l <> 80)))
                   (-map (fn (ct-tint-ratio <> background 4.3)))))

          (foreground (ct-tint-ratio background background 7))

          (faded
            (ct-tint-ratio
              (ct-edit-hsl
                (ct-tint-ratio background background 5.5)
                ;; (nth 2 colors)
                (lambda (h s l) (list h 80 70)))
              background 4.3)))

    (ht<-plist (list :background background
                 :foreground foreground
                 :faded faded
                 :primary (nth 0 colors)
                 :assumed (nth 1 colors)
                 :alt (nth 2 colors)
                 :strings (-> (nth 3 colors)
                            (ct-edit-lch-l (-partial '+ 10))
                            (ct-edit-lch-c 100)
                            (ct-tint-ratio background 4.3))))))

(defun myron-struan-create ()
  (-let*
    ((background (ct-make-lab 93 2 4))
      (normal-parts (myron-struan-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background>
        (-> background
          (ct-iterate 'ct-edit-lab-l-dec
            (fn (> (ct-distance <> background) 4)))
          (ct-edit-hsluv-h (ct-get-hsluv-h alt))))

      (background>>
        (-> background
          (ct-iterate 'ct-edit-lab-l-dec
            (fn (> (ct-distance <> background) 7)))
          (ct-edit-hsluv-h (ct-get-hsluv-h primary))))

      (background+
        (-> alt
          (ct-edit-lch-c 25)
          (ct-edit-hsluv-l
            (ct-get-hsluv-l background>>)))))

    (ht
      (:focused (ht-merge normal-parts (ht (:background background+))))
      (:normal normal-parts)
      (:weak (myron-struan-colors background>))
      (:strong (myron-struan-colors background>>)))))

(deftheme myron-struan)
(myron-theme-define 'myron-struan)

(provide-theme 'myron-struan)
(provide 'myron-struan-theme)
;;; myron-struan-theme.el ends here
