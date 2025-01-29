;; -*- lexical-binding: t; -*-
;; namesake: the Struan family, from James Clavell's Asian Saga series

(require 'myron-themes)

(defun myron-struan-colors (background)
  "Get the struan foreground colors against a specific BACKGROUND."
  (let* ((colors (->> (ct-make-hsluv 265 60 40)
                   (ct-rotation-hsluv 6)
                   (-select-by-indices '(1 5 2 3))
                   (-map (lambda (c) (ct-edit-hsluv-l c 80)))
                   (-map (lambda (c) (ct-contrast-min c background 4.3)))))

          (foreground (ct-contrast-min background background 7))
          (faded (ct-contrast-min (ct-aedit-hsl
                                    ;; (nth 2 colors)
                                    (ct-contrast-min background background 5.5)
                                    (list h 80 70))
                   background 4.3)))

    (ht<-plist (list :background background
                 :foreground foreground
                 :faded faded
                 :primary (nth 0 colors)
                 :assumed (nth 1 colors)
                 :alt (nth 2 colors)
                 :strings (-> (nth 3 colors)
                            (ct-aedit-lch (list (+ l 10) 100 h))
                            (ct-contrast-min background 4.3))))))

(defun myron-struan-create ()
  "Create the colors for the struan theme."
  (-let*
    ((background (ct-make-lab 93 2 4))
      (normal-parts (myron-struan-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background>
        (-> background
          (ct-change 4 'ct-edit-lab-l-dec)
          (ct-steal 'hsluv-h alt)))

      (background>>
        (-> background
          (ct-change 7 'ct-edit-lab-l-dec)
          (ct-steal 'hsluv-h primary)))

      (background+
        (-> alt
          (ct-edit-lch-c 25)
          (ct-steal 'hsluv-l background>>))))

    (ht
      (:normal  normal-parts)
      (:weak    (myron-struan-colors background>))
      (:strong  (myron-struan-colors background>>))
      (:focused (ht-merge normal-parts (ht (:background background+)))))))


(deftheme myron-struan)
(myron-themes--define 'myron-struan)

(provide-theme 'myron-struan)
(provide 'myron-struan-theme)
;;; myron-struan-theme.el ends here
