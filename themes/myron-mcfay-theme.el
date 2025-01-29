;; -*- lexical-binding: t; -*-
;; namesake: Jamie McFay, from the James Clavell novel Gai-Jin

(require 'myron-themes)

(defun myron-mcfay-colors (background)
  "Get the mcfay foreground colors against a specific BACKGROUND."
  (let* ((colors (->> (ct-make-hsluv 270 75 43.596)
                   (ct-rotation-lch -8)
                   (-select-by-indices '(7 1 2 4))
                   (-map (lambda (c) (ct-edit-hsluv-l c 43.596)))
                   (-map (lambda (c) (ct-contrast-min c background 4.3))))))

    (ht
      (:background background)
      (:foreground (ct-contrast-min background background 7.7))
      (:faded (ct-contrast-min background background 4.3))
      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt     (nth 2 colors))
      (:strings (ct-edit-lch-c (nth 3 colors) 100)))))

(defun myron-mcfay-create ()
  "Create the colors for the mcfay theme."
  (-let*
    (
      ;; /slightly/ cool
      (background (ct-make-lab 93 -0.5 -1))
      (normal-parts (myron-mcfay-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background> (-> background
                     (ct-change 4 'ct-edit-lab-l-dec)
                     (ct-steal 'hsluv-h alt)))

      (background>> (-> background
                      (ct-change 7 'ct-edit-lab-l-dec)
                      (ct-steal 'hsluv-h primary)))

      (background+ (-> alt
                     (ct-edit-lch-c 20)
                     (ct-steal 'hsluv-l background>>))))

    (ht
      (:focused (myron-mcfay-colors background+))
      (:normal normal-parts)
      (:weak (myron-mcfay-colors background>))
      (:strong (myron-mcfay-colors background>>)))))

(deftheme myron-mcfay)
(myron-themes--define 'myron-mcfay)
(myron-themes-evil-cursor-color (myron-themes-get :alt))

(provide-theme 'myron-mcfay)
(provide 'myron-mcfay-theme)
;;; myron-mcfay-theme.el ends here
