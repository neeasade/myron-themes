;; -*- lexical-binding: t; -*-
;; Namesake: Jamie McFay, from the James Clavell novel Gai-Jin

(require 'myron-themes)

(defun myron-mcfay-colors (background)
  "get the foreground colors against a specific background"
  (let*
    ((colors (-->
               (ct-make-hsluv 270 75 43.596)
               (ct-rotation-lch it -45)
               (-select-by-indices '(7 1 2 4) it)
               (-map (lambda (c) (ct-edit-hsluv-l c 43.596)) it)
               (-map (lambda (c) (ct-contrast-min c background 4.3)) it))))

    (ht
      (:background background)

      (:foreground (ct-contrast-min background background 7.7))
      (:faded (ct-contrast-min background background 4.3))

      (:primary (nth 0 colors))
      (:assumed (nth 1 colors))
      (:alt (nth 2 colors))
      (:strings (ct-edit-lch-c (nth 3 colors) 100)))))

(defun myron-mcfay-create ()
  (-let*
    (
      ;; /slightly/ cool
      (background (ct-make-lab 93 -0.5 -1))
      (normal-parts (myron-mcfay-colors background))
      ((&hash :alt :assumed :primary :faded :foreground) normal-parts)

      (background>
        (-> background
          (myron-cdist 4 'ct-edit-lab-l-dec)
          (ct-edit-hsluv-h (ct-get-hsluv-h alt))))

      (background>>
        (-> background
          (myron-cdist 7 'ct-edit-lab-l-dec)
          (ct-edit-hsluv-h (ct-get-hsluv-h primary))))

      (background+
        (-> alt
          (ct-edit-lch-c 20)
          (ct-edit-hsluv-l
            (ct-get-hsluv-l background>>)))))

    (ht
      (:focused (myron-mcfay-colors background+))
      (:normal normal-parts)
      (:weak (myron-mcfay-colors background>))
      (:strong (myron-mcfay-colors background>>)))))

(deftheme myron-mcfay)
(myron-theme-define 'myron-mcfay)
(myron-evil-cursor-color (myron-get :alt))

(provide-theme 'myron-mcfay)
(provide 'myron-mcfay-theme)
;;; myron-mcfay-theme.el ends here
