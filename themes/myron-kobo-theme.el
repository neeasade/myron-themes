;; -*- lexical-binding: t; -*-
;; bg and fg inspired by my kobo reader running plato
;; the hue is to add a tint of spice

(require 'myron-themes)

(defun myron-kobo-colors (background hue)
  "get the foreground colors against a specific background"
  (->> '(:background 0
          :foreground 5.5
          :assumed 5.5
          :strings 3
          :alt 3
          :faded 4
          :primary 4)
    (-partition 2)
    (-mapcat (-lambda ((label contrast))
               (let ((color (ct-contrast-min background background contrast)))
                 (if (-contains-p '(:primary :alt :strings) label)
                   (list label (ct-aedit-hsluv color (list hue 100 l)))
                   (list label color)))))
    (ht<-plist)))

(defun myron-kobo-create ()
  (-let*
    ((background
       ;; to get this bg I took a pic of my kobo with my phone camera
       "#d7d2cf"
       ;; bg fg from camshot of kobo on phone
       ;; (ct-contrast-ratio "#d7d2cf" "#4c4d5a")
       ;; 5.566905771576383
       )
      (hue 90))
    (->> (list
           ;; the unique thing about this theme - a *lighter* weak bg
           :weak (myron-cdist background 1.5 'ct-edit-lab-l-inc)

           ;; torn on whether or not to give focus bg a flare
           ;; syncing strong+focused works b/c in practice, focus selection on a strong bg is not used
           :strong (myron-cdist background 3 'ct-edit-lab-l-dec)
           :focused (myron-cdist background 2.5 'ct-edit-lab-l-dec)
           ;; (-> background
           ;;   (ct-edit-hsluv-h hue)
           ;;   (ct-edit-hsluv-s 10)
           ;;   (myron-cdist 1.5 'ct-edit-lab-l-dec))
           )
      (-partition 2)
      (append `((:normal ,background)))
      (-mapcat (-lambda ((label bg)) (list label (myron-kobo-colors bg hue))))
      (ht<-plist))))

(deftheme myron-kobo)
(myron-theme-define 'myron-kobo
  (lambda () '((font-lock-type-face :foreground foreground))))
(myron-evil-cursor-color (myron-get :faded))

(provide-theme 'myron-kobo)
(provide 'myron-kobo-theme)
;;; myron-kobo-theme.el ends here
