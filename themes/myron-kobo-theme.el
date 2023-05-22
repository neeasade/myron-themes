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
               (list label
                 (if (-contains-p '(:primary :alt :strings) label)
                   (-> background
                     (ct-contrast-min background contrast)
                     (ct-edit-hsluv-h hue)
                     (ct-edit-hsluv-s 100))
                   (ct-contrast-min background background contrast)))))
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
    (->> '(:focused 7
            :weak 4
            :strong 7)
      (-partition 2)
      (-map (-lambda ((label distance))
              (list label
                (if (eq label :focused)
                  (-> background
                    (ct-aiterate 'ct-edit-lab-l-dec (> (ct-distance C C0) distance))
                    (ct-edit-hsluv-h hue))
                  (ct-aiterate background 'ct-edit-lab-l-dec
                    (> (ct-distance C C0) distance))))))
      (append `((:normal ,background)))
      (-mapcat (-lambda ((label bg)) (list label (myron-kobo-colors bg hue))))
      (ht<-plist))))

(deftheme myron-kobo)
(myron-theme-define 'myron-kobo)

(provide-theme 'myron-kobo)
(provide 'myron-kobo-theme)
;;; myron-kobo-theme.el ends here
