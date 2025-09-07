;; -*- lexical-binding: t; -*-
;; a soft warm theme

(require 'myron-themes)

(defun myron-whisper-colors (background hue)
  "Get the whisper foreground colors against a specific BACKGROUND."
  (->> '(
          ;; :background 0
          ;; :foreground 5.5
          ;; :assumed 5.5
          :foreground 3
          :assumed 3
          :strings 3
          :alt 3
          :faded 4
          :primary 4
          )
    (-partition 2)
    (-mapcat (-lambda ((label contrast))
               (let ((color (ct-contrast-min background background 4.6)))
                 (if (-contains-p '(:primary :alt :strings) label)
                   (list label (ct-aedit-lch color (list l 100 hue)))
                   ;; (list label (ct-aedit-hsluv color (list hue 100 l)))
                   (list label color)))))
    (ht<-plist)
    (ht-merge (-ht :background background))))

(defun myron-whisper-create ()
  "Create the colors for the whisper theme."
  ;; to get this bg I took a pic of my whisper with my phone camera
  (-let* (
           (bg-hue 0)

           (background (ct-make-hsv bg-hue 12 100))

           (hue (mod (+ bg-hue 180) 360))
           (hue 200)
           (hue 190)
           )
    (->> (list
           :normal background

           :weak (ct-change background 1.5 'ct-edit-lab-l-dec)
           :strong (ct-change background 3 'ct-edit-lab-l-dec)

           :focused
           (->
             (ct-change background 3 'ct-edit-lab-l-dec)
             (ct-edit-hsluv-s 100)
             ;; (ct-edit-lch-h-inc bg-hue)
             )

           ;; (-> background
           ;;   ;; (ct-edit-hsluv-h hue)
           ;;   (ct-edit-hsluv-s-inc 10)
           ;;   (ct-change 1.5 'ct-edit-lab-l-dec))
           )
      (-partition 2)
      (-mapcat (-lambda ((label bg)) (list label (myron-whisper-colors bg hue))))
      (ht<-plist))))

(deftheme myron-whisper)

(myron-themes--define 'myron-whisper
  '((font-lock-type-face :foreground foreground)
     (cursor :background faded)))

(provide-theme 'myron-whisper)
(provide 'myron-whisper-theme)
;;; myron-whisper-theme.el ends here
