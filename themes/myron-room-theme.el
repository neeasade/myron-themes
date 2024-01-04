;; -*- lexical-binding: t; -*-
;; it's warm outside, it's cool in here
;; I'm not feeling very well

(require 'myron-themes)

(defun myron-room-colors (background hue)
  "get the foreground colors against a specific background"
  (->> '(:background 0
          :foreground 6
          :faded 3
          :primary 4
          :assumed 5
          :alt 3
          :strings 4)
    (-partition 2)
    (-mapcat (-lambda ((label contrast))
               (list label
                 (if (-contains-p '(:primary :alt :strings) label)
                   (-> background
                     (ct-contrast-min background contrast)
                     (ct-edit-hsluv-h hue)
                     ;; (ct-edit-hsluv-s 100)
                     (ct-edit-lch-c 100)
                     )
                   (ct-contrast-min background background contrast)))))
    ;; (-mapcat (-lambda ((label contrast))
    ;;            (list label (ct-contrast-min background background contrast))))
    (ht<-plist)))

(defun myron-room-create ()
  (-let*
    ((background "#f7f7f7")

      (hue (ct-get-hsluv-h "#ebe5f4"))
      ;; (hue 90)
      )

    ;; todo: consider making focused inverted (inspo: moe-light)
    (->> '(:focused 3
            :weak 4
            :strong 7)
      (-partition 2)
      (-map (-lambda ((label distance))
              (list label (myron-cdist background distance 'ct-edit-lab-l-dec))))
      (append `((:normal ,background)))
      (-mapcat (-lambda ((label bg)) (list label (myron-room-colors bg hue))))
      (ht<-plist))))

(deftheme myron-room)
(myron-theme-define 'myron-room)
(myron-evil-cursor-color (myron-get :alt))

(provide-theme 'myron-room)
(provide 'myron-room-theme)
;;; myron-room-theme.el ends here
