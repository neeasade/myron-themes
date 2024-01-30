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
                 (cond
                   ((eq :strings label)
                     (-> (ct-contrast-min background background contrast)
                       (ct-edit-hsluv-h hue)
                       (ct-edit-lch-c 100)
                       (ct-complement)))
                   ((-contains-p '(:primary :alt :strings) label)
                     (-> (ct-contrast-min background background contrast)
                       (ct-edit-hsluv-h hue)
                       ;; (ct-edit-hsluv-s 100)
                       (ct-edit-lch-c 100)))
                   (t (ct-contrast-min background background contrast))))))
    ;; (-mapcat (-lambda ((label contrast))
    ;;            (list label (ct-contrast-min background background contrast))))
    (ht<-plist)
    ))

(defun myron-room-create ()
  (-let*
    (;; maybe a darker bg
      (background "#f0f0f0")

      (hue (ct-get-hsluv-h "#ebe5f4"))

      ((background> background>>) (->> '(:weak 4
                                          :strong 7)
                                    (-partition 2)
                                    (-map (-lambda ((label distance))
                                            (myron-cdist background distance 'ct-edit-lab-l-dec)))))
      (background+ (-> (ht-get (myron-room-colors background hue) :alt)
                     ;; (ct-edit-lch-h 40)
                     (ct-complement)
                     (ct-edit-lch-c 100)
                     (ct-edit-hsluv-l (ct-get-hsluv-l background>))
                     )))
    (ht<-plist
      (list
        :focused (myron-room-colors background+ hue)
        :normal (myron-room-colors background hue)
        :weak (myron-room-colors background> hue)
        :strong (myron-room-colors background>> hue)))))

(deftheme myron-room)
(myron-theme-define 'myron-room)
(myron-evil-cursor-color (myron-get :alt))

(provide-theme 'myron-room)
(provide 'myron-room-theme)
;;; myron-room-theme.el ends here
