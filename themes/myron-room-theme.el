;; -*- lexical-binding: t; -*-
;; it's warm outside, it's cool in here
;; I'm not feeling very well

(require 'myron-themes)

(defun myron-room-colors (background hue)
  "Get the room foreground colors against a specific BACKGROUND."
  (->> '(:background 0
          :foreground 6
          :faded 3
          :primary 4
          ;; todo: something with assumed color
          :assumed 6
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
                   ((-contains-p '(:primary :alt) label)
                     (-> (ct-contrast-min background background contrast)
                       (ct-edit-hsluv-h hue)
                       ;; (ct-edit-hsluv-s 100)
                       (ct-edit-lch-c 100)))
                   (t (ct-contrast-min background background contrast))))))
    ;; (-mapcat (-lambda ((label contrast))
    ;;            (list label (ct-contrast-min background background contrast))))
    (ht<-plist)))

(defun myron-room-create ()
  "Create the colors for the room theme."
  (-let*
    (;; maybe a darker bg
      (background "#f0f0f0")

      (hue (ct-get-hsluv-h "#ebe5f4"))

      ((background> background>>) (->> '(:weak 4
                                          :strong 7)
                                    (-partition 2)
                                    (-map (-lambda ((label distance))
                                            (ct-change background distance 'ct-edit-lab-l-dec)))))
      (background+ (-> (ht-get (myron-room-colors background hue) :alt)
                     ;; (ct-edit-lch-h 40)
                     (ct-complement)
                     (ct-edit-lch-c 100)
                     (ct-steal 'hsluv-l background>))))
    (ht<-plist
      (list
        :focused (myron-room-colors background+  hue)
        :normal  (myron-room-colors background   hue)
        :weak    (myron-room-colors background>  hue)
        :strong  (myron-room-colors background>> hue)))))

(deftheme myron-room)
(myron-themes--define 'myron-room
  '((cursor :background alt)))

(provide-theme 'myron-room)
(provide 'myron-room-theme)
;;; myron-room-theme.el ends here
