;; -*- lexical-binding: t; -*-
;; It's a shadowy dusk here, and I am merely mortal.
;; a theme wherein we play with the hct colorspace

;; playing with hct, advice I read:
;; use single hue, use tone for emphasis, and /maybe/ some chroma
;; I shall complement

(require 'myron-themes)

(defun myron-mortal-colors (background)
  "Get the mortal foreground colors against a specific BACKGROUND."
  ;; (let* (
  ;;         ()
  ;;         )
  ;;   (ht<-plist
  ;;     `(:background ,background
  ;;        ;; thought; complement colors for fg?
  ;;        :foreground ,(ct-contrast-min background background 5.0 'hct-t)
  ;;        :faded ,(ct-contrast-min background background 3.0 'hct-t)
  ;;        :assumed ,(ct-contrast-min background background 3.0 'hct-t)
  ;;        :primary
  ;;        :alt
  ;;        :strings
  ;;        )))
  ;;
  (-let* (
           ;; applied below
           (contrast-boost 0.2)
           (og-hue (ct-get-hct-h background))

           ;; (list
           ;;   (+ og-hue 180)
           ;;   ;; this range is useless for me
           ;;   ;; ...
           ;;   (+ og-hue 120 -30)
           ;;   (+ og-hue 120 120))

           ;; c is complement, a is original
           ;; ｔｅｔｒａｄｉｃ
           ((a b c d) (-iota 4 og-hue 90))

           ;; to consider:
           ;; (pair-offset 45)
           ;; ((a b c d)
           ;;   (list
           ;;     og-hue
           ;;     (+ pair-offset og-hue)
           ;;     (+ 180 og-hue)
           ;;     (+ 180 pair-offset og-hue)))
           )
    ;; (prn a b c d)
    (->> (list
           ;; contrast, c level, hue
           :foreground  5   6    a
           :faded       3   1    a
           :assumed     4   40   c
           :strings     4   80   b
           :alt         3.3 100  d ; *pop*
           :primary     5   100  c
           )

      (-partition 4)
      (-mapcat (-lambda ((label contrast c-set hue))
                 (list label
                   ;; intuition: darken a smidge, fuck with it, then contrast it
                   (-> background
                     (ct-edit-hct-t-dec 10)
                     (ct-edit-hct (lambda (h c tt)
                                    (list (or hue h)
                                      ;; (* c-mult c)
                                      c-set
                                      tt)))
                     (ct-contrast-min background (+ contrast-boost contrast) 'hct-t)))))
      (-concat (list :background background))
      (ht<-plist))))

(defun myron-mortal-create ()
  "Create the colors for the mortal theme."
  (-let* (
           ;; (ct-rotation-hct 12 "#fbe6e1")
           (seed "#fbe6e1")
           (b seed)

           (b (ct-edit-hct-t-inc seed 2))
           (b (ct-edit-hct-t-inc seed 1.5))

           ;; todo: bump chroma?
           ;; thought: what if we used contrast to dec tone instead of a solid value
           (b> (ct-edit-hct-t-dec seed 6))
           (b>> (ct-edit-hct-t-dec seed 12))

           (b> (ct-edit-hct-t-dec seed 5))
           (b>> (ct-edit-hct-t-dec seed 10))

           (b+ (-> b>
                 (ct-edit-hct-c 100)
                 (ct-complement-hct))))

    (ht<-plist
      (list
        :focused (myron-mortal-colors b+)
        :normal  (myron-mortal-colors b)
        :weak    (myron-mortal-colors b>)
        :strong  (myron-mortal-colors b>>)))))

(deftheme myron-mortal)

(myron-themes--define 'myron-mortal
  ;; ehh
  ;; '((font-lock-comment-face :slant italic))
  )

;; (myron-themes-evil-cursor-color (myron-themes-get :assumed))

(provide-theme 'myron-mortal)

(provide 'myron-mortal-theme)
;;; myron-mortal-theme.el ends here
