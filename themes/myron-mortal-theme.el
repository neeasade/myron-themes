;; -*- lexical-binding: t; -*-
;; It's a shadowy dusk here, and I am merely mortal.
;; a theme wherein we play with the hct colorspace

(require 'myron-themes)

(defun myron-mortal-colors (background &optional print?)
  "Get the mortal foreground colors against a specific BACKGROUND."
  (-let* (
           (og-hue (ct-get-hct-h background))

           ;; c is complement, a is original
           ;; ｔｅｔｒａｄｉｃ
           ;; ((a b c d) (-iota 4 hue 90))

           ;; to consider:
           (o 60)
           ;; pairs: ac and bd
           ((a b c d)
             (list
               og-hue
               (+ og-hue o)
               (+ og-hue 180)
               (+ og-hue o 180)))
           (contrast-boost .6)
           ;; (contrast-boost 1)
           )

    (when print?
      ;; visualize
      (->> (list a b c d)
        (-map (lambda (hue)
                (prn
                  (-> background
                    (ct-edit-hct-t-dec 10)
                    (ct-edit-hct-h hue)
                    (ct-edit-hct-c 50)))))))


    ;; todo: there's a problem here.
    ;; we've been complecting "highlighting" and "types"
    ;; maybe "types" can be "highlighting" with less chroma
    ;; we need a meta -> highlight color for character matching, can't get away with it here


    ;; okay what if we ignore the first hue. we h8 that shit anyway
    ;; "#ffc6b6"
    ;; "#dfd758"
    ;; "#57e4ff"
    ;; "#d3cdff"

    (->> (list
           ;; contrast hue chroma
           :foreground  4.0  a 7
           :assumed     4.0  c 40

           :primary     4.0  b 10
           :alt         3.0  a 90

           :faded       2.5  a 40
           :strings     3.0  d 50

           ;; highlight value
           ;; :alt         3    80  b
           )
      (-partition 4)
      (-mapcat (-lambda ((label contrast hue chroma))
                 (list label
                   (-> background
                     ;; important to contrast first and then apply chroma, else we nuke it
                     (ct-contrast-min background (+ contrast-boost contrast) 'hct-t)
                     (ct-edit-hct (lambda (_ _ tone) (list hue chroma tone)))))))
      (-concat (list :background background))
      (ht<-plist))))

(defun myron-mortal-create ()
  "Create the colors for the mortal theme."
  (-let* (
           ;; (ct-rotation-hct 12 "#fbe6e1")
           (seed "#fbe6e1")
           (b seed)

           (b (ct-edit-hct-t-inc seed 2))

           (b (ct-edit-hct-t-inc seed 1.6))

           ;; (b> (ct-edit-hct-t-dec seed 6))
           ;; (b>> (ct-edit-hct-t-dec seed 12))

           ;; (b> (ct-edit-hct-t-dec seed 5))
           ;; (b>> (ct-edit-hct-t-dec seed 9))

           (b> (ct-aedit-hct seed  (list h (* 1.5 c) (- tt 5))))
           (b>> (ct-aedit-hct seed (list h (* 2 c)   (- tt 8))))

           (b+ (-> b>
                 (ct-complement-hct)
                 (ct-edit-hct-c 25)
                 ;; (ct-aedit-hct-h (+ 180 60 h))
                 )))

    (ht<-plist
      (list
        :focused (myron-mortal-colors b+)
        :normal  (myron-mortal-colors b t)
        :weak    (myron-mortal-colors b>)
        :strong  (myron-mortal-colors b>>)))))

(deftheme myron-mortal)

(myron-themes--define 'myron-mortal
  ;; '((font-lock-comment-face :slant italic))
  `(
     ;; todo: might not need to tweak org-link with type/highlight separation
     ;; (org-link :foreground
     ;;   ;; ,(myron-themes-get :alt :focused)
     ;;   ,(myron-themes-get :strings :weak)
     ;;   )
     ;; (consult-preview-match :foreground "#ffffff")
     ((orderless-match-face-0 orderless-match-face-1 orderless-match-face-2 orderless-match-face-3)
       :foreground
       ,(-> (myron-themes-get :strings)
          (ct-edit-hct-c 60)))
     ))

;; (myron-themes-evil-cursor-color (myron-themes-get :assumed))

(provide-theme 'myron-mortal)

(provide 'myron-mortal-theme)
;;; myron-mortal-theme.el ends here
