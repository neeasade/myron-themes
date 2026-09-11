;; -*- lexical-binding: t; -*-
;; It's a shadowy dusk here, and I am merely mortal.
;; a theme wherein we play with the hct colorspace

(require 'myron-themes)

(defun myron-mortal-colors (background &optional print?)
  "Get the mortal foreground colors against a specific BACKGROUND."
  (-let* (
           (hue (ct-get-hct-h background))

           ;; c is complement, a is original
           ;; ｔｅｔｒａｄｉｃ
           ;; ((a b c d) (-iota 4 hue 90))

           ;; to consider:
           (o 60)
           ;; pairs: ac and bd
           ((a b c d)
             (list
               hue
               (+ hue o)
               (+ hue 180)
               (+ hue o 180)))
           (contrast-boost .6))

    (when print?
      ;; visualize
      (->> (list a b c d)
        (-map (lambda (hue)
                (prn
                  (-> background
                    (ct-edit-hct-t-dec 10)
                    (ct-edit-hct-h hue)
                    (ct-edit-hct-c 50)))))))


    (->> (list
           ;; contrast chroma hue
           :assumed     4    40   a
           :foreground  5    6    c
           :primary     4.5  100  c

           ;; todo: there's a problem here.
           ;; we complect "highlighting" and "types"
           ;; maybe "types" can be "highlighting" with less chroma
           ;; we need a meta -> highlight color for character matching, can't get away with it here
           :alt         3    45  b

           ;; highlight value
           ;; :alt         3    80  b

           :faded       2.5    14   d
           :strings     3    100   d
           )
      (-partition 4)
      (-mapcat (-lambda ((label contrast chroma hue))
                 (list label
                   ;; intuition: darken a smidge, fuck with it, then contrast it
                   (-> background
                     (ct-edit-hct-t-dec 10)
                     (ct-edit-hct (lambda (h c tt)
                                    (list hue
                                      ;; (* c-mult c)
                                      chroma
                                      ;; c
                                      tt)))
                     ;; (myron-mortal-min background (+ contrast-boost contrast))
                     (ct-contrast-min background (+ contrast-boost contrast) 'hct-t)
                     ))))
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

           ;; (b> (ct-edit-hct-t-dec seed 6))
           ;; (b>> (ct-edit-hct-t-dec seed 12))

           ;; (b> (ct-edit-hct-t-dec seed 5))
           ;; (b>> (ct-edit-hct-t-dec seed 9))

           (b> (ct-aedit-hct seed  (list h (* 1.5 c) (- tt 5))))
           (b>> (ct-aedit-hct seed (list h (* 2 c)   (- tt 8))))

           (b+ (-> b>
                 (ct-edit-hct-c 90)
                 (ct-complement-hct)
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
     (org-link :foreground

       ;; ,(myron-themes-get :alt :focused)
       ,(myron-themes-get :strings :weak)
       )
     ;; (consult-preview-match :foreground "#ffffff")
     ((orderless-match-face-0 orderless-match-face-1 orderless-match-face-2 orderless-match-face-3)
       :foreground
       ,(->
          (myron-themes-get :alt)
          (ct-edit-hct-c 80)))))

;; (myron-themes-evil-cursor-color (myron-themes-get :assumed))

(provide-theme 'myron-mortal)

(provide 'myron-mortal-theme)
;;; myron-mortal-theme.el ends here
