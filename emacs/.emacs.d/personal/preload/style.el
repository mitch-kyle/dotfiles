;; Set up some inital window style configuration
(require 'prelude-packages)

(set-default-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso10646-1")
(set-frame-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso10646-1")

;; Add font with better unicode coverage
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


(prelude-require-packages '(monokai-theme
                            powerline))


(setq prelude-theme 'monokai)
(setq powerline-default-separator 'contour)

(require 'prelude-custom)
(require 'prelude-ui)

(powerline-default-theme)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
