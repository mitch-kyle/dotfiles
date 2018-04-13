;; Load modules
(require 'prelude-erc)
(require 'prelude-ido)
(require 'prelude-company)

;;; languages support
;; TODO load these the first time editing associated file typed
(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-common-lisp)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-erlang)
(require 'prelude-elixir)
(require 'prelude-go)
(require 'prelude-haskell)
(require 'prelude-js)
(require 'prelude-latex)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-shell)
(require 'prelude-scss)
(require 'prelude-web)
(require 'prelude-xml)

(prelude-require-packages '(rainbow-delimiters
                            restclient
                            powerline
			    auto-compile
                            diminish
                            emojify))

(setq prelude-flyspell nil)
(setq inhibit-startup-message t) ; Don't want any startup message
(setq initial-scratch-message nil) ; Don't print a header in the scratch buffer
(setq search-highlight t) ; Highlight search object
(setq query-replace-highlight t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening
(setq window-divider-default-right-width 1)
(setq tab-width 4)
(setq whitespace-line-column 120)
(setq prelude-guru nil)

(defun personal-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(cua-mode 1)
(global-linum-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(window-divider-mode t)
(global-hl-line-mode -1)

;; Diminish hides modes from the modeline
(diminish 'company-mode)
(diminish 'prelude-mode)
(diminish 'beacon-mode)
(diminish 'editorconfig-mode)
(diminish 'flyspell-mode)
(diminish 'which-key-mode)
(diminish 'whitespace-mode)
(diminish 'flycheck-mode)

(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)


(setq emojify-display-style 'image) ;; gtk emacs doesn't always like colored symbols (2018)
(global-emojify-mode t)
(global-emojify-mode-line-mode t)

(show-paren-mode t)
(set-face-foreground 'show-paren-match "DimGrey")

;; ibuffer columns
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 40 40 :left :elide) ;; 40 40 is the column width
              " "
              (size 9 -1 :right)
              " "
              (mode 8 8 :left :elide)
              " "
              filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Help\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Packages\\*$")
                         (name . "^\\*Compile-Log\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*Backtrace\\*$")
                         (name . "^\\*Emojis\\*$")
                         (name . "^\\*eshell\\*$")
                         (name . "^\\*terminal\\*$")
                         (name . "^\\*ansi-term\\*$")))
               ("AURel" (or (mode . aurel-info-mode)
                            (mode . aurel-list-mode)
                            (mode . aurel-revert-tail-mode)))
               ("Cider" (or (name . "^\\*nrepl-.*\\*$")
                            (name . "^\\*cider-.*\\*$")))
               ("IRC" (mode . erc-mode))
               ("X Windows" (mode . exwm-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; TODO this fails when ediff complains about a buffer already open for a file being merged
(defun personal-ediff-janitor ()
  "Delete buffers and restore window on ediff exit."
  (let* ((ctl-buf ediff-control-buffer)
         (ctl-win (ediff-get-visible-buffer-window ctl-buf))
         (ctl-frm ediff-control-frame)
         (main-frame (cond ((window-live-p ediff-window-A)
                            (window-frame ediff-window-A))
                           ((window-live-p ediff-window-B)
                            (window-frame ediff-window-B)))))
    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
    (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
    (ediff-kill-buffer-carefully ediff-tmp-buffer)
    (ediff-kill-buffer-carefully ediff-error-buffer)
    (ediff-kill-buffer-carefully ediff-msg-buffer)
    (ediff-kill-buffer-carefully ediff-debug-buffer)
    (when (boundp 'ediff-patch-diagnostics)
      (ediff-kill-buffer-carefully ediff-patch-diagnostics))
    (cond ((and (ediff-window-display-p)
                (frame-live-p ctl-frm))
           (delete-frame ctl-frm))
          ((window-live-p ctl-win)
           (delete-window ctl-win)))
    (unless (ediff-multiframe-setup-p)
      (ediff-kill-bottom-toolbar))
    (ediff-kill-buffer-carefully ctl-buf)
    (when (frame-live-p main-frame)
      (select-frame main-frame)))
  (ediff-janitor nil nil)
  (winner-undo))

(add-hook 'ediff-cleanup-hook 'personal-ediff-janitor)

;;; IRC config
(setq erc-nick "tavoris")

(defun irc-identify (&optional SERVER NICK)
  (interactive)
  (erc-message "PRIVMSG" (concat "NickServ identify "
                                 (read-passwd (concat "IRC Password"
                                                      (when NICK (format " (%s)" NICK))
                                                      ": ")))))

(add-hook 'erc-after-connect 'irc-identify)


(defun toggle-transparency ()
  "Toggle off window transparency"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(95 . 95) '(100 . 100)))))

(prelude-install-search-engine "personal-github" "https://github.com/" "Github [user[/repo]]: ")
(define-key prelude-mode-map (kbd "\C-c G") #'prelude-personal-github)

(global-set-key "\C-l" #'goto-line)
(global-set-key (kbd "\C-c h") #'replace-string)
(global-set-key (kbd "M-<tab>") #'ido-imenu-anywhere)
(global-set-key (kbd "C-<tab>") #'personal-switch-to-last-buffer)
(global-set-key (kbd "C-\;") #'comment-or-uncomment-region)
(global-set-key (kbd "s-e") #'emojify-insert-emoji)
(global-set-key (kbd "s-p s-p") #'projectile-switch-project)
(global-set-key (kbd "s-p s-f") #'projectile-find-file)


(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(declare cider-show-error-buffer cider-auto-select-error-buffer)
(eval-after-load 'cider
  '(progn
     (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
     (add-hook 'cider-repl-mode-hook #'company-mode)
     (add-hook 'cider-mode-hook #'company-mode)))

(defun personal-split-window (&optional window)
  "Split windows more senibly"
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function #'personal-split-window)

(defun personal-lisp-coding-defaults ()
  (rainbow-delimiters-mode t))

(setq prelude-lisp-coding-hook #'personal-lisp-coding-defaults)

(defun personal-interactive-lisp-coding-defaults ()
  (rainbow-delimiters-mode t)
  (whitespace-mode nil))

(setq prelude-interactive-lisp-coding-hook #'personal-interactive-lisp-coding-defaults)

(remove-hook 'minibuffer-setup-hook #'conditionally-enable-smartparens-mode)

(defun personal-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (and (executable-find ispell-program-name)
             prelude-flyspell)
    (flyspell-prog-mode))
  (when prelude-guru
    (guru-mode +1))
  (prelude-enable-whitespace)
  (prelude-local-comment-auto-fill)
  (prelude-font-lock-comment-annotations))

(setq prelude-prog-mode-hook #'personal-prog-mode-defaults)

(setenv "ENVIRONMENT" "dev")

(provide 'personal)

;;; personal.el ends here
