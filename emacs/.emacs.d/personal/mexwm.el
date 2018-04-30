;;; -*- lexical-binding: t -*-
;; mexwm.el
;;  Configuration for exwm
(require 'prelude-packages)
(prelude-require-packages '(windmove
                            exwm))
(require 'framemove)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)

;; Dialog boxes do not work with exwm
(setq use-dialog-box nil)
(setq display-time-day-and-date t)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(defvar mexwm-tmux-session-name "\"0\"")

(defun mexwm-no-op ()
  "Used to suppress warnings for shortcut keys that already work in hardware"
  (interactive))

;; ;; Launchers

(defun mexwm-run-sh-async (command)
  "Interactive prompt to run a shell command in a child process which may or may not spawn an x window"
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command "" nil command))

(defun mexwm-run-tmux (command)
  "Run a command in a new window of the tmux session"
  (interactive (list (read-shell-command "[tmux]$ ")))
  (start-process-shell-command ""
                               nil
                               (concat "terminator -e 'tmux new-session -AD -c $HOME -s "
                                       mexwm-tmux-session-name
                                       "\\; new-window -c $(pwd) \""
                                       command
                                       "\"'")))

(defmacro mexwm-define-launcher (fun-name command-and-args)
  "Define an interactive function that invokes the shell command given"
  `(defun ,fun-name ()
     (interactive)
     (start-process-shell-command "" nil ,command-and-args)))

(mexwm-define-launcher mexwm-browser (or (getenv "X_BROWSER")
                                         "firefox"))
(mexwm-define-launcher mexwm-tmux-shell-here (concat "terminator -e 'tmux new-session -AD -c $HOME -s "
                                                     mexwm-tmux-session-name
                                                     " \\; new-window -c $(pwd) /usr/bin/zsh'"))
(mexwm-define-launcher mexwm-term (concat "terminator -e 'tmux new-session -AD -c $HOME -s "
                                          mexwm-tmux-session-name
                                          "'"))
(mexwm-define-launcher mexwm-volume-manager "terminator --title Volume -e 'pulsemixer || alsamixer'")
(mexwm-define-launcher mexwm-volume-up "amixer set Master 5%+")
(mexwm-define-launcher mexwm-volume-down "amixer set Master 5%-")
(mexwm-define-launcher mexwm-mute-toggle "amixer set Master toggle")
(mexwm-define-launcher mexwm-mute-mic "amixer set Mic toggle")
(mexwm-define-launcher mexwm-scrot "scrot --select --exec 'mv $f ~/Pictures/screenshots'")
(mexwm-define-launcher mexwm-lock "dm-tool lock")
(mexwm-define-launcher mexwm-music-toggle "mpc toggle")
(mexwm-define-launcher mexwm-music-next "mpc next")
(mexwm-define-launcher mexwm-music-prev "mpc prev")
(mexwm-define-launcher mexwm-music-manager "terminator -e 'ncmpcpp -s playlist -S visualizer'")

;; windmove with framemove integration
(defun mexwm--frame-move (dir)
 (pcase dir
   ('up (fm-up-frame))
   ('down (fm-down-frame))
   ('left (fm-left-frame))
   ('right (fm-right-frame))))

(defun mexwm--do-window-select (dir &optional arg window)
  "Move to the window at direction DIR.
DIR, ARG, and WINDOW are handled as by `windmove-other-window-loc'.
If no window is at direction DIR, an error is signaled."
  (let ((other-window (windmove-find-other-window dir arg window)))
    (cond ((null other-window)
           (mexwm--frame-move dir))
          ((and (window-minibuffer-p other-window)
                (not (minibuffer-window-active-p other-window)))
           (mexwm--frame-move dir))
          (t
           (select-window other-window)))))

(defun mexwm-move-left (&optional arg)
  (interactive "P")
  (mexwm--do-window-select 'left arg))

(defun mexwm-move-up (&optional arg)
  (interactive "P")
  (mexwm--do-window-select 'up arg))

(defun mexwm-move-right (&optional arg)
  (interactive "P")
  (mexwm--do-window-select 'right arg))

(defun mexwm-move-down (&optional arg)
  (interactive "P")
  (mexwm--do-window-select 'down arg))

(defun mexwm-insert (string)
  "Send `string' to clipboard and then send C-v to
application to hopefully trigger the paste operation, `string'
will be inserted into the application."
  (if (derived-mode-p 'exwm-mode)
      (progn
        (kill-new string)
        (dolist (key (string-to-list (kbd "\C-v")))
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))

(defun mexwm-insert-emoji ()
  (interactive)
  (mexwm-insert
   (emojify-completing-read "Insert Emoji: ")))

(defun mexwm--xrandr-update-outputs ()
  (let ((connected-monitors (car
                             (read-from-string
                              ;; TODO write in el
                              (shell-command-to-string
                               "xrandr | awk 'BEGIN {print \"(\"}
                                              / connected/ {print \"\\\"\" $1 \"\\\"\"}
                                              END {print \")\"}'"))))
         (i -1))
    (setq exwm-randr-workspace-output-plist (cl-reduce (lambda (acc s)
                                                         (setq i (+ i 1))
                                                         (append acc (list i s)))
                                                       connected-monitors
                                                       :initial-value '()))
    (setq i (+ i 1))
    (while (> i (exwm-workspace--count))
      (exwm-workspace-add))
    (while (< i (exwm-workspace--count))
      (exwm-workspace-delete (- (exwm-workspace--count) 1)))))

(defun mexwm--rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer

   (concat exwm-class-name ": "
           (if (<= (length exwm-title) 50)
               exwm-title
             (concat (substring exwm-title 0 49) "...")))))

(add-hook 'exwm-update-class-hook 'mexwm--rename-buffer)
(add-hook 'exwm-update-title-hook 'mexwm--rename-buffer)

(defun mexwm--xrandr-init ()
  (add-hook 'exwm-randr-screen-change-hook 'mexwm--xrandr-update-outputs)
  (mexwm--xrandr-update-outputs)
  (exwm-randr--init))

(defun mexwm--xrandr-exit ()
  (remove-hook 'exwm-randr-screen-change-hook 'mexwm--xrandr-update-outputs)
  (exwm-randr--exit))


(add-hook 'exwm-init-hook #'mexwm--xrandr-init)
(add-hook 'exwm-exit-hook #'mexwm--xrandr-exit)

(exwm-config-ido)

(exwm-input-set-key (kbd "s-SPC") #'exwm-input-toggle-keyboard)

;; Do stuff
(exwm-input-set-key (kbd "s-`") #'mexwm-run-sh-async)
(exwm-input-set-key (kbd "s-!") #'mexwm-run-tmux)
(exwm-input-set-key (kbd "s-x s-x") #'execute-extended-command)

;; Navigation
(exwm-input-set-key (kbd "M-<tab>") #'previous-buffer)
(exwm-input-set-key (kbd "M-<iso-lefttab>") #'next-buffer)
(exwm-input-set-key (kbd "M-<left>") #'previous-buffer)
(exwm-input-set-key (kbd "M-<right>") #'next-buffer)

(exwm-input-set-key (kbd "s-<left>") #'mexwm-move-left)
(exwm-input-set-key (kbd "s-<right>") #'mexwm-move-right)
(exwm-input-set-key (kbd "s-<up>") #'mexwm-move-up)
(exwm-input-set-key (kbd "s-<down>") #'mexwm-move-down)

;; Features
(exwm-input-set-key (kbd "s-x e") #'mexwm-insert-emoji)

;; Cheating
(exwm-input-set-key (kbd "s-x b") #'ido-switch-buffer)
(exwm-input-set-key (kbd "s-x s-b") #'personal-ibuffer)


;; Apps
(exwm-input-set-key (kbd "s-x i") #'mexwm-browser)
(exwm-input-set-key (kbd "s-x <return>") #'mexwm-tmux-shell-here)
(exwm-input-set-key (kbd "s-x v") #'mexwm-volume-manager)
(exwm-input-set-key (kbd "s-x l") #'mexwm-lock)
(exwm-input-set-key (kbd "s-l") #'mexwm-lock)
(exwm-input-set-key (kbd "s-<return>") #'mexwm-term)

(exwm-input-set-key (kbd "s-x m") #'mexwm-music-manager)
(exwm-input-set-key (kbd "s-x <down>") #'mexwm-music-toggle)
(exwm-input-set-key (kbd "s-x <left>") #'mexwm-music-prev)
(exwm-input-set-key (kbd "s-x <right>") #'mexwm-music-next)

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'mexwm-volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'mexwm-volume-down)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'mexwm-mute-toggle)
(exwm-input-set-key (kbd "<XF86AudioMicMute>") #'mexwm-mute-mic)
(exwm-input-set-key (kbd "<XF86AudioPlay>") #'mexwm-music-toggle)
(exwm-input-set-key (kbd "<XF86AudioNext>") #'mexwm-music-next)
(exwm-input-set-key (kbd "<XF86AudioPrev>") #'mexwm-music-prev)
(exwm-input-set-key (kbd "<XF86Launch1>") #'mexwm-scrot)
(exwm-input-set-key (kbd "<XF86ScreenSaver>") #'mexwm-lock)
(exwm-input-set-key (kbd "<XF86LaunchA>") #'mexwm-music-toggle)
(exwm-input-set-key (kbd "<XF86Search>") #'mexwm-music-prev)
(exwm-input-set-key (kbd "<XF86Explorer>") #'mexwm-music-next)

;; These work in hardware so don't need warning about undefined
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'mexwm-no-op)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'mexwm-no-op)
(exwm-input-set-key (kbd "<XF86Sleep>") #'mexwm-no-op)
(exwm-input-set-key (kbd "<XF86WLAN>") #'mexwm-no-op)

;; Keybind to send emacs bound keys to x window while in line mode
(exwm-input-set-key (kbd "C-q") #'exwm-input-send-next-key)

(display-time-mode t)
(display-battery-mode t)
(exwm-systemtray-enable)

(provide 'mexwm)
