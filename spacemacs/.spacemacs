;;; .spacemacs --- Settings for the Spacemacs Emacs configuration package
;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     markdown
     org
     ;; (org :variables
     ;;      org-export-allow-bind-keywords t
     ;;      org-agenda-files '("~/private/work.org")
     ;;      org-pomodoro-length 24
     ;;      org-pomodoro-audio-player "mplayer"
     ;;      org-pomodoro-finished-sound
     ;;      "/home/aseyfarth/.emacs.d/elpa/org-pomodoro-20150803.530/resources/bell_multiple.wav"
     ;;      org-pomodoro-start-sound-p t
     ;;      org-pomodoro-ticking-sound-states '(:pomodoro)
     ;;      org-pomodoro-ticking-sound-p t
     ;;      )
     latex
     (gnus :variables
           gnus-secondary-select-methods
           '((nnimap "mail.margeo.nrlssc.navy.mil")
             (nntp "gmane" (nntp-address "news.gmane.org"))
             (nntp "news.gwene.org")
             ;; (nnimap "imap.gmail.com"
             ;;         (nnimap-server-port "imaps")
             ;;         (nnimap-stream ssl))
             ;; (nnimap  "imap.kolabnow.com")
             )
           gnus-posting-styles
           '(("nrlssc.navy.mil" (address "adam.seyfarth@nrlssc.navy.mil")))
           gnus-read-active-file 'some
           gnus-fetch-old-headers nil
           message-citation-line-function
           'message-insert-formatted-citation-line
           message-citation-line-format "[%Y-%m-%d %H:%M%z] %f:"
           )
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      )
     emacs-lisp
     (shell :variables
            shell-default-height 48
            shell-default-position 'bottom
            shell-default-shell 'multi-term
            shell-default-term-shell "zsh"
            )
     syntax-checking
     version-control
     (python :variables python-test-runner 'pytest)
     clojure
     emoji
     games
     haskell
     ipython-notebook
     racket
     scheme
     rust
     octave
     shell-scripts
     gtags
     git
     semantic
     c-c++
     csharp
     yaml
     deft
     typography
     agda
     idris
     javascript
     react
     )
   dotspacemacs-additional-packages
   '(
     monky
     base16-theme
     smtpmail-multi
     dash dash-functional
     gnus-desktop-notify
     bbdb
     web-mode
     )
   dotspacemacs-excluded-packages
   (if (version< emacs-version "24.4")
       '(magit)
     '())
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(projects recents)
   dotspacemacs-themes '(base16-ashes-dark
                         base16-ashes-light
                         solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 19
                               :weight semi-bold
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"  ;; aka M-RET
   dotspacemacs-command-key ":"
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'any
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   ))

(defun gnus-list-all-subscribed ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(defun make-evil-cursors-in-region ()
  (interactive)
  (when (region-active-p)
    (let ((begin (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (goto-char end)
      (backward-char)  ;; `end' is one past the last highlighted char
      (beginning-of-line)
      (evil-mc-pause-cursors)
      (while (> (point) begin)
        (evil-mc-make-cursor-here)
        (forward-line -1))
      (evil-mc-resume-cursors))))

(defun goto-random-line ()
  (interactive)
  (if (region-active-p)
      (goto-random-line-region)
    (goto-random-line-buffer)))

(defun goto-random-line-region ()
  (let* ((begin (region-beginning))
         (end (region-end))
         (lines (count-lines begin end))
         (line-begin (save-excursion
                       (goto-char begin)
                       (beginning-of-line)
                       (1+ (count-lines (point-min) (point)))))
         (winner (random lines)))
    (message "Going to line [%d +] %d out of %d"
             line-begin winner lines)
    (deactivate-mark)
    (goto-char begin)
    (forward-line winner)))

(defun goto-random-line-buffer ()
  (let* ((lines (count-lines (point-min) (point-max)))
         (winner (random lines)))
    (message "Going to line %d out of %d" (1+ winner) lines)
    (goto-char (point-min))
    (forward-line winner)))

(defvar faces-to-unhighlight
  '(
    font-lock-keyword-face
    font-lock-function-name-face
    font-lock-builtin-face
    font-lock-variable-name-face
    font-lock-type-face
    font-lock-preprocessor-face
    org-document-info
    org-document-info-keyword
    ))

(defvar faces-to-italic
  '(
    font-lock-string-face
    font-lock-constant-face
    link
    ))

(defun unhighlight-remappings ()
  "Turn off most syntax highlighting for current buffer (a la this guy:
https://www.robertmelton.com/2016/02/24/syntax-highlighting-off/)"
  (interactive)
  (dolist (face faces-to-unhighlight)
    (face-remap-add-relative face 'default))
  (dolist (face faces-to-italic)
    (face-remap-add-relative face 'italic 'default)))

(defun clear-remapping-alist ()
  "Clear the remapping list.  Meant to undo effects of unhighlight-remappings."
  (interactive)
  (setq face-remapping-alist nil))

(defun terminal-init-gnome ()
  "Terminal initialization function for gnome-terminal."

  ;; This is a dirty hack that I accidentally stumbled across:
  ;;  initializing "rxvt" first and _then_ "xterm" seems
  ;;  to make the colors work... although I have no idea why.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(defun fight-stickyfunc ()
  (with-eval-after-load 'semantic
    (setq-default semantic-default-submodes
                  (remove 'global-semantic-stickyfunc-mode
                          semantic-default-submodes))
    (spacemacs/toggle-semantic-stickyfunc-globally-off)))

(defun dotspacemacs/user-config ()
  "Configuration function.

   This function is called at the very end of Spacemacs initialization after
   layers configuration."
  (spacemacs/declare-prefix "\\" "User commands")
  (spacemacs/set-leader-keys "\\ r" 'goto-random-line)
  (spacemacs/set-leader-keys "\\ TAB" 'yas-expand)
  (spacemacs/set-leader-keys "\\ g" 'gnus-summary-insert-new-articles)
  (spacemacs/set-leader-keys "\\ s f" 'unhighlight-remappings)
  (spacemacs/set-leader-keys "\\ s n" 'clear-remapping-alist)
  (spacemacs/set-leader-keys "\\ c" 'make-evil-cursors-in-region)
  (spacemacs/set-leader-keys "\\ j" 'semantic-ia-fast-jump)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (add-hook 'semantic-mode-hook 'fight-stickyfunc)
  (global-evil-mc-mode 1)
  (setq-default typo-language 'English
                indent-tabs-mode nil
                tab-width 8
                c-basic-offset 4)
  (add-hook 'after-make-frame-functions 'on-frame-open)
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg" (selected-frame)))
  (add-hook 'prog-mode-hook 'unhighlight-remappings)
  (add-hook 'gnus-group-mode-hook
            ;; list all subscribed groups, even with zero unread messages
            (lambda () (local-set-key "o" 'gnus-list-all-subscribed)))
  (defvar smtp-accounts
    '((ssl "adam.seyfarth@nrlssc.navy.mil" "mail.margeo.nrlssc.navy.mil"
           587 "MARGEO\aseyfarth" nil)))
  (add-to-list 'auto-mode-alist '("SConfig\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
  (add-to-list 'auto-mode-alist '(".eslintrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.F\\'" . f90-mode))
  (setq sentence-end-double-space t)
  (setq dotspacemacs-auto-resume-layouts t)
  (with-eval-after-load 'gnus
    (setq-default gnus-thread-sort-functions
                  '((not gnus-thread-sort-by-most-recent-date)))
    (setq-default  gnus-summary-line-format "%U%R%z %(%&user-date;  %-16,16f  %B %s%)\n"))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "e p" 'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "<M-return>" 'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'term-mode "j" 'term-line-mode)
  (evil-leader/set-key-for-mode 'term-mode "k" 'term-char-mode)
  (evil-set-initial-state 'term-mode 'emacs)
  (require 'smtpmail)
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        mail-from-style nil
        user-full-name "Adam Seyfarth"
        user-mail-address "adam.seyfarth@nrlssc.navy.mil"
        ;; message-signature-file "~/.signature"
        smtpmail-debug-info t
        smtpmail-debug-verb t)
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server "mail.margeo.nrlssc.navy.mil"
        smtpmail-smtp-service "587"
        smtpmail-auth-credentials "~/.authinfo")
  (spacemacs|define-custom-layout "@Gnus"
    :binding "g"
    :body
    (gnus))
  (spacemacs|define-custom-layout "@Term"
    :binding "t"
    :body
    (multi-term)
    (spacemacs/toggle-maximize-buffer))
  (require 'gnus-desktop-notify)
  (setq gnus-desktop-notify-function 'gnus-desktop-notify-exec
        gnus-desktop-notify-exec-program "notify-send -i ~/dotfiles/emacs.png")
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail)
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-message-all-addresses t)
  )

;; Sometimes this has an unneeded 'unspecified at the front...
(defun remove-unspecified ()
  (setq ansi-term-color-vector
        (let ((lvec (append ansi-term-color-vector nil)))
          (vconcat (-remove (lambda (elt) (eq elt 'unspecified)) lvec)))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(bbdb-mua-pop-up nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8b584a30417351e60bff667fd6f902c31c8ff53ad7b85e54fcadb17d65e7e9ab" "2159a1f9ea13fb1236b684e8e09d4c40b2f09fff345f7a93d0dacc5f8f9deb27" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "240fea1bddbd9b6445860b8cfd323c03c58c92cb4339a3bc65cd9b3c63be9a4a" "4ab89cc4c58408bb799084a4d9be77fe0700b2f1b75809eae330129b4b921b6f" "7545d3bb77926908aadbd525dcb70256558ba05d7c478db6386bfb37fb6c9120" "73ae6088787f6f72ef52f19698b25bc6f0edf47b9e677bf0a85e3a1e8a7a3b17" "f0e69da2cf73c7f153fc09ed3e0ba6e1fd670fec09b8a6a8ed7b4f9efea3b501" "d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#073642")
 '(gnus-thread-sort-functions (quote ((not gnus-thread-sort-by-most-recent-date))))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(js2-include-node-externs t)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-diff-use-overlays nil)
 '(multi-term-program "zsh")
 '(org-agenda-files (quote ("~/private/work.org")))
 '(org-capture-templates
   (quote
    (("e" "Normal entry" entry
      (file+headline
       (ort/todo-file)
       "Entry"))
     ("c" "Org Repo Checklist Item" checkitem
      (file+headline
       (ort/todo-file)
       "Checklist"))
     ("t" "Org Repo Todo" entry
      (file+headline
       (ort/todo-file)
       "Todos")
      "* TODO  %?			%T
 %i
 Link: %l
"))))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-shell-interpreter "ipython")
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
     (org-babel-tangle-use-relative-file-links)
     (org-src-preserve-indentation . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(smtpmail-smtp-server "imap.gmail.com")
 '(smtpmail-smtp-service 25)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(js2-function-call ((t nil)))
 '(js2-function-param ((t nil)))
 '(js2-instance-member ((t nil)))
 '(js2-private-function-call ((t nil)))
 '(js2-private-member ((t nil))))
