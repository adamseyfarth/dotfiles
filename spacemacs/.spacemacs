;;; .spacemacs --- Settings for the Spacemacs Emacs configuration package
;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ;; Document related
     markdown
     latex
     yaml
     typography
     deft
     (org
      :variables
      org-agenda-files (quote ("~/private/work.org"))
      org-capture-templates
      '(("e" "Normal entry" entry
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
"))
      org-export-allow-bind-keywords t
      org-pomodoro-length 24
      org-pomodoro-audio-player "mplayer"
      org-pomodoro-start-sound-p t
      org-pomodoro-ticking-sound-states '(:pomodoro)
      org-pomodoro-ticking-sound-p t
      )

     ;; Tooling
     (auto-completion
      :variables
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'complete
      )
     syntax-checking
     version-control
     (git
      :variables
      magit-diff-use-overlays nil
      )
     ;; gtags
     semantic
     (shell
      :variables
      shell-default-height 48
      shell-default-position 'bottom
      shell-default-shell 'multi-term
      shell-default-term-shell "zsh"
      multi-term-program "zsh"
      )

     ;; Languages
     shell-scripts
     (python
      :variables
      python-test-runner 'pytest
      python-shell-interpreter
      (concat (getenv "HOME") "/anaconda3/bin/ipython")
      python-shell-interpreter-args "--simple-prompt -i"
      )
     ipython-notebook
     octave
     haskell
     idris
     emacs-lisp
     racket
     scheme
     clojure
     rust
     (c-c++
      :variables
      c-c++-default-mode-for-headers 'c++-mode
      c-c++-enable-clang-support t)
     csharp
     html
     (javascript
      :variables
      js2-strict-trailing-comma-warning nil
      js2-include-node-externs t)
     react

     ;; Other
     emoji
     games
     (gnus
      :variables
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
     erc
     )

   dotspacemacs-additional-packages
   '(
     monky
     smtpmail-multi
     dash dash-functional
     gnus-desktop-notify
     bbdb
     highlight-indent-guides
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   (if (version< emacs-version "24.4")
       '(magit)
     '())
   dotspacemacs-download-packages 'used
   ))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((projects . 12) (recents . 12))
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   dotspacemacs-themes
   '(
     base16-ashes
     )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font
   '("Source Code Pro"
     :size 17
     :weight semi-bold
     :width normal
     :powerline-scale 1.0
     )
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"  ; aka M-RET
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-large-file-size 32
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 6
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'any
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun gnus-list-all-subscribed ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(defun make-evil-cursors-in-region ()
  "Using evil-mc, make a cursor at the beginning of each line in region"
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
    js2-function-param
    ))

(defvar faces-to-italic
  '(
    font-lock-string-face
    font-lock-constant-face
    link
    web-mode-html-tag-face
    ))

(defvar keep-highlighting-modes
  '(
    react-mode
    js2-mode
    web-mode
    css-mode
    rust-mode
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
  ;; Initializing "rxvt" first and _then_ "xterm" seems
  ;; to make the colors work... although I have no idea why.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(defun fight-stickyfunc ()
  "Do whatever it takes to disable semantic-stickyfunc-mode"
  (with-eval-after-load 'semantic
    (setq-default semantic-default-submodes
                  (remove 'global-semantic-stickyfunc-mode
                          semantic-default-submodes))
    (spacemacs/toggle-semantic-stickyfunc-globally-off)))

(defun insert-timestamp (resolution separator)
  ;; (interactive)
  (let* ((format-strings-space
          #s(hash-table data (year
                              "%Y"
                              month "%Y-%m"
                              day "%Y-%m-%d"
                              hour "%Y-%m-%d %H%z"
                              minute "%Y-%m-%d %H:%M%z"
                              second "%Y-%m%d %H:%M:%S%z")))
         (format-strings-T
          #s(hash-table data (year
                              "%Y"
                              month "%Y-%m"
                              day "%Y-%m-%d"
                              hour "%Y-%m-%dT%H%z"
                              minute "%Y-%m-%dT%H:%M%z"
                              second "%Y-%m%dT%H:%M:%S%z")))
         (format-strings (if (eq separator 'space)
                             format-strings-space
                           format-strings-T)))
    (insert (format-time-string (gethash resolution format-strings)))))

(defun config-keybindings ()
  (spacemacs/declare-prefix "\\" "User commands")
  (spacemacs/set-leader-keys
    "\\ r" 'goto-random-line
    "\\ TAB" 'yas-expand
    "\\ g" 'gnus-summary-insert-new-articles
    "\\ s f" 'unhighlight-remappings
    "\\ s n" 'clear-remapping-alist
    "\\ c" 'make-evil-cursors-in-region
    "\\ j" 'semantic-ia-fast-jump
    "\\ t y"   (lambda () (interactive) (insert-timestamp 'year   'space))
    "\\ t m o" (lambda () (interactive) (insert-timestamp 'month  'space))
    "\\ t d"   (lambda () (interactive) (insert-timestamp 'day    'space))
    "\\ t h"   (lambda () (interactive) (insert-timestamp 'hour   'space))
    "\\ t m i" (lambda () (interactive) (insert-timestamp 'minute 'space))
    "\\ t s"   (lambda () (interactive) (insert-timestamp 'second 'space))
    "\\ T h"   (lambda () (interactive) (insert-timestamp 'hour   'T))
    "\\ T m i" (lambda () (interactive) (insert-timestamp 'minute 'T))
    "\\ T s"   (lambda () (interactive) (insert-timestamp 'second 'T))
    )
  (add-hook 'gnus-group-mode-hook
            ;; list all subscribed groups, even with zero unread messages
            (lambda () (local-set-key "o" 'gnus-list-all-subscribed)))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "e p"
    'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "<M-return>"
    'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'lisp-interaction-mode "e p"
    'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'lisp-interaction-mode "<M-return>"
    'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'term-mode "j" 'term-line-mode)
  (evil-leader/set-key-for-mode 'term-mode "k" 'term-char-mode)
  (evil-leader/set-key-for-mode 'c++-mode "=" 'clang-format-buffer)
  (setq-default
   expand-region-contract-fast-key "V"
   expand-region-reset-fast-key "r"
   ))

(defun config-visuals ()
  (spacemacs/toggle-highlight-current-line-globally-off)
  (add-hook 'semantic-mode-hook 'fight-stickyfunc)
  (add-hook 'after-make-frame-functions 'on-frame-open)
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda () (unless (memq major-mode keep-highlighting-modes)
  ;;                        (unhighlight-remappings))))
  (add-hook 'prog-mode-hook
            (lambda () (unless (memq major-mode '(web-mode react-mode))
                         (highlight-indent-guides-mode))))
  (setq-default highlight-indent-guides-method 'character)
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun config-filetypes ()
  (add-to-list 'auto-mode-alist '("SConfig\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
  (add-to-list 'auto-mode-alist '(".eslintrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '(".babelrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.F\\'" . f90-mode))
  (evil-set-initial-state 'term-mode 'emacs)
  )

(defun config-email ()
  (require 'smtpmail)
  (defvar smtp-accounts
    '((ssl "adam.seyfarth@nrlssc.navy.mil" "mail.margeo.nrlssc.navy.mil"
           587 "MARGEO\aseyfarth" nil)))
  (with-eval-after-load 'gnus
    (setq-default
     gnus-thread-sort-functions '((not gnus-thread-sort-by-most-recent-date))
     gnus-summary-line-format "%U%R%z %(%&user-date;  %-16,16f  %B %s%)\n"
     ))
  (setq
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   mail-from-style nil
   user-full-name "Adam Seyfarth"
   user-mail-address "adam.seyfarth@nrlssc.navy.mil"
   smtpmail-debug-info t
   smtpmail-debug-verb t
   starttls-use-gnutls t
   starttls-gnutls-program "gnutls-cli"
   starttls-extra-arguments nil
   smtpmail-smtp-server "mail.margeo.nrlssc.navy.mil"
   smtpmail-smtp-service "587"
   smtpmail-auth-credentials "~/.authinfo"
   )
  (require 'gnus-desktop-notify)
  (setq
   gnus-desktop-notify-function 'gnus-desktop-notify-send
   )
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail)
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq
   bbdb-mua-update-interactive-p '(query . create)
   bbdb-message-all-addresses t
   bbdb-mua-pop-up nil
   ))

(defun config-layouts ()
  (spacemacs|define-custom-layout "@Gnus"
    :binding "g"
    :body
    (gnus))
  (spacemacs|define-custom-layout "@IRC"
    :binding "i"
    :body
    (erc))
  (spacemacs|define-custom-layout "@Term"
    :binding "t"
    :body
    (multi-term)
    (spacemacs/toggle-maximize-buffer))
  )

(defun config-indentation ()
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
  (setq
   indent-tabs-mode nil
   tab-width 8
   c-basic-offset 2
   js-indent-level 2
   js2-basic-offset 2
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   ))

(defun config-misc ()
  (global-evil-mc-mode 1)
  (setq-default
   typo-language 'English
   sentence-end-double-space t
   ring-bell-function 'ignore
   ))

(defun dotspacemacs/user-config ()
  "Configuration function.

   This function is called at the very end of Spacemacs initialization after
   layers configuration."

  (config-keybindings)
  (config-visuals)
  (config-filetypes)
  (config-email)
  (config-layouts)
  (config-indentation)
  (config-misc))

;; Sometimes this has an unneeded 'unspecified at the front...
(defun remove-unspecified ()
  (setq ansi-term-color-vector
        (let ((lvec (append ansi-term-color-vector nil)))
          (vconcat (remove 'unspecified lvec)))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8b584a30417351e60bff667fd6f902c31c8ff53ad7b85e54fcadb17d65e7e9ab" "2159a1f9ea13fb1236b684e8e09d4c40b2f09fff345f7a93d0dacc5f8f9deb27" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "240fea1bddbd9b6445860b8cfd323c03c58c92cb4339a3bc65cd9b3c63be9a4a" "4ab89cc4c58408bb799084a4d9be77fe0700b2f1b75809eae330129b4b921b6f" "7545d3bb77926908aadbd525dcb70256558ba05d7c478db6386bfb37fb6c9120" "73ae6088787f6f72ef52f19698b25bc6f0edf47b9e677bf0a85e3a1e8a7a3b17" "f0e69da2cf73c7f153fc09ed3e0ba6e1fd670fec09b8a6a8ed7b4f9efea3b501" "d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(fci-rule-color "#073642" t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (iedit git-commit rust-mode f anaconda-mode simple-httpd auctex csharp-mode web-mode racket-mode racer persp-mode org-plus-contrib intero hindent geiser evil-unimpaired evil-matchit dumb-jump diff-hl cider smartparens evil haskell-mode git-gutter company helm helm-core markdown-mode auto-complete flycheck projectile magit with-editor hydra js2-mode yapfify yaml-mode xterm-color ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree typo typit toml-mode toc-org tagedit stickyfunc-enhance srefactor spacemacs-theme spaceline smtpmail-multi smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters queue quelpa pyvenv pytest pyenv-mode py-isort popwin pkg-info pip-requirements pcre2el paradox pacmacs origami orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file omnisharp neotree multi-term move-text monky mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide idris-mode ido-vertical-mode hy-mode hungry-delete htmlize hlint-refactor hl-todo highlight-parentheses highlight-numbers highlight-indentation highlight-indent-guides help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets goto-chg google-translate golden-ratio gnus-desktop-notify gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator fancy-battery faceup eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus emmet-mode elisp-slime-nav ein disaster deft define-word cython-mode company-web company-tern company-statistics company-shell company-ghci company-ghc company-emoji company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu cargo bbdb base16-theme auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell 2048-game)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
     (org-babel-tangle-use-relative-file-links)
     (org-src-preserve-indentation . t))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sp-highlight-pair-overlay nil)
 '(sp-highlight-wrap-overlay nil)
 '(sp-highlight-wrap-tag-overlay nil)
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
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
