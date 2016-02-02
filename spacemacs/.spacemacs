;;; .spacemacs --- Settings for the Spacemacs Emacs configuration package
;; -*- mode: emacs-lisp -*-

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     markdown
     (gnus :variables
           gnus-secondary-select-methods
           '((nnimap "mail.margeo.nrlssc.navy.mil")
             ;; (nntp "gmane" (nntp-address "news.gmane.org"))
             ;; (nntp "news.gwene.org")
             ;; (nnimap "imap.gmail.com"
             ;;         (nnimap-server-port "imaps")
             ;;         (nnimap-stream ssl))
             ;; (nnimap  "imap.kolabnow.com")
             )
           gnus-posting-styles
           '(("nrlssc.navy.mil" (address "adam.seyfarth@nrlssc.navy.mil")))
           gnus-read-active-file 'some
           gnus-fetch-old-headers nil)
     auto-completion
     emacs-lisp
     (org :variables
          org-export-allow-bind-keywords t
          org-agenda-files '("~/private/plan.org")
          org-pomodoro-length 24
          org-pomodoro-audio-player "mplayer"
          org-pomodoro-finished-sound
          "/home/aseyfarth/.emacs.d/elpa/org-pomodoro-20150803.530/resources/bell_multiple.wav"
          org-pomodoro-start-sound-p t
          org-pomodoro-ticking-sound-states '(:pomodoro)
          org-pomodoro-ticking-sound-p t)
     (shell :variables
            shell-default-height 48
            shell-default-position 'bottom)
     syntax-checking
     version-control
     python
     clojure
     haskell
     ipython-notebook
     latex
     racket
     scheme
     rust
     shell-scripts
     finance
     gtags
     ;; slime
     ;; irc
     git
     semantic
     c-c++
     csharp
     perspectives
     yaml
     deft
     typography
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages
   '(
     monky
     base16-theme
     smtpmail-multi
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   (concatenate 'list
                (if (version< emacs-version "24.4")
                    '(magit)
                  '())
                '(ox-gfm))
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(projects recents)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(base16-ashes-dark base16-ashes-light solarized-dark solarized-light spacemacs-dark spacemacs-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 19
                               :weight semi-bold
                               :width normal
                               :powerline-scale 1.7)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

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

(defun dotspacemacs/config ()
  "Configuration function.

   This function is called at the very end of Spacemacs initialization after
   layers configuration."
  (spacemacs/declare-prefix "\\" "User commands")
  (spacemacs/set-leader-keys "\\r" 'goto-random-line)
  (setq-default typo-language 'English)
  (add-hook 'gnus-group-mode-hook
            ;; list all the subscribed groups even they contain zero un-read messages
            (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups )))
  (defvar smtp-accounts
    '((ssl "adam.seyfarth@nrlssc.navy.mil" "mail.margeo.nrlssc.navy.mil"
           587 "MARGEO\aseyfarth" nil)))
  (add-to-list 'auto-mode-alist '("SConfig\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
  (setq sentence-end-double-space t)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "m e p" 'eval-print-last-sexp)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "<M-return>" 'eval-print-last-sexp)
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
        smtpmail-auth-credentials "~/.authinfo") )

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
 '(ansi-color-names-vector
   ["#1C2023" "#C7AE95" "#95C7AE" "#AEC795" "#AE95C7" "#C795AE" "#AE95C7" "#C7CCD1"])
 '(ansi-term-color-vector
   [unspecified "#1C2023" "#C7AE95" "#95C7AE" "#AEC795" "#AE95C7" "#C795AE" "#AE95C7" "#C7CCD1"] t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("4ab89cc4c58408bb799084a4d9be77fe0700b2f1b75809eae330129b4b921b6f" "7545d3bb77926908aadbd525dcb70256558ba05d7c478db6386bfb37fb6c9120" "73ae6088787f6f72ef52f19698b25bc6f0edf47b9e677bf0a85e3a1e8a7a3b17" "f0e69da2cf73c7f153fc09ed3e0ba6e1fd670fec09b8a6a8ed7b4f9efea3b501" "d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
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
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
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
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
