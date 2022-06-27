;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Workaround suggested by Henrik to get beancount-mode to work. There was a
;; keybind conflict in the module. This is meant to be temporary until a better
;; solution can be found.
;; (general-auto-unbind-keys)


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matthew R Lincoln"
      user-mail-address "matthew.lincoln@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;;
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Intelligent comment behaviour:
;; Borrowed from https://stackoverflow.com/questions/9688748/emacs-comment-;; uncomment-current-line
;; (defun comment-or-uncomment-region-or-line ()
;;   "Comments or uncomments the region or the current line if there's no active region."
;;   (interactive)
;;   (let (beg end)
;;     (if (region-active-p)
;;         (setq beg (region-beginning) end (region-end))
;;       (setq beg (line-beginning-position) end (line-end-position)))
;;     (comment-or-uncomment-region beg end)
;;     (next-logical-line)))
;; Note that doom-emacs already does this!

;; Intelligent movement to beginning of the current line:
;; From https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; (defun smarter-move-beginning-of-line (arg)
;;   "Move point back to indentation of beginning of line.

;; Move point to the first non-whitespace character on this line.
;; If point is already there, move to the beginning of the line.
;; Effectively toggle between the first non-whitespace character and
;; the beginning of the line.

;; If ARG is not nil or 1, move forward ARG - 1 lines first.  If
;; point reaches the beginning or end of the buffer, stop there."
;;   (interactive "^p")
;;   (setq arg (or arg 1))

;;   ;; Move lines first
;;   (when (/= arg 1)
;;     (let ((line-move-visual nil))
;;       (forward-line (1- arg))))

;;   (let ((orig-point (point)))
;;     (back-to-indentation)
;;     (when (= orig-point (point))
;;       (move-beginning-of-line 1))))
;; Note that doom-emacs already does this!


;; Keybinds:

;; Evaluate lisp expressions with Command-E:
(map! "s-e" #'eval-last-sexp)

;; Some macOS- and Sublime-like settings:
(map! "<s-up>" #'beginning-of-buffer
      "<s-down>" #'end-of-buffer
      "s-x" #'kill-region
      "s-c" #'kill-ring-save
      "s-v" #'yank
      "s-t" #'treemacs
      "s-k" #'kill-whole-line
      "s-d" #'mc/mark-next-like-this-word
      "s-f" #'isearch-forward
      "s-o" #'other-window
      :map isearch-mode-map "s-f" #'isearch-repeat-forward
      :map isearch-mode-map "s-g" #'isearch-repeat-forward)

;; Doom already does these things:
;; (global-set-key (kbd "H-/") 'comment-or-uncomment-region-or-line)
;; (global-set-key (kbd "<H-left>") 'smarter-move-beginning-of-line)
;; (global-set-key (kbd "<H-right>") 'move-end-of-line)

;; (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
;; (global-set-key (kbd "<s-down>") 'end-of-buffer)

(map! :after treemacs
      "s-p" #'treemacs-switch-workspace
      :map treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

(map! :map mc/keymap "<escape>" #'mc/keyboard-quit)

;; Make shift-click expand the selection zone:
(define-key global-map (kbd "<S-down-mouse-1>") #'mouse-save-then-kill)


;; markdown-mode keybinds:
(map! :map markdown-mode-map
      "s-i" #'markdown-insert-italic
      "s-b" #'markdown-insert-bold)

;; ess keybinds:
(map! :after ess
      "s-i" #'ess-indent-exp)

;; org-mode keybinds:
(map! :after org
      :map org-mode-map
      ;; meta-left and -right to have non-org behaviour; org behaviour with ctrl-meta:
      "<C-M-left>" #'org-metaleft
      "<C-M-right>" #'org-metaright
      "<M-left>" #'backward-word
      "<M-right>" #'forward-word
      "<S-M-left>" #'org-shiftcontrolleft
      "<S-M-right>" #'org-shiftcontrolright
      ;; macOS-like formatting:
      :desc "Italic" "s-i" (cmd! (org-emphasize ?\/))
      :desc "Bold" "s-b" (cmd! (org-emphasize ?\*))
      :desc "Underline" "s-u" (cmd! (org-emphasize ?\_))
      ;; insert reference:
      :desc "Insert reference" "s-r" #'citar-insert-citation)

;; refresh candidates cache when local .bib file changes:
(citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))

;; Theme settings:

;; (load-theme 'kaolin-temple t)
;; (kaolin-treemacs-theme)
(setq doom-theme 'doom-oceanic-next
      ;; doom-theme 'doom-nord
      doom-font (font-spec :family "Source Code Pro" :size 12)
      ;; doom-font (font-spec :family "Anonymous Pro" :size 12)
      doom-variable-pitch-font (font-spec :family "Helvetica Neue"))

;; Use Monaco font for tabs:
(after! centaur-tabs
  (centaur-tabs-change-fonts "Monaco" 120)
  ;; (centaur-tabs-change-fonts "Avenir Next Regular" 130)
  (setq centaur-tabs-show-navigation-buttons t
        ;; centaur-tabs-down-tab-text " ▼ "
        centaur-tabs-down-tab-text " ▾ "
        ;; centaur-tabs-backward-tab-text " ◀︎ "
        centaur-tabs-backward-tab-text " < "
        ;; centaur-tabs-forward-tab-text " ▶︎ "
        centaur-tabs-forward-tab-text " > "
        ;; centaur-tabs-show-new-tab-button t
        ;; centaur-tabs-new-tab-text " + "
        x-underline-at-descent-line t
        centaur-tabs-style "chamfer"
        centaur-tabs-height 32
        centaur-tabs-set-bar 'under)
  (centaur-tabs-group-by-projectile-project))

;; Cycle through tabs with ctrl-tab:
(map! :after centaur-tabs
      "<C-tab>" #'centaur-tabs-forward
      "<C-S-tab>" #'centaur-tabs-backward)

(setq +treemacs-use-git-mode 'simple)

;; Do not expand top treemacs project:
(setq treemacs-expand-after-init nil)

;; Add clock and battery status to modeline:
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-format "%a %m/%d %H:%M"
      display-time-default-load-average nil)
(display-time)
;; Only display battery mode when on laptop:
(when (string= (system-name) "gandalf.local")
  (display-battery-mode))


(after! doom-themes
  (setq doom-themes-treemacs-theme "doom-colors")
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config))

;; Prevent underscores from being interpreted as subscripts in tex mode
(setq tex-fontify-script nil)

;; Blink the cursor:
(blink-cursor-mode)

(setq-default cursor-type 'bar
              tab-width 2)

;; Disable exit confirmation:
(setq confirm-kill-emacs nil)

;; Prevent tramp from compressing files: this was causing gzip errors
(setq tramp-copy-size-limit 1000000
      tramp-inline-compress-start-size 1000000)

;; Maximize emacs window on laptop:
(if (or (string= (system-name) "gandalf")
        (string= (system-name) "gandalf.local"))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (progn (add-to-list 'default-frame-alist '(width . 250))
         (add-to-list 'default-frame-alist '(height . 70))
         (add-to-list 'default-frame-alist '(top . 0.5))
         (add-to-list 'default-frame-alist '(left . 0.5))))

(setq ispell-dictionary "en")

;; Do not indent comments in R:
(setq ess-indent-with-fancy-comments nil)

;; Highlight lines that exceed 100 characters:
;; (from https://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/)
;; (after! whitespace
;;   (setq whitespace-line-column 100 ;; limit line length
;;         whitespace-style '(face lines-tail)))

;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(after! highlight-indent-guides
  (setq highlight-indent-guides-responsive t))

;; Use omnifocus-capture to send region to OmniFocus:
(autoload 'send-region-to-omnifocus "omnifocus-capture" "Send region to OmniFocus" t)
;; (global-set-key (kbd "C-c C-o") 'send-region-to-omnifocus)
(map! "M-s-o" #'send-region-to-omnifocus)

;; (map! :after beancount
;;       :map beancount-mode-map
;;       "l" nil)

;; mu4e configuration:
;; NOTE This config assumes that an app-specific password for Gmail is stored in
;; the keychain. This can be done with the following command:
;; security add-internet-password -a matthew.lincoln@gmail.com \
;;   -l smtp.gmail.com -P 587 -r smtp -s smtp.gmail.com -w my-password -U
(set-email-account! "gmail"
  '((user-full-name . "Matthew R Lincoln")
    (user-mail-address . "matthew.lincoln@gmail.com")
    (smtpmail-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-service . 587)
    (smtpmail-smtp-user . "matthew.lincoln@gmail.com")
    (mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail")
    (mu4e-drafts-folder     . "/gmail/[Gmail].Drafts")
    (mu4e-trash-folder      . "/gmail/[Gmail].Trash")
    (mu4e-refile-folder     . "/gmail/[Gmail].All Mail")
    (mu4e-compose-signature . "Matthew R Lincoln MD DPhil FRCPC\nAssociate Research Scientist\nDepartment of Neurology, Yale School of Medicine"))
  t)

;; Yale email server settings obtained from ITS:
;; IMAP incoming outlook.office365.com Port number 993 encryption method SSL
;; SMTP outgoing smtp.office365.com Port number 587 encryption method TLS

;; NOTE This config assumes that a password for Yale is stored in the keychain.
;; This can be done with the following command:
;; security add-internet-password -a matthew.lincoln@yale.edu \
;;   -l smtp.office365.com -P 587 -r smtp -s smtp.office365.com -w my-password -U
(set-email-account! "yale"
  '((user-full-name . "Matthew R Lincoln")
    (user-mail-address . "matthew.lincoln@yale.edu")
    (smtpmail-smtp-server . "smtp.office365.com")
    (smtpmail-smtp-service . 587)
    (smtpmail-smtp-user . "matthew.lincoln@yale.edu")
    (mu4e-sent-folder       . "/yale/Sent Items")
    (mu4e-drafts-folder     . "/yale/Drafts")
    (mu4e-trash-folder      . "/yale/Deleted Items")
    (mu4e-refile-folder     . "/yale/Archive")
    (mu4e-compose-signature . "Matthew R Lincoln MD DPhil FRCPC\nAssociate Research Scientist\nDepartment of Neurology, Yale School of Medicine"))
  t)


;; Additional email configuration:
(after! mu4e
  (setq ;; mu4e-update-interval (* 10 60)
        ;; +mu4e-gmail-accounts '(("matthew.lincoln@gmail.com" . "/gmail"))
        mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask
        mu4e-maildir-shortcuts
        '((:maildir "/gmail/INBOX" :key ?g)
          (:maildir "/yale/INBOX" :key ?y))
        mu4e-alert-interesting-mail-query (concat
                                           "flag:unread"
                                           " AND NOT flag:trashed"
                                           " AND NOT maildir:\"/yale/Junk Email\"")))

(after! mu4e
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-signature (concat "\n\n#+begin_signature\n"
                                  "*Matthew R Lincoln MD DPhil FRCPC*\n"
                                  "Associate Research Scientist\n"
                                  "Department of Neurology, Yale School of Medicine\n"
                                  "#+end_signature")))
(after! mu4e
  (setq org-msg-enforce-css
        (let* ((font-family '(font-family . "\"Helvetica Neue\", Helvetica, Arial, sans-serif"))
               (monospace-font '(font-family . "SFMono-Regular, Monaco, \"Courier New\", monospace"))
               (font-size '(font-size . "11pt"))
               (font `(,font-family ,font-size))
               (line-height '(line-height . "normal"))
               (theme-color "#00205b") ; Pantone 281C
               (bold '(font-weight . "bold"))
               (color `(color . ,theme-color))
               (table `((margin-top . "6px") (margin-bottom . "6px")
                        (border-left . "none") (border-right . "none")
                        (border-top . "2px solid #222222")
                        (border-bottom . "2px solid #222222")
                        ))
               (ftl-number `(,color ,bold (text-align . "left")))
               (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                                   fundamental ini json makefile man org plantuml
                                   python sh xml))
               (inline-src `((background-color . "rgba(27,31,35,.05)")
                             (border-radius . "3px")
                             (padding . ".2em .4em")
                             (font-size . "90%") ,monospace-font
                             (margin . 0)))
               (code-src
                (mapcar (lambda (mode)
                          `(code ,(intern (concat "src src-" (symbol-name mode)))
                                 ,inline-src))
                        inline-modes))
               (base-quote '((padding-left . "5px") (margin-left . "10px")
                             (margin-top . "20px") (margin-bottom . "0")
                             (font-style . "italic") (background . "#f9f9f9")))
               (quote-palette '("#6A8FBF" "#bf8f6a" "#6abf8a" "#906abf"
                                "#6aaebf" "#bf736a" "#bfb66a" "#bf6a94"
                                "#6abf9b" "#bf6a7d" "#acbf6a" "#6a74bf"))
               (quotes
                (mapcar (lambda (x)
                          (let ((c (nth x quote-palette)))
                            `(div ,(intern (format "quote%d" (1+ x)))
                                  (,@base-quote
                                   (color . ,c)
                                   (border-left . ,(concat "3px solid "
                                                           (org-msg-lighten c)))))))
                        (number-sequence 0 (1- (length quote-palette))))))
          `((del nil ((color . "grey") (border-left . "none")
                      (text-decoration . "line-through") (margin-bottom . "0px")
                      (margin-top . "10px") ,line-height))
            (a nil (,color))
            (a reply-header ((color . "black") (text-decoration . "none")))
            (div reply-header ((padding . "3.0pt 0in 0in 0in")
                               (border-top . "solid #e1e1e1 1.0pt")
                               (margin-bottom . "20px")))
            (span underline ((text-decoration . "underline")))
            (li nil (,line-height (margin-bottom . "0px")
                                  (margin-top . "2px")
                                  (max-width . "84ch")))
            (nil org-ul ((list-style-type . "disc")))
            (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                                (margin-top . "0px") (margin-left . "30px")
                                (padding-top . "0px") (padding-left . "5px")))
            (nil signature (,@font (margin-bottom . "20px")))
            (blockquote nil ((padding . "2px 12px") (margin-left . "10px")
                             (margin-top . "10px") (margin-bottom . "0")
                             (border-left . "3px solid #ccc")
                             (font-style . "italic")
                             (background . "#f9f9f9")))
            (p blockquote  ((margin . "0") (padding . "4px 0")))
            ,@quotes
            (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
            ,@code-src
            (nil linenr ((padding-right . "1em")
                         (color . "black")
                         (background-color . "#aaaaaa")))
            (pre nil (,line-height
                      (color . ,(face-foreground 'default))
                      (background-color . ,(face-background 'default))
                      (margin . "4px 0px 8px 0px")
                      (padding . "8px 12px")
                      (width . "max-content")
                      (min-width . "80ch")
                      (border-radius . "5px")
                      (font-weight . "500")
                      ,monospace-font))
            (div org-src-container ((margin-top . "10px")))
            (nil figure-number ,ftl-number)
            (nil table-number)
            (caption nil ((text-align . "left")
                          (background . ,theme-color)
                          (color . "white")
                          ,bold))
            (nil t-above ((caption-side . "top")))
            (nil t-bottom ((caption-side . "bottom")))
            (nil listing-number ,ftl-number)
            (nil figure ,ftl-number)
            (nil org-src-name ,ftl-number)
            (img nil ((vertical-align . "middle")
                      (max-width . "100%")))
            (img latex-fragment-inline ((margin . "0 0.1em")))
            (table nil (,@table ,line-height (border-collapse . "collapse")))
            (th nil ((border . "none") (border-bottom . "1px solid #222222")
                     (background-color . "#EDEDED") (font-weight . "500")
                     (padding . "3px 10px")))
            (td nil (,@table (padding . "1px 10px")
                             (background-color . "#f9f9f9") (border . "none")))
            (td org-left ((text-align . "left")))
            (td org-right ((text-align . "right")))
            (td org-center ((text-align . "center")))
            (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
                      (box-shadow . "inset 0 -1px 0 #d1d5da")
                      (background-color . "#fafbfc") (color . "#444d56")
                      (padding . "3px 5px") (display . "inline-block")))
            (div outline-text-4 ((margin-left . "15px")))
            (div outline-4 ((margin-left . "10px")))
            ;; (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
            ;; (h3 nil ((margin-bottom . "0px")
            ;;          ,color (font-size . "14pt")))
            ;; (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
            ;;          ,color (font-size . "18pt")))
            ;; (h1 nil ((margin-top . "20px") (margin-bottom . "0px")
            ;;          ,color (font-size . "24pt")))
            ;; (p nil ((text-decoration . "none") ,line-height
            ;;         (margin-top . "10px") (margin-bottom . "0px")
            ;;         ,font-size (max-width . "90ch")))
            ;; (b nil ((font-weight . "bold")))
            (h1 nil ((margin-top . "20px") (margin-bottom . "0px")
                     ,@font ,color (font-weight . "bold")))
            (h2 nil ((margin-top . "20px") (margin-bottom . "0px")
                     ,@font ,color (font-weight . "bold") (font-style . "italic")))
            (h3 nil ((margin-bottom . "0px")
                     ,@font ,color (font-weight . "bold")))
            (h4 nil ((margin-bottom . "0px")
                     ,@font ,color (font-style . "italic")))
            (p nil ((text-decoration . "none") ,line-height
                    (margin-top . "10px") (margin-bottom . "0px")
                    ,font-size (max-width . "90ch")))
            (b nil ((font-weight . "bold")))
            ;; (b signature ((font-weight . "bold") ,color))
            (div nil (,@font ,line-height))))))

;; Bind email:
(map! "s-m" #'=mu4e)

;; Configure latex (from https://tecosaur.github.io/emacs-config/config.html):
(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

;; Create option to use LaTeX report class without parts:
(after! ox-latex
  (add-to-list 'org-latex-classes
             '("report-noparts"
               "\\documentclass{report}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Highlight lines longer than 100 characters in programming modes:
(setq whitespace-line-column 100)
(setq whitespace-style '(face tabs tab-mark lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Configure vterm
(after! vterm
  (setq vterm-shell "bash"))
