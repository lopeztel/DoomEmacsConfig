;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept.
;; Linux
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 18 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "SauceCodeProNerdFont" :size 20)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 25))
;; Windows
;; (setq doom-font (font-spec :family "FiraCode NFM" :size 18 :weight 'medium)
;;       doom-variable-pitch-font (font-spec :family "FiraCode NF" :size 18)
;;       doom-big-font (font-spec :family "FiraCode NF" :size 32))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Emacs transparency (compatible with wayland)
;; Taken from https://www.emacswiki.org/emacs/TransparentEmacs
;; Linux Wayland
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))
;; Windows & Linux X11
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;;Hide ugly empty line tildes
(setq-default indicate-empty-lines nil)

;; Disable line numbers for some modes
(dolist (mode '(
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Linux
(setq org-directory "~/org/")
;; Windows
;; (setq org-directory "~/Documents/org/")

;; Set org-agenda-files
;; Linux
(setq org-agenda-files
      '("~/org/Todos.org"
        "~/org/work/Todos.org"
        "~/org/Agenda.org"
        "~/org/Birthdays.org"
        "~/org/Habits.org"
        "~/org/Holidays.org"))
;; Windows
;; (setq org-agenda-files
;;       '("~/Documents/org/Todos.org"
;;         "~/Documents/org/work/Todos.org"
;;         "~/Documents/org/Agenda.org"
;;         "~/Documents/org/Birthdays.org"
;;         "~/Documents/org/Habits.org"
;;         "~/Documents/org/Holidays.org"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ORGMODE CONFIG

(defun efs/org-mode-setup ()
  (display-line-numbers-mode 0)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package! org
  :hook (org-mode . efs/org-mode-setup)
  :config
  ;; Org config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (setq org-src-preserve-indentation t)
  (setq org-startup-indented t)
  (setq org-highlight-links '(bracket angle plain tag date footnote))
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-tags-column -77)
  (setq org-hide-leading-stars t)

  ;; Org Agenda config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)

  ;; Org TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
          (sequence "OPEN(o)" "IN PROGRESS(i)" "REOPENED(r)" "|""RESOLVED(s@/!)" "CLOSED(c@/!)" "WON'T DO(w@/!)")))

  ;; Org tag list
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("blog" . ?b)
          ("computing" . ?c)
          ("exercise" . ?e)
          ("health" . ?h)
          ("knitting" . ?k)
          ("maintenance" . ?m)
          ("note" . ?n)
          ("noexport" .?N)
          ("personal" . ?p)
          ("recipe" . ?r)
          ("TOC" . ?T)
          ("work" . ?w)))

  ;; Org capture templates
  ;; Linux
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/work/Todos.org" "Captured todos")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  ;; Windows
  ;; (setq org-capture-templates
  ;;       '(("t" "Todo" entry (file+headline "~/Documents/org/work/Todos.org" "Captured todos")
  ;;          "* TODO %?\n  %i\n  %a")
  ;;         ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
  ;;          "* %?\nEntered on %U\n  %i\n  %a")))

  ;; Org Publishing
  (setq org-publish-use-timestamps-flag nil) ;;don't generate only when files change
  ;; Linux
  (setq org-publish-project-alist
        '(("org-work-files"
           :base-directory "~/org/work/"
           :base-extension "org"
           :publishing-directory "~/work-dashboard/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamble t
           )
          ("org-presentation-files"
           :base-directory "~/org/work/Presentations/"
           :base-extension "org"
           :publishing-directory "~/work-dashboard/Presentations/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamble t
           )
          ("org-work-assets"
           :base-directory "~/org/work/media/"
           :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
           :publishing-directory "~/work-dashboard/media/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("org-presentation-assets"
           :base-directory "~/org/work/Presentations/media/"
           :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
           :publishing-directory "~/work-dashboard/Presentations/media/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("work-dashboard" :components("org-work-files" "org-work-assets" "org-presentation-files" "org-presentation-assets"))))
  ;; Windows
  ;; (setq org-publish-project-alist
  ;;       '(("org-work-files"
  ;;          :base-directory "~/Documents/org/work/"
  ;;          :base-extension "org"
  ;;          :publishing-directory "~/Documents/work-dashboard/"
  ;;          :recursive t
  ;;          :publishing-function org-html-publish-to-html
  ;;          :headline-levels 4
  ;;          :auto-preamble t
  ;;          )
  ;;         ("org-presentation-files"
  ;;          :base-directory "~/Documents/org/work/Presentations/"
  ;;          :base-extension "org"
  ;;          :publishing-directory "~/Documents/work-dashboard/Presentations/"
  ;;          :recursive t
  ;;          :publishing-function org-html-publish-to-html
  ;;          :headline-levels 4
  ;;          :auto-preamble t
  ;;          )
  ;;         ("org-work-assets"
  ;;          :base-directory "~/Documents/org/work/media/"
  ;;          :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
  ;;          :publishing-directory "~/Documents/work-dashboard/media/"
  ;;          :recursive t
  ;;          :publishing-function org-publish-attachment
  ;;          )
  ;;         ("org-presentation-assets"
  ;;          :base-directory "~/Documents/org/work/Presentations/media/"
  ;;          :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
  ;;          :publishing-directory "~/Documents/work-dashboard/Presentations/media/"
  ;;          :recursive t
  ;;          :publishing-function org-publish-attachment
  ;;          )
  ;;         ("work-dashboard" :components("org-work-files" "org-work-assets" "org-presentation-files" "org-presentation-assets"))))
)

;; ORG-HABIT
(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 25
        org-habit-show-all-today t
        org-habit-graph-column 40
        org-habit-show-habits t))

;; VISUAL-FILL
;;Center org buffers

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


;;HL-TODO

(after! hl-todo
(setq hl-todo-keyword-faces
      '(("TODO" . "#EBCB8B")
        ("NOTE" . "#8FBCBB")
        ("COMMENT" . ,(face-foreground 'font-lock-comment-face))
        ("HACK" . "#D08770")
        ("FIXME" . "#BF616A")
        ("REVIEW" . "#5E81AC")
        ("DEPRECATED" . "#B48EAD")
        ("BUG" . "#B48EAD")
        ("XXX" . "#B48EAD"))))

;; LATEX
(after! ox-latex
  (setq org-latex-src-block-backend 'minted)
  ;; (setq org-latex-minted-options
  ;;       '(("linenos" "true")
  ;;         ("bgcolor" "bg")
  ;;         ("breaklines" "true")
  ;;         ("frame" "lines")
  ;;         ("framesep" "2mm")
  ;;         ("baselinestretch" "1.2")
  ;;         ("fontsize" "\\footnotesize")))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; (setq org-latex-with-hyperref nil) ;; was used before, now deprecated
  (setq org-latex-hyperref-template nil) ;; stop adding hypersetup{author..} to latex export
  ;; (setq org-latex-prefer-user-labels t)

  ;; delete unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
          (quote ("lof"
                  "lot"
                  "tex~"
                  "aux"
                  "idx"
                  "log"
                  "out"
                  "toc"
                  "nav"
                  "snm"
                  "vrb"
                  "dvi"
                  "fdb_latexmk"
                  "blg"
                  "brf"
                  "fls"
                  "entoc"
                  "ps"
                  "spl"
                  "bbl"
                  "xmpi"
                  "run.xml"
                  "bcf"
                  "acn"
                  "acr"
                  "alg"
                  "glg"
                  "gls"
                  "ist")))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nill)))

;; ignore headlines tagged with :noexport:
(use-package! org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; EVIL

(use-package! evil
  :config
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; KEYMAPPINGS
(map! :leader
      :desc "Toggle Treemacs"
      "e" #'treemacs)
