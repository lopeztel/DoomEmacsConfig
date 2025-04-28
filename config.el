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
 ; (setq doom-font (font-spec :family "FiraCode NFM" :size 18 :weight 'medium)
 ;       doom-variable-pitch-font (font-spec :family "SauceCodePro NF" :size 20 :weight 'regular)
 ;       doom-big-font (font-spec :family "FiraCode NF" :size 32))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Emacs transparency (compatible with wayland)
;; Taken from https://www.emacswiki.org/emacs/TransparentEmacs
;; Linux Wayland
;; (set-frame-parameter nil 'alpha-background 85)
;; (add-to-list 'default-frame-alist '(alpha-background . 85))
;; Windows & Linux X11
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
 (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
 (add-to-list 'default-frame-alist '(alpha . (85 . 85)))

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

(setq org-directory "~/org/")

;; Set org-agenda-files
(setq org-agenda-files
      '("~/org/Todos.org"
        "~/org/work/Todos.org"
        "~/org/work/meeting-notes.org"
        "~/org/Agenda.org"
        "~/org/Birthdays.org"
        "~/org/Habits.org"
        "~/org/Holidays.org"))

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

;; CALENDAR settings
;; From https://emacs.stackexchange.com/questions/10965/easiest-way-to-customize-holidays-that-appear-in-org-agenda/13236#13236
(after! calendar
  (setq calendar-week-start-day 1
        calendar-location-name "Oslo"
        holiday-bahai-holidays nil         ; Disable Bahá'í holidays
        holiday-hebrew-holidays nil        ; Disable Hebrew holidays
        holiday-islamic-holidays nil       ; Disable Islamic holidays
        holiday-solar-holidays nil         ; Disable solar holidays
  )
)


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
          ("howto" . ?H)
          ("knitting" . ?k)
          ("maintenance" . ?m)
          ("meeting" . ?M)
          ("note" . ?n)
          ("noexport" .?N)
          ("personal" . ?p)
          ("recipe" . ?r)
          ("TOC" . ?T)
          ("work" . ?w)))

  ;; Org capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/work/Todos.org" "Captured todos")
           "* TODO %?\n  %i\n  %a")
          ("p" "Personal Todo" entry (file+headline "~/org/Todos.org" "Captured todos")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("m" "Meeting Notes" entry (file+olp+datetree "~/org/work/meeting_notes.org")
           "* %^{Meeting Title} %^g\nEntered on: %U\n\n%?" :empty-lines 1)
          ("w" "Work Note" plain
           ;; Create new file in ~/org/work/ with dynamic name
           (file
            (lambda ()
              (let* ((dir (expand-file-name "~/org/work/"))
                     (fname (read-string "File name (without .org): "))
                     (full-path (expand-file-name (concat fname ".org") dir)))
                ;; Create directory if missing
                (unless (file-exists-p dir)
                  (make-directory dir :parents))
                full-path)))
           ;; Insert template content
           (file "~/org/Templates/work-note-template.org"))
        )
  )

;; (setq org-structure-template-alist
;;       '(("n" "#+TITLE: ?\n#+AUTHOR: ?\n#+DATE: ?\n#+OPTIONS: toc:nil\n#+STARTUP: content\n\n* Introduction\n\n** \n\n* Main Content\n\n** \n\n* Conclusion\n\n** \n")))

  ;; ORG-PUBLISH
  (setq org-publish-use-timestamps-flag t) ;;not generate only when files change

  (defun my/generate-img-projects ()
    "Generate publishing entries and names for all *-img directories under ~/org/work."
    (let* ((base-dir "~/org/work/")
           (publish-base "~/work-dashboard/")
           (img-dirs (directory-files base-dir t "^[^.]\\{1,\\}-img$"))
           (projects '())
           (names '()))
      (dolist (dir img-dirs)
        (let* ((name (file-name-nondirectory (directory-file-name dir)))
               (project-name (concat "org-" name))
               (target-dir (expand-file-name name publish-base)))
          (push project-name names)
          (push
           `(,project-name
             :base-directory ,dir
             :base-extension "jpg\\|png\\|gif\\|pdf\\|svg"
             :publishing-directory ,target-dir
             :recursive t
             :publishing-function org-publish-attachment)
           projects)))
      ;; Return a list of two things: the new projects, and their names
      (list projects names)))

  (let* ((img-data (my/generate-img-projects))
         (img-projects (nth 0 img-data))
         (img-names (nth 1 img-data))
         (static-projects
          '(("org-work-files"
             :base-directory "~/org/work/"
             :base-extension "org"
             :publishing-directory "~/work-dashboard/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :auto-preamble t)
            ("org-presentation-files"
             :base-directory "~/org/work/Presentations/"
             :base-extension "org"
             :publishing-directory "~/work-dashboard/Presentations/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :auto-preamble t)
            ("org-work-assets"
             :base-directory "~/org/work/media/"
             :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff\\|pptx"
             :publishing-directory "~/work-dashboard/media/"
             :recursive t
             :publishing-function org-publish-attachment)
            ("org-presentation-assets"
             :base-directory "~/org/work/Presentations/media/"
             :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
             :publishing-directory "~/work-dashboard/Presentations/media/"
             :recursive t
             :publishing-function org-publish-attachment)))
         ;; Combine component names
         (dashboard-components
          (append '("org-work-files"
                    "org-work-assets"
                    "org-presentation-files"
                    "org-presentation-assets")
                  img-names)))

  ;; Final set of projects
  (setq org-publish-project-alist
        (append
         static-projects
         img-projects
         (list `("work-dashboard" :components ,dashboard-components)))))

  ;; ORG-PUBLISH NOTE: This is the old easy way, left for reference
  ;; (setq org-publish-project-alist
  ;;       '(("org-work-files"
  ;;          :base-directory "~/org/work/"
  ;;          :base-extension "org"
  ;;          :publishing-directory "~/work-dashboard/"
  ;;          :recursive t
  ;;          :publishing-function org-html-publish-to-html
  ;;          :headline-levels 4
  ;;          :auto-preamble t
  ;;          )
  ;;         ("org-presentation-files"
  ;;          :base-directory "~/org/work/Presentations/"
  ;;          :base-extension "org"
  ;;          :publishing-directory "~/work-dashboard/Presentations/"
  ;;          :recursive t
  ;;          :publishing-function org-html-publish-to-html
  ;;          :headline-levels 4
  ;;          :auto-preamble t
  ;;          )
  ;;         ("org-work-assets"
  ;;          :base-directory "~/org/work/media/"
  ;;          :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff\\|pptx"
  ;;          :publishing-directory "~/work-dashboard/media/"
  ;;          :recursive t
  ;;          :publishing-function org-publish-attachment
  ;;          )
  ;;         ("org-presentation-assets"
  ;;          :base-directory "~/org/work/Presentations/media/"
  ;;          :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
  ;;          :publishing-directory "~/work-dashboard/Presentations/media/"
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

;;NOTE: see https://github.com/fniessen/org-html-themes/blob/26666aa5c3325dfd11b4c7ec83de53fba260b482/examples/org-mode-syntax-example.org#L702 for syntax on these
(use-package! org-tempo
  :after org
  :config
  (setq tempo-interactive t)
  (tempo-define-template
   "Note block" '("#+begin_note\n">(p "Note content: " note) "\n#+end_note">)
   "<note")
  (tempo-define-template
   "Warning block" '("#+begin_warning\n">(p "Warning content: " warning) "\n#+end_warning">)
   "<w")
  ;; (tempo-define-template
  ;;  "Info block" '("#+begin_info\n">(p "Info content: " info) "\n#+end_info">)
  ;;  "<info")
  (tempo-define-template
   "Tip block" '("#+begin_tip\n">(p "Tip content: " tip) "\n#+end_tip">)
   "<t")
  )


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
;; Some elements taken from: https://www.aidanscannell.com/post/org-mode-resume/
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
  (setq org-preview-latex-default-process 'dvisvgm)
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

;; ORG-ROAM
;; some elements taken from: https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
(use-package! org-roam
  :init
  (setq org-roam-directory (file-truename "~/org/secondBrain")
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           (file "~/org/Templates/roam-main-template.org")
           :target (file "main/${slug}.org")
           :immediate-finish t
           :unnarrowed t)
          ("h" "How-To" plain
           (file "~/org/Templates/roam-howto-template.org")
           :target (file "howto/${slug}.org")
           :immediate-finish t
           :unnarrowed t)
         ))
  )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(require 'org-roam-export)

;; MERMAID
(require 'ob-mermaid)
;; (setq ob-mermaid-cli-path "/usr/bin/mmdc")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)))

(require 'mermaid-mode)
;; (setq mermaid-mmdc-location "/usr/bin/mmdc")

;; ORG-DOWNLOAD
;; NOTE: See https://emacs.stackexchange.com/questions/71100/pasting-images-from-clipboard-into-orgmode
(after! org-download
      (setq org-download-method 'directory)
      (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
      ;; (setq org-download-image-html-width 1000)
      ;; (setq org-download-image-latex-width 1000)
      ;; (setq org-download-image-org-width 1000)
      (setq org-download-image-attr-list
            '("#+ATTR_ORG: :width 1000 :align center"
              "#+ATTR_HTML: :with 1000 :align center"
              "#+ATTR_LATEX: :options {0.9\\textwidth}"))
      (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
      (setq org-download-link-format-function #'org-download-link-format-function-default))

;; HARPER
;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "harper-ls" "--stdio")
;;     :activation-fn (lsp-activate-on "markdown-mode" "text-mode" "org-mode")
;;     :server-id 'harper-ls)))

;; NOTE: Alternative is to use eglot, init.el must have lsp +eglot
(after! 'eglot
  (add-to-list 'eglot-server-programs
               '(markdown-mode text-mode org-mode . ("harper-ls" "--stdio"))))

;; KEYMAPPINGS
(map! :leader
      :desc "Toggle Treemacs"
      "e" #'treemacs)
