;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (cons (cons "\\.tex$" ’yatex-mode) auto-mode-alist))
(autoload ’yatex-mode "yatex" "Yet Another LaTeX mode" t)
(defvar YaTeX-dvi2-command-ext-alist
  ’(("xdvi" . ".dvi")
     ("ghostview\\|gv" . ".ps")
     ("acroread" . ".pdf")))
(setq dvi2-command "acroread")
(setq dviprint-command-format "dvipdfmx ‘basename %s pdf‘dvi")
;;; 色付け
(setq YaTeX-use-font-lock t)
;;; monthly-reportのディレクトリにマッチすればiso-2022-jpで開く
(add-hook ’yatex-mode-hook
           ’(lambda ()
              (progn
                (if (string-match "/monthly-report/" default-directory)
                    (progn (set-buffer-file-coding-system ’iso-2022-jp)
                           (set-buffer-modified-p nil))))))

;; git.el をロードする
(load-library "/usr/share/doc/git-core/contrib/emacs/git.el")
