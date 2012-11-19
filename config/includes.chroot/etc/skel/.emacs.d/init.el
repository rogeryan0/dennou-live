;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;; init.el
;;
;; Copyright(C) 2012 Youhei SASAKI All rights reserved.
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Keywords:
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Code:
;; -----------------------------------------------------------
;; おまじない
(eval-when-compile (require 'cl))
;; 極力 UTF-8 を使用. 過去の資源(特に TeX 関連)には
;; euc-japan のモノも多いので注意すること.
;;
(set-language-environment "Japanese")
(set-language-environment-coding-systems "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; ------------------------------------
;; 起動時はホームディレクトリから
(cd "~/")
;; \C-h -> BS にする.
(global-set-key (kbd "C-h") 'backward-delete-char)
;; [HOME] と [END] でバッファーの先頭/最後へ移動
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
;; モードラインにカーソルのある行番号を表示
(line-number-mode 0)
;; モードラインにカーソルのある桁番号を表示
(column-number-mode 0)
;; 左側に行番号を表示
(when (locate-library "linum")
  (setq linum-format "%4d "))
;; カーソルのある行を強調表示しない
(global-hl-line-mode 0)
;; リージョンに色づけ.
(setq transient-mark-mode t)
;; 対応する括弧を色づけする
(show-paren-mode t)
;; ツールバーを表示しない.
(tool-bar-mode 0)
;; スクロールバーは使用しない.
(set-scroll-bar-mode nil)
;; メニューバーを表示しない.
(menu-bar-mode -1)
;; bell-mode 使用しない
(setq ring-bell-function 'ignore)
;; startup を表示しない
(setq inhibit-startup-screen t)
;; タイトルにバッファ名を表示
(setq frame-title-format "%b")
;; \C-x f で画像を表示しない(主に terminal で起動するから)
;; (setq auto-image-file-mode nil)
;; ファイル名とともにディレクトリも表示
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-min-dir-content 1)
(setq-default save-place nil)
;; .save-* を作らない
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
;; #* を作成しない
(setq auto-save-default nil)
;; *.~ を作成しない
(setq make-backup-files nil)
;; tab 幅4, tab での indent の停止
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; 分割ウィンドウの大きさを M + up/down で変更
;; (global-set-key (kbd "M-<up>")
;;                 '(lambda (arg) (interactive "p")(shrink-window arg)))
;; (global-set-key (kbd "M-<down>")
;;                 '(lambda (arg) (interactive "p")(shrink-window (- arg))))
;; 空になったファイルを尋ねず自動削除
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
;; *scratch* を殺さない設定(再生成する)
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg
        (progn
          (setq arg 0)
          (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら
          ;; *scratch* バッファを新しく作る.
          (function
           (lambda ()
             (unless (member "*scratch*" (my-buffer-name-list))
               (my-make-scratch 1)))))
;;行末の無駄な空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; 日本語入力 -> Anthy
(eval-when-compile (require 'anthy))
(load-library "anthy")
(setq default-input-method "japanese-anthy")
(setq anthy-accept-timeout 1)
;;; font
;; (when window-system
;;   (create-fontset-from-ascii-font "Inconsolata:size=12" nil "myfont")
;;   (set-fontset-font "fontset-myfont"
;;                     'unicode
;;                     (font-spec :family "IPAGothic" :size 12)
;;                     nil 'append)
;;   (dolist (charset '(
;;                      japanese-jisx0208
;;                      japanese-jisx0208-1978
;;                      japanese-jisx0212
;;                      japanese-jisx0213-1
;;                      japanese-jisx0213-2
;;                      japanese-jisx0213-a
;;                      japanese-jisx0213.2004-1
;;                      katakana-jisx0201
;;                      ))
;;     (set-fontset-font "fontset-myfont"
;;                       charset
;;                       (font-spec :family "IPAGothic" :size 12)
;;                       nil 'prepend))
;;   (setq face-font-rescale-alist
;;         '(("-cdac$" . 1.3)))
;;   (custom-set-faces
;;    '(variable-pitch ((t (:family "Monospace")))))
;;   (add-to-list 'default-frame-alist
;;                '(font . "fontset-myfont")))





