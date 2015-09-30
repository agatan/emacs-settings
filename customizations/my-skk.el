(require 'skk-vars)

(global-set-key "\C-x\C-j" 'skk-mode)
(autoload 'skk-mode "skk" nil t)
(autoload 'skk-auto-fill-mode "skk" nil t)
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)

;; Enterキーを押したときには確定する
(setq skk-egg-like-newline t)
;; 句読点に. , を使う
(setq skk-kutouten-type 'en)
;; 辞書登録のとき， 余計な送り仮名を送らないようにする
(setq skk-check-okurigana-on-touroku 'auto)
;; look コマンド を使った検索をする
(setq skk-use-look t)
;; 半角数字
(setq skk-number-style nil)
