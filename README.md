# jal-mode

Emacs major mode for editing [JAL](http://justanotherlanguage.org/) source file

![Screenshot](ss.png)

**WIP**

JAL is a programming language to program Microchip PIC controllers.

This major mode supports jalv2 compiler and jallib.

## Example settings

``` emacs-lisp
(use-package jal-mode
  :config
  (setq tab-width 4)                          ;; as you like
  (setq jal-mode-compiler-path "/SOMEWHERE/jalv2-XXXX")
  (setq jal-mode-lib-path "/SOMEWHERE/jallib-XXXX")

  ;; Enable flymake
  (setq temporary-file-directory "~/tmp")     ;; as you like
  (add-hook 'jal-mode-hook #'flymake-mode)

  ;; or Enable flycheck
  ;; (use-package flycheck-jal
  ;;   :config
  ;;   (add-hook 'jal-mode-hook #'flycheck-mode))
)
```

## Default keybindings
|Function|Key|Function name|
|:--|:--|:--|
|Compile|`C-c` `C-c`|`jal-mode-compile`|
|Next error|`C-c` `C-n`|`flymake-goto-next-error`|
|Previous error|`C-c` `C-p`|`flymake-goto-prev-error`|

## TODO
- [x] font-lock syntax highlighting
- [x] compile .jal
- [x] flymake
- [x] flycheck
- [ ] programming .hex into pic micro controller
- [ ] package
- [ ] indentation (smie?)
- [ ] completion

## Links
- [JAL (Just Another Language)](http://justanotherlanguage.org/)
- [jallib](https://github.com/jallib/jallib)
