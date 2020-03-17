# nvim.el: Emacs client to Neovim

This is a simple client for [Nvim API](https://neovim.io/doc/user/api.html). It
let you control Neovim with Emacs.

``` emacs-lisp
(nvim command "echo 'Hello, World!'")
;; => nil

(nvim set-current-line "你好")
;; => nil

(nvim get-current-line)
;; => "你好"
```

## Requirements

- Emacs 25.1
- https://github.com/xuchunyang/msgpack.el
