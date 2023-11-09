Debugging Tips
--------------

Get current value for `var`
```
:set var?
```

Print plugin configs
```
:=vim.inspect(require('lazy.core.config').plugins)
```

Print current config of netrw
```
:NetrwSettings
```

Print `print()` statements from code.
```
:messages
```

Show all highlight groups
```
:so $VIMRUNTIME/syntax/hitest.vim
:Telescope highlights
```

Show all keymaps in Telescope
```
:Telescope keymaps
```

Debug language server
```
:LspInfo
:=print(vim.inspect(vim.lsp.get_active_clients()))
```

Show full rtp
```
:=print(vim.inspect(vim.api.nvim_list_runtime_paths()))
```

Useful Links
------------

https://neovim.io/doc/user/lua-guide.html

Regarding Fonts
---------------

This config assumes use of a nerd font:
https://www.nerdfonts.com/

My favorite nerd font is ProggyClean.
