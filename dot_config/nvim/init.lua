-- map leader to space
-- (this needs to be before plugins per lazy docs)
vim.g.mapleader = ' '

-- set up lazy.nvim for plugins
-- note that lazy must be installed to nvim's data dir via git
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
vim.opt.rtp:prepend(lazypath)

local lazyopts = {}
require('lazy').setup('plugins', lazyopts)

require('kirei')
