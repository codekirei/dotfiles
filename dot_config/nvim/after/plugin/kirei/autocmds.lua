--[[
Note -- to see all events available
:h autocmd-events
--]]

--[[
The way vim instantiates autocmds is tricky, such that it's easy to add the
same cmd multiple times.

This line prevents that by throwing everything in a group with `clear = true`
(the default, but also set explicitly for clarity).

With this approach, every time these cmds are set up, the previous instances
are cleared properly.
--]]
local group = vim.api.nvim_create_augroup('DefaultGroup', { clear = true })

vim.api.nvim_create_autocmd('insertenter', {
  group = group,
  desc = 'Hide trail listchar during insert mode.',
  command = 'set listchars-=trail:·',
})

vim.api.nvim_create_autocmd('insertleave', {
  group = group,
  desc = 'Re-add trail listchar outside insert mode.',
  command = 'set listchars+=trail:·',
})

-- Buf-specific LSP keymaps
-- :h lsp-buf
-- :h nvim-lspconfig
vim.api.nvim_create_autocmd('LspAttach', {
  group = group,
  callback = function(ev)
    local opts = { buffer = ev.buf }

    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)

    opts.desc = 'Rename'
    vim.keymap.set('n', '<space>rr', vim.lsp.buf.rename, opts)
  end,
})

vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'InsertLeave' }, {
  group = group,
  callback = function()
    --[[
    Sometimes in a monorepo different packages have their own lint configs. This code finds the
    closest dir that matches root_pattern and passes that to try_lint as the cwd,
    which gives us a good chance at finding a working config.

    TODO: expand this to actual config files and other file types.
    --]]
    local util = require('lspconfig.util')
    local cwd = util.root_pattern('package.json', '.tool-versions')(vim.fn.expand('%:p')) or vim.loop.cwd()
    require('lint').try_lint(nil, { cwd = cwd })
  end,
})
