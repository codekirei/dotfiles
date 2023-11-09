local k = vim.keymap.set
local tele = require('telescope.builtin')

-- move visual selection vertically
k('v', 'J', ":m '>+1<CR>gv=gv")
k('v', 'K', ":m '<-2<CR>gv=gv")

-- don't move cursor when joining lines
-- overwrites 'z' mark
k('n', 'J', 'mzJ`z')

-- stay centered when scrolling or searching
k('n', '<C-d>', '<C-d>zz')
k('n', '<C-u>', '<C-u>zz')
k('n', 'n', 'nzzzv')
k('n', 'N', 'Nzzzv')

-- overwrite without losing current register
k('x', '<leader>p', '"_dP', { desc = 'Paste but keep register' })

-- get wc of selection or doc | [E]xternal [W]ordcount
k({ 'v', 'n' }, '<leader>ew', '::w !wc -w<CR>', { desc = 'Get word count' })

-- explorer / netrw ------------------------------------------------------------

-- modified from Doomguy3003 example
local function ex_to_current_file()
  local this_file = vim.fn.expand('%:t')
  vim.cmd.Ex()

  -- jump to line via search
  -- use keepp to not override prev search
  vim.cmd('keepp /^' .. this_file .. '$')
  vim.cmd('noh')
end

k('n', '-', ex_to_current_file)

-- window movement -------------------------------------------------------------

local function get_smart_win(key)
  local vim_to_tmux = {
    h = 'L',
    j = 'D',
    k = 'U',
    l = 'R',
  }
  return function()
    local start_win = vim.fn.win_getid()
    vim.cmd.wincmd(key)
    local new_win = vim.fn.win_getid()
    if new_win == start_win then
      vim.cmd('silent !tmux select-pane -' .. vim_to_tmux[key])
    end
  end
end

k('n', '<C-h>', get_smart_win('h'))
k('n', '<C-j>', get_smart_win('j'))
k('n', '<C-k>', get_smart_win('k'))
k('n', '<C-l>', get_smart_win('l'))

-- diagnostics -----------------------------------------------------------------
-- :h diagnostic-api
local diag = vim.diagnostic

k('n', '<leader>xd', diag.open_float, { desc = 'Expand diagnostic details' })
k('n', '[d', diag.goto_prev, { desc = 'Go to prev diagnostic line' })
k('n', ']d', diag.goto_next, { desc = 'Go to next diagnostic line' })

-- lsp gotos via telescope -----------------------------------------------------
k('n', 'gd', tele.lsp_definitions, { desc = 'Go to definition' })
k('n', 'gD', tele.lsp_type_definitions, { desc = 'Go to typedef' })
k('n', 'gi', tele.lsp_implementations, { desc = 'Go to implmentation' })
k('n', 'gI', tele.lsp_references, { desc = 'Go to reference' })

-- plugins ---------------------------------------------------------------------

require('which-key').register({
  ['<leader>'] = {
    e = { name = '+external' },
    f = { name = '+find' },
    r = { name = '+refactor' },
    t = { name = '+toggle' },
    x = { name = '+expand' },
  },
})

k('n', '<leader>l', require('lint').try_lint, { desc = 'Lint this file' })
k('n', '<leader>sd', require('neogen').generate, { desc = 'Generate docstring' })
k('n', '<leader>tf', require('twilight').toggle, { desc = 'Toggle focus' })
k('n', '<leader>tm', require('mini.map').toggle, { desc = 'Toggle minimap' })
k('n', '<leader>fd', ':DevdocsOpenCurrent<CR>', { desc = 'Find dev docs' })

k('n', '<leader>ff', tele.find_files, { desc = 'Find files' })
k('n', '<leader>fg', tele.live_grep, { desc = 'Find string (grep)' })
k('n', '<leader>fb', tele.buffers, { desc = 'Find buffer' })
k('n', '<leader>fs', tele.lsp_document_symbols, { desc = 'Find symbol (buffer)' })
k('n', '<leader>fS', tele.lsp_workspace_symbols, { desc = 'Find symbol (workspace)' })
