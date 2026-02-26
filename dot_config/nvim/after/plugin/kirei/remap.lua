local k = vim.keymap.set
local tele = require('telescope.builtin')

-- move vertically based on rendered lines, not real lines
k({ 'v', 'n' }, 'j', 'gj')
k({ 'v', 'n' }, 'k', 'gk')

-- move visual selection vertically
k('v', 'J', ":m '>+1<CR>gv=gv")
k('v', 'K', ":m '<-2<CR>gv=gv")

-- don't move cursor when joining lines
-- overwrites 'z' mark
k('n', 'J', 'mzJ`z')

-- don't highlight after *
k('n', '*', '*:noh<CR>', { silent = true })

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

-- Buffers ---------------------------------------------------------------------
-- modified from kshenoy and shmup on stackoverflow
vim.api.nvim_create_user_command('BufOnly', '%bd|e#|bd#|\'"', {})
k('n', '<leader>bo', ':BufOnly<cr>', { desc = 'Close other bufs', silent = true })
k('n', '<leader>bb', tele.buffers, { desc = 'Browse bufs' })

-- random chars ----------------------------------------------------------------
local function random_char()
  local code = math.random(65, 65 + 50)
  -- there are 6 non-alpha chars between upper + lower codes
  if code > 90 then
    code = code + 6
  end
  return string.char(code)
end

local function insert_random_char()
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local char = random_char()
  vim.api.nvim_buf_set_text(0, row - 1, col, row - 1, col, { char })
end

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

-- plugins ---------------------------------------------------------------------

require('which-key').register({
  ['<leader>'] = {
    b = { name = '+buffer' },
    e = { name = '+external' },
    f = { name = '+find' },
    g = { name = '+generate' },
    r = { name = '+refactor' },
    t = { name = '+toggle' },
    x = { name = '+expand' },
  },
})

k('n', '<leader>l', require('lint').try_lint, { desc = 'Lint this file' })
k('n', '<leader>sd', require('neogen').generate, { desc = 'Generate docstring' })
k('n', '<leader>tf', require('twilight').toggle, { desc = 'Toggle focus' })
k('n', '<leader>tm', require('mini.map').toggle, { desc = 'Toggle minimap' })
k('n', '<leader>tb', ':BlameToggle<CR>', { desc = 'Toggle git blame' })
k('n', '<leader>tl', ':LspRestart<CR>', { desc = 'Restart LSP' })
k('n', '<leader>tz', ':ZenMode<CR>', { desc = 'Toggle zen mode' })
k('n', '<leader>fd', ':DevdocsOpenCurrent<CR>', { desc = 'Find dev docs' })
k('n', '<leader>gr', insert_random_char, { desc = 'Generate random alpha char' })

local entry_display = require('telescope.pickers.entry_display')
local make_entry = require('telescope.make_entry')

-- telescope customization -----------------------------------------------------

-- TODO: include missing icon. try running tele.live_grep directly to see what I mean.
-- TODO: make width dynamic instead of static %
local function kirei_live_grep_entry_maker(opts)
  opts = opts or {}
  local path_col_width = opts.path_col_width or math.floor(vim.o.columns * 0.46)

  local displayer = entry_display.create({
    separator = ' || ',
    items = {
      { width = path_col_width }, -- fixed width for "path:line:col"
      { remaining = true }, -- match text takes remaining space
    },
  })

  local base_maker = make_entry.gen_from_vimgrep(opts)

  return function(line)
    local entry = base_maker(line)

    -- build the left column: "path:ln:col"
    local path_col = string.format('%s:%s:%s', entry.filename or '', entry.lnum or '', entry.col or '')

    entry.display = function(e)
      -- build the match column (strip preceding whitespace)
      local match_col = e.text:gsub('^%s+', '')

      -- highlight groups are optional
      return displayer({
        { path_col, 'TelescopeResultsIdentifier' },
        { match_col, 'TelescopeResultsLineNr' },
      })
    end

    return entry
  end
end

-- convenience wrapper to call live_grep with aligned display
local function kirei_live_grep()
  tele.live_grep({
    entry_maker = kirei_live_grep_entry_maker(),
  })
end

-- TODO: update telescope to be able to use this
local function kirei_lsp_references_entry_maker()
  local base_maker = make_entry.gen_from_lsp({})

  -- choose path column width as fraction of window (adjust as you like)
  local win_width = vim.api.nvim_win_get_width(0)
  local path_col_width = math.floor(win_width * 0.45)

  local displayer = entry_display.create({
    separator = ' || ',
    items = {
      { width = path_col_width }, -- path:line:col
      { remaining = true }, -- the matched line
    },
  })

  return function(line)
    local entry = base_maker(line)
    local path_col = string.format('%s:%s:%s', entry.filename or '', entry.lnum or '', entry.col or '')

    entry.display = function(e)
      local text = e.text or e.value or ''
      return displayer({
        { path_col, 'TelescopeResultsFile' },
        { text, 'TelescopeResultsLineNr' },
      })
    end

    return entry
  end
end

local function kirei_lsp_references()
  tele.lsp_references({
    entry_maker = kirei_lsp_references_entry_maker(),
  })
end

k('n', '<leader>ff', tele.find_files, { desc = 'Find files' })
-- k('n', '<leader>fg', tele.live_grep, { desc = 'Find string (grep)' })
k('n', '<leader>fg', kirei_live_grep, { desc = 'Find string (grep)' })
k('n', '<leader>fs', tele.lsp_document_symbols, { desc = 'Find symbol (buffer)' })
k('n', '<leader>fS', tele.lsp_workspace_symbols, { desc = 'Find symbol (workspace)' })
k('n', 'gd', tele.lsp_definitions, { desc = 'Go to definition' })
k('n', 'gD', tele.lsp_type_definitions, { desc = 'Go to typedef' })
k('n', 'gi', tele.lsp_implementations, { desc = 'Go to implmentation' })
k('n', 'gI', tele.lsp_references, { desc = 'Go to reference' })
-- k('n', 'gI', kirei_lsp_references, { desc = 'Go to reference' })

local function eslint_fix_file()
  local file = vim.api.nvim_buf_get_name(0)
  if file == '' then
    return
  end
  vim.fn.jobstart({ 'eslint_d', '--fix', file }, {
    on_exit = function(_, code, _)
      vim.schedule(function()
        if code == 0 then
          vim.cmd('edit!') -- reload file so buffer picks up fixes
          print('eslint_d --fix applied')
        else
          print('eslint_d --fix failed (exit ' .. tostring(code) .. ')')
        end
      end)
    end,
  })
end
k('n', '<leader>rfe', eslint_fix_file, { desc = 'eslint_d --fix' })
