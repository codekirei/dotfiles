local function config()
  local action_state = require('telescope.actions.state')

  local open_in_bg_buf = function(prompt_bufnr)
    local selection = action_state.get_selected_entry()
    vim.cmd('badd ' .. vim.fn.fnameescape(selection.path))
    -- intentionally leaving prompt open
    -- actions.close(prompt_bufnr)
  end

  require('telescope').setup({
    defaults = {
      layout_strategy = 'vertical',
      layout_config = {
        width = 0.99,
        height = 0.99,
        prompt_position = 'bottom',
      },
      preview_cutoff = 0,
      mappings = {
        i = {
          ['<C-b>'] = open_in_bg_buf,
        },
      },
    },
  })
  -- require('telescope').load_extension('cmdline')
end

return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.3',
  config = config,
  dependencies = { 'nvim-lua/plenary.nvim' },
}
