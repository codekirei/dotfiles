local function config(_, opts)
  require('telescope').setup(opts)
  require('telescope').load_extension('cmdline')
end

return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.3',
  config = config,
  dependencies = { 'nvim-lua/plenary.nvim' },
}
