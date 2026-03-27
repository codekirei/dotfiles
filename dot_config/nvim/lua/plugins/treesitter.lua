local parsers = {
  'c',
  'css',
  'csv',
  'diff',
  'dockerfile',
  'elixir',
  'erlang',
  'go',
  'gomod',
  'gpg',
  'graphql',
  'html',
  'javascript',
  'jsdoc',
  'json',
  'just',
  'lua',
  'make',
  'markdown',
  'markdown_inline',
  'python',
  'query',
  'rust',
  'scss',
  'sql',
  'terraform',
  'typescript',
  'vim',
  'vimdoc',
  'yaml',
}

local function config()
  require('nvim-treesitter').install(parsers)

  vim.api.nvim_create_autocmd('FileType', {
    pattern = parsers,
    callback = function()
      vim.treesitter.start()
    end,
  })
end

return {
  'nvim-treesitter/nvim-treesitter',
  lazy = false,
  build = ':TSUpdate',
  config = config,
}
