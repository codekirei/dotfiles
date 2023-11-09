local opts = {
  transparent = true,
}

local function config(_, opts)
  require('nightfox').setup({options = opts})
  vim.cmd([[colorscheme carbonfox]])
end

return {
  'EdenEast/nightfox.nvim',
  lazy = false,
  priority = 1000,
  opts = opts,
  config = config,
}
