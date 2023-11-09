local function config(_, opts)
  require('mini.align').setup(opts)
end

return {
  'echasnovski/mini.align',
  version = false,
  config = config,
}
