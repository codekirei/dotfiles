local function opts()
  return {
    options = {
      theme = 'nightfly',
      disabled_filetypes = {
        statusline = { 'dashboard' },
      },
      component_separators = {
        left = '┃',
        right = '┃',
      },
      section_separators = {
        left = '',
        right = '',
      },
    },
    sections = {
      lualine_x = {'encoding'},
      lualine_y = {'filetype'},
    },
  }
end

return {
  'nvim-lualine/lualine.nvim',
  event = 'VeryLazy',
  opts = opts,
}
