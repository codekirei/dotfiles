return {
  'folke/zen-mode.nvim',
  opts = {
    window = {
      width = 80,
      options = {
        number = false,
        relativenumber = false,
      },
    },
    plugins = {
      options = {
        wrap = true,
        linebreak = true,
      },
      twilight = { enabled = false },
      tmux = { enabled = true },
    },
  },
}
