local opts = {
  draw = {
    delay = 0,
  },
  symbol = '›',
}

local function config(_, opts)
  opts.draw.animation = require('mini.indentscope').gen_animation.quadratic({
    easing = 'in-out',
    duration = 60,
    unit = 'total',
  })

  require('mini.indentscope').setup(opts)
end

return {
  'echasnovski/mini.indentscope',
  lazy = false,
  version = false,
  opts = opts,
  config = config,
}
