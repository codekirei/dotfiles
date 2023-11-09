local function config()
  local cmp = require('cmp')
  local luasnip = require('luasnip')
  local lspkind = require('lspkind')

  --[[
  Some key defaults:
  <C-n> = open cmp menu || next
  <C-p> = prev
  <C-e> = close cmp menu
  --]]
  local mappings = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-u>'] = cmp.mapping.scroll_docs(4),
    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
  }

  cmp.setup({
    completion = {
      completeopt = 'menu,menuone,preview,noselect',
    },
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert(mappings),
    formatting = {
      format = lspkind.cmp_format({
        mode = 'symbol_text',
        maxwidth = 60,
        ellipsis_char = '…',
      }),
    },
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
      { name = 'nvim_lua' },
      { name = 'buffer' },
      { name = 'async_path' },
    }),
  })

  cmp.setup.filetype('text', {
    sources = cmp.config.sources({
      { name = 'async_path' },
    }),
  })

  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      {
        name = 'buffer',
        option = { keyword_pattern = [[\k\+]] },
      },
      { name = 'buffer-lines' },
    },
  })

  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'async_path' },
      { name = 'cmdline' },
    }),
  })

  local cmp_autopairs = require('nvim-autopairs.completion.cmp')
  cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
end

return {
  'hrsh7th/nvim-cmp',
  config = config,
  dependencies = {
    'FelipeLema/cmp-async-path',
    'L3MON4D3/LuaSnip',
    'amarakon/nvim-cmp-buffer-lines',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lua',
    'onsails/lspkind.nvim',
    'saadparwaiz1/cmp_luasnip',
    'windwp/nvim-autopairs',
  },
}
