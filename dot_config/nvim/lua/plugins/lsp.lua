--[[
Mason package registry:
https://mason-registry.dev/registry/list

LSP server names <-> Mason package names
https://github.com/williamboman/mason-lspconfig.nvim/blob/main/doc/server-mapping.md
--]]
local function config()
  local lspconfig = require('lspconfig')
  local _capabilities = require('cmp_nvim_lsp').default_capabilities()

  require('mason').setup({
    ui = {
      icons = {
        package_installed = '✓',
        package_pending = '➜',
        package_uninstalled = '✗',
      },
    },
  })

  require('mason-lspconfig').setup({
    -- Only language servers here, not other tools -- see tool-installer for those.
    -- Use LSP server names, not Mason package names.
    ensure_installed = {
      'ansiblels',
      'astro',
      'clangd', -- c, c++
      'cssls',
      'docker_compose_language_service',
      'dockerls',
      'gopls', -- go
      'html',
      'jsonls',
      'lua_ls',
      'marksman', -- markdown
      'sqlls',
      'rust_analyzer',
      'tsserver', -- typescript
      'yamlls',
    },
    handlers = {
      -- special default function; must be first entry in handlers table
      function(server_name)
        lspconfig[server_name].setup({
          capabilities = _capabilities,
        })
      end,
    },
  })

  require('mason-tool-installer').setup({
    -- Use Mason package names.
    ensure_installed = {
      'clang-format',
      'gofumpt',
      'goimports',
      'prettierd',
      'stylua',
      'sql-formatter',
      'sqlfluff',
    },
  })

  -- Supplementary linting. Many LSPs do linting, but sometimes a dedicated
  -- linter is better.
  require('lint').linters_by_ft = {
    sql = { 'sqlfluff' },
  }

  -- Auto-formatting. Generally preferred over LSP formatting.
  -- Note that formatter names here must match the Conform formatter list to
  -- autoload configs, and the names don't always match the Mason package names.
  -- e.g. Mason: sql-formatter <-> Conform: sql_formatter
  -- :h conform-formatters
  require('conform').setup({
    formatters_by_ft = {
      ['c++'] = { 'clang-format' },
      c = { 'clang-format' },
      go = { 'goimports', 'gofumpt' },
      javascript = { 'prettierd' },
      json = { 'prettierd' },
      lua = { 'stylua' },
      rust = { 'rustfmt' }, -- see comment at EOF
      sql = { 'sql_formatter' },
      typescript = { 'prettierd' },
      typescriptreact = { 'prettierd' },
    },
    formatters = {
      sql_formatter = {
        args = { '--config', '.sql-formatter.json' },
      },
    },
    format_after_save = {
      lsp_fallback = true,
    },
  })
end

return {
  'neovim/nvim-lspconfig',
  config = config,
  dependencies = {
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'WhoIsSethDaniel/mason-tool-installer.nvim',
    'jay-babu/mason-nvim-dap.nvim',
    'mfussenegger/nvim-lint',
    'stevearc/conform.nvim',
    'hrsh7th/cmp-nvim-lsp',
  },
}

--[[
rustfmt must be manually installed (not by Mason).

For details:
https://github.com/mason-org/mason-registry/issues/2054
--]]
