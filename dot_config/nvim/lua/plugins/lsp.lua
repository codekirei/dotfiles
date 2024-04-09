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
      'clangd', -- c, c++
      'cssls',
      'docker_compose_language_service',
      'dockerls',
      'gopls', -- go
      'html',
      'jsonls',
      'lua_ls',
      'marksman', -- markdown
      'pyright', -- python
      'ruff_lsp', -- python
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
      ['pyright'] = function()
        lspconfig.pyright.setup({
          capabilities = _capabilities,
          settings = {
            python = {
              analysis = {
                diagnosticSeverityOverrides = {
                  reportIncompatibleVariableOverride = 'none',
                  reportIncompatibleMethodOverride = 'none',
                },
              },
            },
          },
        })
      end,
      -- ['eslint'] = function()
      --   lspconfig.eslint.setup({
      --     capabilities = _capabilities,
      --     root_dir = lspconfig.util.root_pattern(
      --       'eslint.config.js',
      --       'eslint.config.cjs',
      --       'eslint.config.mjs',
      --       '.eslintrc.js',
      --       '.eslintrc.cjs',
      --       '.eslintrc.json',
      --       'package.json',
      --       '.git'
      --     ),
      --     settings = {
      --       eslint = {
      --         useFlatConfig = true,
      --         workingDirectories = { mode = 'auto' },
      --         validate = {
      --           'javascript',
      --           'javascriptreact',
      --           'typescript',
      --           'typescriptreact',
      --         },
      --         -- helpful debug override
      --         options = {
      --           overrideConfigFile = vim.fn.getcwd() .. '/eslint.config.mjs',
      --         },
      --       },
      --     },
      --   })
      -- end,
    },
  })

  require('mason-tool-installer').setup({
    -- Use Mason package names.
    ensure_installed = {
      'clang-format',
      'gofumpt',
      'goimports',
      'prettier',
      'stylua',
      'sql-formatter',
      'sqlfluff',
    },
  })

  -- Supplementary linting. Many LSPs do linting, but sometimes a dedicated
  -- linter is better.
  vim.env.ESLINT_D_PPID = vim.fn.getpid()
  require('lint').linters_by_ft = {
    sql = { 'sqlfluff' },
    typescript = { 'eslint_d' },
    typescriptreact = { 'eslint_d' },
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
      javascript = { 'prettier' },
      json = { 'prettier' },
      jsonc = { 'prettier' },
      lua = { 'stylua' },
      markdown = { 'prettier' },
      rust = { 'rustfmt' }, -- see comment at EOF
      sh = { 'shfmt' },
      sql = { 'sql_formatter' },
      typescript = { 'prettier' },
      typescriptreact = { 'prettier' },
    },
    formatters = {
      sql_formatter = {
        args = { '--config', '.sql-formatter.json' },
      },
      prettier = {
        prefer_local = true,
      },
      shfmt = {
        args = { '-i', '2' },
      },
    },
    format_after_save = {
      lsp_fallback = true,
    },
    -- log_level = vim.log.levels.DEBUG,
  })

  vim.lsp.set_log_level(vim.log.levels.DEBUG)
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
    {
      'stevearc/conform.nvim',
      version = 'v7.1.0',
    },
    'hrsh7th/cmp-nvim-lsp',
  },
}

--[[
rustfmt must be manually installed (not by Mason).

For details:
https://github.com/mason-org/mason-registry/issues/2054
--]]
