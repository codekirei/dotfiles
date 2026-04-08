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
      'cssls',
      'docker_compose_language_service',
      'dockerls',
      'gopls', -- go
      'html',
      'jsonls',
      'lua_ls',
      'marksman', -- markdown
      'pyright', -- python
      'ruff', -- python
      'sqlls',
      'rust_analyzer',
      'ts_ls', -- typescript
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
      'eslint_d',
      'gofumpt',
      'goimports',
      'oxfmt',
      'prettier',
      'shfmt',
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
      css = { 'prettier', 'oxfmt' },
      go = { 'goimports', 'gofumpt' },
      javascript = { 'prettier', 'oxfmt' },
      json = { 'prettier', 'oxfmt' },
      jsonc = { 'prettier', 'oxfmt' },
      lua = { 'stylua' },
      markdown = { 'prettier', 'oxfmt' },
      rust = { 'rustfmt' }, -- see comment at EOF
      sh = { 'shfmt' },
      sql = { 'sql_formatter' },
      typescript = { 'prettier', 'oxfmt' },
      typescriptreact = { 'prettier', 'oxfmt' },
    },
    formatters = {
      sql_formatter = {
        args = { '--config', '.sql-formatter.json' },
      },
      prettier = {
        prefer_local = true,
        condition = function(self, ctx)
          local git = vim.fs.find('.git', { path = ctx.dirname, upward = true })[1]
          if not git then return false end
          local root = vim.fs.dirname(git)
          return vim.fs.find({
            '.prettierrc',
            '.prettierrc.js',
            '.prettierrc.cjs',
            '.prettierrc.mjs',
            '.prettierrc.json',
            '.prettierrc.json5',
            '.prettierrc.yaml',
            '.prettierrc.yml',
            '.prettierrc.toml',
            'prettier.config.js',
            'prettier.config.cjs',
            'prettier.config.mjs',
            'prettier.config.ts',
          }, { path = root, limit = 1 })[1] ~= nil
        end,
      },
      oxfmt = {
        command = function(self, ctx)
          local git = vim.fs.find('.git', { path = ctx.dirname, upward = true })[1]
          if git then
            local local_bin = vim.fs.dirname(git) .. '/node_modules/.bin/oxfmt'
            if vim.fn.executable(local_bin) == 1 then
              return local_bin
            end
          end
          return 'oxfmt'
        end,
        args = function(self, ctx)
          local git = vim.fs.find('.git', { path = ctx.dirname, upward = true })[1]
          local args = { '--stdin-filepath', ctx.filename }
          if git then
            local config = vim.fs.dirname(git) .. '/.oxfmtrc.json'
            vim.list_extend(args, { '-c', config })
          end
          return args
        end,
        stdin = true,
        condition = function(self, ctx)
          local git = vim.fs.find('.git', { path = ctx.dirname, upward = true })[1]
          if not git then return true end
          local root = vim.fs.dirname(git)
          return vim.fs.find({ '.oxfmtrc.json' }, { path = root, limit = 1 })[1] ~= nil
        end,
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

  vim.lsp.log.set_level(vim.log.levels.WARN)
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
