-- diagnostics
for name, icon in pairs({
  Error = '',
  Hint = '',
  Info = '',
  Warn = '',
}) do
  local hl_group = 'DiagnosticSign' .. name
  vim.fn.sign_define(hl_group, { text = icon })
end
