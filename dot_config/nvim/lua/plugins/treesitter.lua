local opts = {
	ensure_installed = {
		"c",
		"css",
		"csv",
		"diff",
		"dockerfile",
		"elixir",
		"erlang",
		"go",
		"gomod",
		"gpg",
		"graphql",
		"html",
		"javascript",
		"jsdoc",
		"json",
		"lua",
		"make",
		"norg",
		"python",
		"query",
		"rust",
		"scss",
		"sql",
		"terraform",
		"typescript",
		"vim",
		"vimdoc",
		"yaml",
	},
	highlight = { enable = true },
}

local function config()
	require("nvim-treesitter.configs").setup(opts)
	require("nvim-treesitter.install").compilers = { "gcc-11" }
	require("tree-sitter-just").setup({})
end

return {
	"nvim-treesitter/nvim-treesitter",
	config = config,
	dependencies = {
		"IndianBoy42/tree-sitter-just",
	},
}
