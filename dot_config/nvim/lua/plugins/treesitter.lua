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

local function config(_, opts)
	require("nvim-treesitter.configs").setup(opts)
end

return {
	"nvim-treesitter/nvim-treesitter",
	opts = opts,
	config = config,
}
