local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end

local opts = {
	ui = {
		border = "rounded",
	},
}

vim.opt.rtp:prepend(lazypath)

local plugins = {
	{ -- Useful plugin to show you pending keybinds.
		"folke/which-key.nvim",
		event = "VimEnter", -- Sets the loading event to 'VimEnter'
	},

	{ -- Oil: A better way to navigate filesystem
		"stevearc/oil.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},

	{ -- Treesitter: Better syntax highlighting
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
	},

	{ -- Telescope: Jumping around files
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
				cond = function()
					return vim.fn.executable("make") == 1
				end,
			},
			{ "nvim-telescope/telescope-ui-select.nvim" },
		},
	},

	{ "williamboman/mason.nvim" }, -- Install language servers from inside neovim
	{ "williamboman/mason-lspconfig.nvim" },

	{ "VonHeikemen/lsp-zero.nvim", branch = "v4.x" }, -- LSP Helper
	{ "neovim/nvim-lspconfig" }, -- Neovim LSP
	{ "hrsh7th/cmp-nvim-lsp" }, -- Completion LSP

	{ "hrsh7th/nvim-cmp" }, -- Completions

	{ "stevearc/conform.nvim" }, -- Formatting

	{ "saadparwaiz1/cmp_luasnip" }, -- Snippets

	{ "numToStr/Comment.nvim" }, -- Comments

	{
		"windwp/nvim-autopairs", -- Autopairs: Autocomplete for brackets
		event = "InsertEnter",
		opts = {},
	},

	{ "alexghergh/nvim-tmux-navigation" }, -- Tmux navigation

	{ -- Collection of various small independent plugins/modules
		"echasnovski/mini.nvim",
	},

	{
		"miikanissi/modus-themes.nvim", -- Modus from emacs
		priority = 1000,
	},
}

require("lazy").setup(plugins, opts)
-- vim: ts=2 sts=2 sw=2 et
