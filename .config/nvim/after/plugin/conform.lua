require("conform").setup({
	format_on_save = {
		timeout_ms = 500,
		lsp_format = "fallback",
	},
	formatters_by_ft = {
		lua = { "stylua" },
		python = { "black" },
		go = { "gofumpt" },
	},
})
-- vim: ts=2 sts=2 sw=2 et
