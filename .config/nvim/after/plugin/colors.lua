function SetColorScheme(color)
	color = color or "tokyonight-night"
	vim.cmd.colorscheme(color)
	vim.cmd.hi("Comment gui=none")
	vim.cmd("hi Pmenu guibg=None")
	vim.cmd("hi NormalFloat guibg=None")
end

SetColorScheme("modus")
-- vim: ts=2 sts=2 sw=2 et
