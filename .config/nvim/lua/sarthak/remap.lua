vim.g.mapleader = " "
-- vim.keymap.set('n', '<C-k>', ':wincmd k<CR>', { noremap = true, silent = true })
-- vim.keymap.set('n', '<C-j>', ':wincmd j<CR>', { noremap = true, silent = true })
-- vim.keymap.set('n', '<C-l>', ':wincmd l<CR>', { noremap = true, silent = true })
-- vim.keymap.set('n', '<C-h>', ':wincmd h<CR>', { noremap = true, silent = true })
--
vim.keymap.set("n", "<leader>sv", ":vsplit<CR>")
vim.keymap.set("n", "<leader>sh", ":split<CR>")

vim.keymap.set("n", "<leader>b", ":set nomore <Bar> :ls <Bar> :set more <CR>:b<Space>")
-- vim: ts=2 sts=2 sw=2 et
