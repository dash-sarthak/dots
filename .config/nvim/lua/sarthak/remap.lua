vim.g.mapleader = " "
vim.keymap.set('n', '<C-k>', ':wincmd k<CR>', { noremap = true, silent = true })
vim.keymap.set('n', '<C-j>', ':wincmd j<CR>', { noremap = true, silent = true })
vim.keymap.set('n', '<C-l>', ':wincmd l<CR>', { noremap = true, silent = true })
vim.keymap.set('n', '<C-h>', ':wincmd h<CR>', { noremap = true, silent = true })

vim.keymap.set('n', '<leader>sv', ':vsplit<CR>')
vim.keymap.set('n', '<leader>sh', ':split<CR>')
