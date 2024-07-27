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
    }
}

vim.opt.rtp:prepend(lazypath)

local plugins = {
    -- Treesitter: Better syntax highlighting
    {
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate'
    },

    -- Telescope: Jumping around files
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.3',
        dependencies = { 'nvim-lua/plenary.nvim' }
    },

    -- Oil: A better way to navigate filesystem
    {
        'stevearc/oil.nvim',
        opts = {},
        -- Optional dependencies
        dependencies = { "nvim-tree/nvim-web-devicons" },
    },

    -- Nvim tree: A file tree for nvim
    { 'nvim-tree/nvim-tree.lua' },

    -- Icons
    { 'nvim-tree/nvim-web-devicons' },

    -- Install language servers from inside neovim
    { 'williamboman/mason.nvim' },
    { 'williamboman/mason-lspconfig.nvim' },

    -- LSP
    { 'VonHeikemen/lsp-zero.nvim',        branch = 'v3.x' },
    { 'neovim/nvim-lspconfig' },
    { 'hrsh7th/cmp-nvim-lsp' },

    -- Formatting
    {
        'stevearc/conform.nvim',
        opts = {},
    },
    -- Completions
    { 'hrsh7th/nvim-cmp' },

    -- Snippets
    { 'L3MON4D3/LuaSnip' },
    { 'saadparwaiz1/cmp_luasnip' },

    -- Comments
    {
        'numToStr/Comment.nvim'
    },

    -- ToggleTerm: Terminal inside vim
    { 'akinsho/toggleterm.nvim',      version = "*", config = true },

    -- Autopairs: Autocomplete for brackets
    {
        'windwp/nvim-autopairs',
        event = "InsertEnter",
        opts = {}
    },

    -- Surround
    { 'kylechui/nvim-surround' },

    -- Table-mode: Better tables in vim
    { 'dhruvasagar/vim-table-mode' },

    -- Follow MD Links: Jump around in markdown
    { 'jghauser/follow-md-links.nvim' },

    -- Lualine
    {
        "nvim-lualine/lualine.nvim",
        opts = {}
    },

    -- THEMES
    -- TokyoNight: Tokyo night vibes in vim
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        opts = {},
    },
    -- Gruvbox: Earth, green
    { "ellisonleao/gruvbox.nvim",     priority = 1000, config = true, opts = ... },

    -- Kanagawa: A dark theme inspiired by Japanese aesthetics
    {
        "rebelot/kanagawa.nvim"
    },
    -- oxocarbon: dark, cool
    {
        "nyoom-engineering/oxocarbon.nvim"
    },
    { "miikanissi/modus-themes.nvim", priority = 1000 },
    {
        "neanias/everforest-nvim",
        version = false,
        lazy = false,
        priority = 1000,
    },
}

require("lazy").setup(plugins, opts)
