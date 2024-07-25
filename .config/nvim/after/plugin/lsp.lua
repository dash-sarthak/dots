local lsp_zero = require('lsp-zero')
local lspconfig = require('lspconfig')

lsp_zero.on_attach(function(_, bufnr)
    lsp_zero.default_keymaps({buffer = bufnr})
end)

local function lua_handler()
    lspconfig.lua_ls.setup({
        settings = {
            Lua = {
                diagnostics = {
                    globals = { 'vim' },
                }
            },
        },

    })
end

local function go_handler()
    lspconfig.gopls.setup({
        settings = {
            gopls = {
                completeUnimported = true,
                usePlaceholders = true,
                staticcheck = true,
                gofumpt = true
            }
        }
    })
end

require('mason').setup({})
require('mason-lspconfig').setup({
    ensure_installed = {
        'pyright',
        'gopls',
        'lua_ls'
    },
    handlers = {
        lsp_zero.default_setup,
        lua_ls = lua_handler,
        gopls = go_handler
    }
})
