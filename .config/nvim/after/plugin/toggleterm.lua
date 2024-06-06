--local colors = require("oxocarbon.colors").setup()

require("toggleterm").setup{
    size = 20,
    open_mapping = [[<c-Return>]],
    hide_numbers = true,
    autochdir = false,
    start_in_insert = true,
    insert_mappings = true,
    terminal_mappings = true,
    persist_size = true,
    persist_mode = true,
    direction = 'horizontal',
    close_on_exit = true,
    auto_scroll = true,
    shade_terminals = false,
    --highlights = {
    --    Normal = {
    --        guibg = colors.bg_dark
    --    }
    --}
}
