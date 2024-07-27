function SetColorScheme(color)
    color = color or "tokyonight-night"
    vim.cmd.colorscheme(color)
    vim.cmd 'hi Pmenu guibg=None'
    if color == "kanagawa-dragon" then
        vim.cmd 'hi Pmenuthumb guibg=#282828'
    elseif color == "tokyonight-night" then
        vim.cmd 'hi Pmenuthumb guibg=#2f323f'
    end
    vim.cmd 'hi normal ctermbg=none guibg=none'
end

SetColorScheme("modus")
