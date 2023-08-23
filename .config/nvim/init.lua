-- import legacy vimrc
local legacy_vimrc = vim.fn.stdpath("config") .. "/legacy.vim"
vim.cmd.source(legacy_vimrc)

vim.opt.mouse = "" -- disable mouse support

local Plug = vim.fn['plug#'] -- use vim.fn and vim.call to use vim-plug
vim.call('plug#begin', '~/.config/nvim/plugged')
-- START: Dependencies for lsp-zero.nvim
-- LSP Support
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'

-- Autocompletion
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'L3MON4D3/LuaSnip'
-- END: Dependencies for lsp-zero.nvim

-- LSP Zero
Plug 'VonHeikemen/lsp-zero.nvim'
vim.call('plug#end')

local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
end)

lsp.setup()
