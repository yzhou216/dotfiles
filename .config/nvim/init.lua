-- import legacy vimrc
local legacy_vimrc = vim.fn.stdpath("config") .. "/legacy.vim"
vim.cmd.source(legacy_vimrc)

vim.opt.mouse = "" -- disable mouse support
vim.cmd("autocmd TermOpen * startinsert") -- start terminal emulator in insert mode

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

-- nvim-cmp autocompletion
local cmp = require'cmp'

cmp.setup({
  mapping = {
    ['<C-n>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end),
    ['<C-p>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
    ['<Tab>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
  },
})
