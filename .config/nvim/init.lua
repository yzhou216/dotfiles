-- import legacy vimrc
local legacy_vimrc = vim.fn.stdpath("config") .. "/legacy.vim"
vim.cmd.source(legacy_vimrc)

vim.opt.mouse = "" -- disable mouse support
vim.cmd("autocmd TermOpen * startinsert") -- start terminal emulator in insert mode

-- bootstrap lazy.nvim
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
vim.opt.rtp:prepend(lazypath)

-- plugins
require("lazy").setup({
  'github/copilot.vim', -- GitHub Copilot
  'airblade/vim-gitgutter', -- show git diff in gutter
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},
      {'williamboman/mason.nvim'},
      {'williamboman/mason-lspconfig.nvim'},

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'L3MON4D3/LuaSnip'},
    }
  },
  {
    -- fuzzy finder: fzf
    'junegunn/fzf',
    on = {'FZF', 'Files', 'Rg'},
    run = function() vim.cmd('call fzf#install()') end
  },
  'junegunn/fzf.vim',
  'kylelaker/riscv.vim' -- RISC-V assembly syntax highlighting
})

-- improve vim-gitgutter colors
vim.api.nvim_set_hl(0, 'SignColumn', {})
vim.api.nvim_set_hl(0, 'GitGutterAdd', {ctermfg='green'})
vim.api.nvim_set_hl(0, 'GitGutterChange', {ctermfg='yellow'})
vim.api.nvim_set_hl(0, 'GitGutterDelete', {ctermfg='red'})
vim.api.nvim_set_hl(0, 'GitGutterChangeDelete', {ctermfg='yellow'})

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

-- disable GitHub Copilot by default on startup
vim.g.copilot_enabled = false

-- fuzzy search files: fzf
vim.api.nvim_set_keymap('n', '<Leader>sf', ':Files!<CR>', {noremap = true, silent = true})
