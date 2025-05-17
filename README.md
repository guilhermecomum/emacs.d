# Modern Emacs Configuration

This is a clean, modular Emacs configuration designed for:

- TypeScript/JavaScript development (Remix apps, React)
- Ruby on Rails development
- Shopify Theme development
- Org-mode productivity (org-roam, org-super-agenda)

## Features

- **Package Management**: Uses `straight.el` for reproducible package management
- **Modern UI**: Doom themes, modeline, dashboard, nerd-icons
- **Completion**: Corfu for in-buffer completion with LSP integration
- **Project Management**: Projectile for navigating projects
- **Git Integration**: Magit, git-gutter, git-timemachine
- **Search**: Vertico, Consult, Orderless
- **Development**:
  - LSP Mode for intelligent code completion and navigation
  - Tree-sitter for better syntax highlighting and code navigation
  - Flycheck for real-time syntax checking
  - Prettier/Rubocop for auto-formatting
  - Language-specific setups for TypeScript/JavaScript, Ruby, HTML, CSS
  - Shopify Theme language server integration
- **Org Mode**:
  - Org-roam for knowledge management
  - Org-super-agenda for task organization
  - Modern visual enhancements

## Structure

The configuration is organized in a modular way:

- `early-init.el`: Early initialization for better startup performance
- `init.el`: Main initialization file that loads other modules
- `config/base.el`: Core Emacs settings
- `config/ui.el`: User interface customizations
- `config/editor.el`: Text editing enhancements
- `config/dev-core.el`: Core development tools
- `config/typescript.el`: TypeScript/JavaScript specific setup
- `config/ruby.el`: Ruby/Rails specific setup
- `config/web.el`: HTML/CSS/Shopify specific setup
- `config/org.el`: Org-mode configuration

## Requirements

- Emacs 29+ (for built-in tree-sitter support)
- Node.js and npm (for JavaScript development tools)
- Ruby (for Rails development)
- Shopify CLI (for Shopify theme development)
- Git

## Optional System Packages

- ripgrep (for fast searching)
- fd (for fast file finding)
- emacs-lsp-booster (for LSP performance)

## Installation

1. Backup your existing configuration:
   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   ```

2. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/emacs-config.git ~/.emacs.d
   ```

3. Start Emacs and let packages install automatically.

## Key Bindings

### Global

- `C-c p`: Projectile command prefix
- `C-x g`: Magit status
- `C-c t`: Open vterm terminal
- `C-c d`: Docker command
- `C-s`: Consult line search
- `C-c n r`: Run npm/yarn script

### TypeScript/JavaScript

- `C-c C-l`: Insert console.log statement
- Many LSP bindings under `C-c l` prefix

### Ruby

- `C-c r c`: Open Rails console
- `C-c r s`: Start Rails server

### Shopify

- `C-c s v`: Validate Shopify theme

### Org-mode

- `C-c a`: Org agenda
- `C-c c`: Org capture
- `C-c n f`: Find org-roam node
- `C-c n i`: Insert org-roam node link

## Customization

You can customize this config by:

1. Editing the files in `~/.emacs.d/config/`
2. Creating a `~/.emacs.d/custom.el` file for user-specific customizations

## License

MIT
