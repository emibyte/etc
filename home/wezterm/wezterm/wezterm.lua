-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where your actually apply your config choices

-- For example, changing the color scheme:
-- config.color_scheme = 'Batman'

config.adjust_window_size_when_changing_font_size = false;
-- config.hide_mouse_cursor_when_typing = false;

-- Set background to same color as neovim
-- config.colors = {}
-- config.colors.background = '#111111'
config.color_scheme = "Catppuccin Mocha"

config.enable_tab_bar = false

-- wezterm.font = {
--     family = 'Iosevka Term',
--     stretch = 'Expanded',
--     weight = 'Regular',
-- }


config.font = wezterm.font_with_fallback {
    'JetBrainsMono Nerd Font Mono',
    'Font Awesome 6 Brands',
    -- 'FantasqueSansM Nerd Font',
}

config.window_background_opacity = 1

-- default is true, has more "native" look
config.use_fancy_tab_bar = false

config.enable_scroll_bar = false
config.window_padding= {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.tab_bar_at_bottom = true
config.freetype_load_target = "HorizontalLcd"

-- and finally, return the configuration to wezterm	
return config
