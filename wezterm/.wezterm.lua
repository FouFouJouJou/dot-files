local wezterm = require "wezterm"
local config = {}

config.color_scheme = "Framer (base16)"
config.font = wezterm.font "Osaka"
config.enable_tab_bar = false
padding = 2
config.window_padding = { 
  left = padding
  , right = padding
  , top = padding
  , bottom = padding
}

return config
