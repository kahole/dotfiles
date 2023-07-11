## Keybindings ##

## Workspace matrix
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "['<Super>Left']"
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Super>Right']"

## Terminal ctrl tabbing between tabs
gsettings set org.gnome.Terminal.Legacy.Keybindings:/org/gnome/terminal/legacy/keybindings/ next-tab '<Primary>Tab'
gsettings set org.gnome.Terminal.Legacy.Keybindings:/org/gnome/terminal/legacy/keybindings/ prev-tab '<Primary><Shift>Tab'

## Disable emoji hotkey
gsettings set org.freedesktop.ibus.panel.emoji hotkey "['']"

