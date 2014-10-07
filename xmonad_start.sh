compton &

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

xsetroot -cursor_name left_ptr

# turn off Display Power Management Service (DPMS)
xset -dpms
setterm -blank 0 -powerdown 0

# turn off black Screensaver
xset s off

# add local font dir
xset +fp ~/.fonts

urxvtd &
feh --randomize --recursive --bg-fill ~/Wallpapers
#~/bar_start.sh &
unclutter -grab  &
xmonad
