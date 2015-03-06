#! /bin/bash
echo $PATH 1>&2
compton &

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

xsetroot -cursor_name left_ptr

# turn off Display Power Management Service (DPMS)
xset -dpms
xset -b
setterm -blank 0 -powerdown 0

# turn off black Screensaver
xset s off

# add local font dir
xset +fp ~/.fonts

urxvtd &
scripts/wp slideshow &
#~/bar_start.sh &
unclutter -grab  &
/opt/urserver/urserver --daemon &
tmux start-server
xrdb -merge .Xresources
pulseaudio -D
wmname LG3D
mpd && mpc stop && mpdscribble &
xmonad
