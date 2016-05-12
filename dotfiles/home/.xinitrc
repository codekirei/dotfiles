[ -s ~/.Xmodmap ] && xmodmap ~/.Xmodmap
[ -s ~/.Xresources ] && xrdb -merge ~/.Xresources
[ -s ~/.config/mpd/pid ] && mpd

eval $(ssh-agent -s)
xset r rate 200 15
sxhkd &
xscreensaver -no-splash &
xsetroot -cursor_name left_ptr &
feh --randomize --bg-fill --no-fehbg ~/.config/wallpapers/* &
emacs --daemon
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
exec bspwm
