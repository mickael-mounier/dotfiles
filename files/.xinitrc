# Use my .Xresources
xrdb -merge ~/.Xresources

# Start VBox guest additions if available
command -v VBoxClient-all >/dev/null 2>&1 && VBoxClient-all

# Start an openbox session
exec openbox-session
