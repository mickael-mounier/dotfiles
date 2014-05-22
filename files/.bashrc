# Check for an interactive session
test -z "$PS1" && return

test -d /usr/local/lib && export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"
test -d $HOME/bin && export PATH="$HOME/bin:$PATH"

export PS1="\[\e[36;1m\]\j \[\e[32;1m\]\u\[\e[37;0m\]@\[\e[33;1m\]\h \[\e[34;1m\]\w \[\e[35;1m\]> \[\e[37;0m\]"

case $HOSTNAME in
    shodan) # Home
        ;;
    mmounier) # ETAI
        export JAVA_HOME="$HOME/apps/jdk1.7.0_51"
        export JAVA="$JAVA_HOME/bin/java"
        # export CATALINA_HOME="$HOME/apps/apache-tomcat-7.0.52"
        export CATALINA_HOME="$HOME/apps/apache-tomcat-5.5.36"
        export ORACLE_HOME="/usr/lib/oracle/11.2/client64"
        export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$ORACLE_HOME/lib"
        export PATH="$JAVA_HOME/bin:$PATH"

        if [ x"$DISPLAY" != x"" ] ; then
            xrdb -merge ~/.Xdefaults
            xrandr --auto --output VGA1 --mode 1280x1024 --right-of HDMI1
            xrandr --auto --output HDMI1 --mode 1920x1080 --left-of VGA1
            $HOME/bin/setlayout 0 4 4 0
        fi

        VIRTUAL_ENV_DISABLE_PROMPT=1 test -x "$HOME/work/virtualenv/bin/activate" && source "$HOME/work/virtualenv/bin/activate"
        ;;
esac

# export MALLOC_OPTIONS=J
# export GNOME_DISABLE_CRASH_DIALOG=1

command -v emacs                >/dev/null 2>&1 && export EDITOR='emacs'
command -v emacs                >/dev/null 2>&1 && alias  e="emacs"
command -v yaourt               >/dev/null 2>&1 && alias  pacman="yaourt"
command -v colormake            >/dev/null 2>&1 && alias  make="colormake"
command -v python               >/dev/null 2>&1 && alias  py="python"
command -v svn                  >/dev/null 2>&1 && alias  svnst="svn st --ignore-externals -q"
command -v xscreensaver-command >/dev/null 2>&1 && alias  lock="xscreensaver-command -lock"

alias   l="\ls -hlGLX   --color --group-directories-first --ignore='*.pyc'"
alias  ll="\ls -halX    --color --group-directories-first"
alias llr="\ls -halXR   --color --group-directories-first"
alias llt="\ls -halXrt  --color --group-directories-first"
alias  lr="\ls -hlGLXR  --color --group-directories-first --ignore='*.pyc'"
alias  ls="\ls -hlGLX   --color --group-directories-first --ignore='*.pyc'"
alias  lt="\ls -hlGLXrt --color --group-directories-first --ignore='*.pyc'"

export LS_COLORS="\
no=00:\
fi=00:\
di=01;04;94:\
ln=01;07;36:\
pi=40;33:\
so=01;35:\
do=01;35:\
bd=40;33;01:\
cd=40;33;01:\
or=40;31;01:\
su=37;41:\
sg=30;43:\
tw=30;42:\
ow=34;42:\
st=37;44:\
ex=01;32:\
*.tar=01;31:\
*.tgz=01;31:\
*.svgz=01;31:\
*.arj=01;31:\
*.taz=01;31:\
*.lzh=01;31:\
*.lzma=01;31:\
*.zip=01;31:\
*.z=01;31:\
*.Z=01;31:\
*.dz=01;31:\
*.gz=01;31:\
*.bz2=01;31:\
*.bz=01;31:\
*.tbz2=01;31:\
*.tz=01;31:\
*.deb=01;31:\
*.rpm=01;31:\
*.jar=01;31:\
*.rar=01;31:\
*.ace=01;31:\
*.zoo=01;31:\
*.cpio=01;31:\
*.7z=01;31:\
*.rz=01;31:\
*.jpg=01;35:\
*.jpeg=01;35:\
*.gif=01;35:\
*.bmp=01;35:\
*.pbm=01;35:\
*.pgm=01;35:\
*.ppm=01;35:\
*.tga=01;35:\
*.xbm=01;35:\
*.xpm=01;35:\
*.tif=01;35:\
*.tiff=01;35:\
*.png=01;35:\
*.svg=01;35:\
*.mng=01;35:\
*.pcx=01;35:\
*.mov=01;35:\
*.mpg=01;35:\
*.mpeg=01;35:\
*.m2v=01;35:\
*.mkv=01;35:\
*.ogm=01;35:\
*.mp4=01;35:\
*.m4v=01;35:\
*.mp4v=01;35:\
*.vob=01;35:\
*.qt=01;35:\
*.nuv=01;35:\
*.wmv=01;35:\
*.asf=01;35:\
*.rm=01;35:\
*.rmvb=01;35:\
*.flc=01;35:\
*.avi=01;35:\
*.fli=01;35:\
*.gl=01;35:\
*.dl=01;35:\
*.xcf=01;35:\
*.xwd=01;35:\
*.yuv=01;35:\
*.aac=00;36:\
*.au=00;36:\
*.flac=00;36:\
*.mid=00;36:\
*.midi=00;36:\
*.mka=00;36:\
*.mp3=00;36:\
*.html=01;36:\
*.htm=01;36:\
*.xml=01;36:\
*.mpc=00;36:\
*.ogg=00;36:\
*.ra=00;36:\
*.wav=00;36:\
*.h=01;33:\
*.hh=01;33:\
*.hxx=01;33:\
*.c=01;37:\
*.cc=01;37:\
*.ll=01;37:\
*.l=01;37:\
*.yy=01;37:\
*.y=01;37:\
*.cc=01;37:\
*.cpp=01;37:\
*.cxx=01;37:\
*.vcproj=01;92;40:\
*.o=00;37:\
*.oo=00;37:\
*.lo=00;37:\
*.a=01;31:\
*.so=01;31:"
