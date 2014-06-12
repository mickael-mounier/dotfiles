# Check for an interactive session
test -z "$PS1" && return

function append_to_var()
{
    RES=""
    OLD_IFS=$IFS
    IFS=":"
    for i in $2 ; do
        test x"$i" == x"$1" && continue
        if [ x"$RES" != x"" ] ; then
            RES="$RES:$i"
        else
            RES="$i"
        fi
    done
    IFS=$OLD_IFS
    if [ x"$RES" != x"" ] ; then
        RES="$RES:$1"
    else
        RES="$1"
    fi
    echo $RES
}

function prepend_to_var()
{
    RES=""
    OLD_IFS=$IFS
    IFS=":"
    for i in $2 ; do
        test x"$i" == x"$1" && continue
        if [ x"$RES" != x"" ] ; then
            RES="$RES:$i"
        else
            RES="$i"
        fi
    done
    IFS=$OLD_IFS
    if [ x"$RES" != x"" ] ; then
        RES="$1:$RES"
    else
        RES="$1"
    fi
    echo $RES
}

test -d /usr/local/lib && export LD_LIBRARY_PATH=$(append_to_var  /usr/local/lib $LD_LIBRARY_PATH)
test -d $HOME/bin      && export PATH=$(prepend_to_var $HOME/bin      $PATH)

function __prompt_command() {
    local EXIT="$?"

    PS1="\[\e[36;1m\]\j \[\e[32;1m\]\u\[\e[37;0m\]@\[\e[33;1m\]\h \[\e[34;1m\]\w"

    if [ $EXIT != 0 ]; then
        PS1+=" \e[31;1m$EXIT\e[0;0m"
    fi

    PS1="$PS1 \[\e[35;1m\]> \[\e[37;0m\]"
}

export PROMPT_COMMAND=__prompt_command
export TERM=xterm

case $HOSTNAME in
    shodan) # Home
        export WORKON_HOME="$HOME/virtualenv"
        ;;
    mmounier) # ETAI
        export JAVA_HOME="$HOME/apps/jdk1.7.0_51"
        export JAVA="$JAVA_HOME/bin/java"
        # export CATALINA_HOME="$HOME/apps/apache-tomcat-7.0.52"
        export CATALINA_HOME="$HOME/apps/apache-tomcat-5.5.36"
        export ORACLE_HOME="/usr/lib/oracle/11.2/client64"
        export LD_LIBRARY_PATH=$(append_to_var  $ORACLE_HOME/lib $LD_LIBRARY_PATH)
        export PATH=$(prepend_to_var $JAVA_HOME/bin   $PATH)
        export WORKON_HOME="$HOME/work/virtualenv"

        if [ x"$DISPLAY" != x"" ] ; then
            xrdb -merge ~/.Xdefaults
            xrandr --auto --output VGA1 --mode 1280x1024 --right-of HDMI1
            xrandr --auto --output HDMI1 --mode 1920x1080 --left-of VGA1
            $HOME/bin/setlayout 0 4 4 0
        fi
        ;;
esac

test x"$WORKON_HOME" != x"" && test -f "$WORKON_HOME/bin/activate" && VIRTUAL_ENV_DISABLE_PROMPT=1 source "$WORKON_HOME/bin/activate"

# export MALLOC_OPTIONS=J
# export GNOME_DISABLE_CRASH_DIALOG=1

command -v emacs                >/dev/null 2>&1 && export EDITOR="emacs"
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
*.7z=01;31:\
*.Z=01;31:\
*.a=01;31:\
*.aac=00;36:\
*.ace=01;31:\
*.arj=01;31:\
*.asf=01;35:\
*.au=00;36:\
*.avi=01;35:\
*.bmp=01;35:\
*.bz2=01;31:\
*.bz=01;31:\
*.c=01;37:\
*.cc=01;37:\
*.cc=01;37:\
*.cpio=01;31:\
*.cpp=01;37:\
*.cxx=01;37:\
*.deb=01;31:\
*.dl=01;35:\
*.dz=01;31:\
*.flac=00;36:\
*.flc=01;35:\
*.fli=01;35:\
*.gif=01;35:\
*.gl=01;35:\
*.gz=01;31:\
*.h=01;33:\
*.hh=01;33:\
*.htm=01;36:\
*.html=01;36:\
*.hxx=01;33:\
*.jar=01;31:\
*.jpeg=01;35:\
*.jpg=01;35:\
*.l=01;37:\
*.ll=01;37:\
*.lo=00;37:\
*.lzh=01;31:\
*.lzma=01;31:\
*.m2v=01;35:\
*.m4v=01;35:\
*.mid=00;36:\
*.midi=00;36:\
*.mka=00;36:\
*.mkv=01;35:\
*.mng=01;35:\
*.mov=01;35:\
*.mp3=00;36:\
*.mp4=01;35:\
*.mp4v=01;35:\
*.mpc=00;36:\
*.mpeg=01;35:\
*.mpg=01;35:\
*.nuv=01;35:\
*.o=00;37:\
*.ogg=00;36:\
*.ogm=01;35:\
*.oo=00;37:\
*.pbm=01;35:\
*.pcx=01;35:\
*.pgm=01;35:\
*.png=01;35:\
*.ppm=01;35:\
*.py=01;37:\
*.pyc=00;37:\
*.qt=01;35:\
*.ra=00;36:\
*.rar=01;31:\
*.rm=01;35:\
*.rmvb=01;35:\
*.rpm=01;31:\
*.rz=01;31:\
*.so=01;31:\
*.svg=01;35:\
*.svgz=01;31:\
*.tar=01;31:\
*.taz=01;31:\
*.tbz2=01;31:\
*.tga=01;35:\
*.tgz=01;31:\
*.tif=01;35:\
*.tiff=01;35:\
*.tz=01;31:\
*.vcproj=01;92;40:\
*.vob=01;35:\
*.wav=00;36:\
*.wmv=01;35:\
*.xbm=01;35:\
*.xcf=01;35:\
*.xml=01;36:\
*.xpm=01;35:\
*.xwd=01;35:\
*.y=01;37:\
*.yuv=01;35:\
*.yy=01;37:\
*.z=01;31:\
*.zip=01;31:\
*.zoo=01;31:\
bd=40;33;01:\
cd=40;33;01:\
di=01;04;94:\
do=01;35:\
ex=01;32:\
fi=00:\
ln=01;07;36:\
no=00:\
or=40;31;01:\
ow=34;42:\
pi=40;33:\
sg=30;43:\
so=01;35:\
st=37;44:\
su=37;41:\
tw=30;42:"
