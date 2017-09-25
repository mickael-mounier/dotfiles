# Check for an interactive session
test -z "$PS1" && return

function contains() {
    string="$1"
    substring="$2"
    if test "${string#*$substring}" != "$string" ; then
        return 0
    else
        return 1
    fi
}

# Git completion
test -f /usr/share/git/completion/git-prompt.sh && source /usr/share/git/completion/git-prompt.sh
contains $SHELL 'bash' && test -f /usr/share/git/completion/git-completion.bash && source /usr/share/git/completion/git-completion.bash

# Prompt
function __prompt_command() {
    # FIXME: this function blows
    local EXIT="$?"
    history -a  # Save command in history file
    PS1=""
    CURR_COUNT=0
    if [ x"$VIRTUAL_ENV" != x"" ] ; then
        CURR_VENV="`basename \"$VIRTUAL_ENV\"`"
        CURR_COUNT=$(($CURR_COUNT+1))
    fi
    if type __git_ps1 2>/dev/null | \grep -q 'function' 2>/dev/null; then
        CURR_GIT_HEAD="`__git_ps1 | tr -d '() '`"
        CURR_COUNT=$(($CURR_COUNT+1))
    fi
    test $CURR_COUNT -gt 0 && PS1="$PS1("
    test x"$CURR_VENV" != x"" && PS1="${PS1}\[\e[34;1m\]$CURR_VENV\[\e[0;0m\]"
    test $CURR_COUNT -gt 1 && PS1="$PS1|" # not good but w/e
    test x"$CURR_GIT_HEAD" != x"" && PS1="${PS1}\[\e[34;1m\]$CURR_GIT_HEAD\[\e[0;0m\]"
    test $CURR_COUNT -gt 0 && PS1="$PS1) "

    PS1="$PS1\[\e[36;1m\]\j \[\e[32;1m\]\u\[\e[37;0m\]@\[\e[33;1m\]\h \[\e[34;1m\]\w"
    test $EXIT != 0 && PS1="$PS1 \e[31;1m$EXIT\e[0;0m"
    PS1="$PS1 \[\e[35;1m\]> \[\e[37;0m\]"
}

export PROMPT_COMMAND=__prompt_command

# Ghetto proxy management
export ORGINAL_httpproxy=$http_proxy
export ORGINAL_httpsproxy=$https_proxy
export CUSTOM_httpproxy=$http_proxy   # This will be customized later if needed
export CUSTOM_httpsproxy=$https_proxy # This will be customized later if needed

function pon()
{
    export http_proxy=$CUSTOM_httpproxy
    export https_proxy=$CUSTOM_httpsproxy
}

function poff()
{
    export http_proxy=$ORIGINAL_httpproxy
    export https_proxy=$ORIGINAL_httpsproxy
}

# EDITOR
command -v emacs >/dev/null 2>&1 && export EDITOR="emacs"

# Bash history settings
shopt -s histappend                  # Append to history file
export HISTFILESIZE=                 # Unlimited history size
export HISTSIZE=                     # Unlimited history length
export HISTCONTROL=ignoreboth        # Ignore repeats and lines starting by a space
export HISTIGNORE='ls:bg:fg:history' # Commands to ignore
export HISTTIMEFORMAT='%F %T '       # Prepend easy to parse timestamps
shopt -s cmdhist                     # Multiline commands on a single history line

# Virtualenv
test x"$WORKON_HOME" != x"" && test -f "$WORKON_HOME/bin/activate" && VIRTUAL_ENV_DISABLE_PROMPT=1 source "$WORKON_HOME/bin/activate"
# FIXME: make it work for windows!

# export MALLOC_OPTIONS=J
# export GNOME_DISABLE_CRASH_DIALOG=1

# Aliases
command -v emacs                >/dev/null 2>&1 && alias e="emacs"
command -v yaourt               >/dev/null 2>&1 && alias pacman="yaourt"
command -v colormake            >/dev/null 2>&1 && alias make="colormake"
command -v python               >/dev/null 2>&1 && alias py="python"
command -v xscreensaver-command >/dev/null 2>&1 && alias lock="xscreensaver-command -lock"
command -v git                  >/dev/null 2>&1 && alias git_amend='GIT_COMMITTER_DATE="`date`" git commit --amend --date "`date`"'

alias h="history"
alias f="find"
alias grep="\grep --color"
alias g="grep"

# ls configuration
LS_BIN="ls"
command -v gls >/dev/null 2>&1 && LS_BIN="gls"

if $LS_BIN --color -d . >/dev/null 2>&1; then
    LS_TYPE="GNU"
elif $LS_BIN -G -d . >/dev/null 2>&1; then
    LS_TYPE="BSD"
fi

case $LS_TYPE in
    GNU)
        LS_OPTS='-X --group-directories-first --color'
        ;;
    BSD)
        LS_OPTS='-G'
        ;;
esac

alias   l="\\$LS_BIN $LS_OPTS -hlL"
alias  ll="\\$LS_BIN $LS_OPTS -hal"
alias  ls="\\$LS_BIN $LS_OPTS -hlL"
alias  lt="\\$LS_BIN $LS_OPTS -hlLrt"

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
*.cfg=01;36:\
*.conf=01;36:\
*.json=01;36:\
*.yaml=01;36:\
*.yml=01;36:\
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

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# Load local settings
test -f ~/.bashrc_local && source ~/.bashrc_local
