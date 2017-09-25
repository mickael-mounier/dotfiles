# Muting output because some users won't have the right to set ulimit
ulimit -s unlimited 2>/dev/null
umask 077

function title()
{
    echo -ne "\033]0;$*\007"
}

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

function prepend_to_path()
{
    test -d "$1" && export PATH=$(prepend_to_var "$1" "$PATH")
}

# Update LD_LIBRARY_PATH
test -d /usr/local/lib && export LD_LIBRARY_PATH=$(append_to_var "/usr/local/lib" '$LD_LIBRARY_PATH')

# Update PATH (ordered by least precedence)
prepend_to_path /bin
prepend_to_path /usr/bin
prepend_to_path /usr/local/bin
prepend_to_path $HOME/bin
prepend_to_path $HOME/local/bin
prepend_to_path $HOME/node_modules/.bin

# Load local profile if any
test -f ~/.profile_local && source ~/.profile_local
