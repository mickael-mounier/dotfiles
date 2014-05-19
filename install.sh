#!/bin/sh

deploy() {
    for i in `find $1 -maxdepth 1 -type f` ; do
        DSTFILE="$2/`basename $i`"
        PDSTFILE="\033[36m`echo $DSTFILE | sed -e "s|$HOME|~|"`\033[0m"
        if [ -h "$DSTFILE" ] ; then
            echo "$PDSTFILE" # TODO: check if we use the right symlink
        elif [ -f "$DSTFILE" ] ; then
            echo "WARNING: $PDSTFILE file already exists, delete it to use the dotfile version"
        else
            echo "NEW: $PDSTFILE"
            ln -s "$i" "$DSTFILE"
        fi
    done
    for i in `find $1 -mindepth 1 -maxdepth 1 -type d` ; do
        DIRNAME=`basename $i`
        test -d $2/$DIRNAME || mkdir $2/$DIRNAME
        deploy $1/$DIRNAME $2/$DIRNAME
    done
}

if [ x"`dirname $0`" = x"." ] ; then
    FILESDIR="$PWD/files"
else
    FILESDIR="$PWD/`dirname $0`/files"
fi

deploy $FILESDIR $HOME
