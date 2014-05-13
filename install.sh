#!/bin/sh

deploy() {
    local SRC=$1
    local DST=$2

    for i in `find $SRC -maxdepth 1 -type f` ; do
        FILENAME=`basename $i`
        if [ -h "$DST/$FILENAME" ] ; then
            echo "nothing to do for $DST/$FILENAME"
            # FIXME: check if we use the right symlink
        elif [ -f "$DST/$FILENAME" ] ; then
            echo "warning: $DST/$FILENAME already exists, please get rid of it and restart this script"
        else
            echo "linking $DST/$FILENAME"
            ln -s "$i" "$DST/$FILENAME"
            true
        fi
    done

    for i in `find $SRC -mindepth 1 -maxdepth 1 -type d` ; do
        DIRNAME=`basename $i`
        test -d $DST/$DIRNAME || echo mkdir $DST/$DIRNAME
        deploy $SRC/$DIRNAME $DST/$DIRNAME
    done
}

if [ x"$(dirname $0)" = x"." ] ; then
    FILESDIR="$PWD/files"
else
    FILESDIR="$PWD/$(dirname $0)/files"
fi

deploy $FILESDIR $HOME
