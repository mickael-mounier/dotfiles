#!/bin/sh

deploy() {
    local SRC=$1
    local DST=$2

    for i in `find $SRC -maxdepth 1 -type f` ; do
        DSTFILE=$DST/`basename $i`
        if [ -h "$DSTFILE" ] ; then
            echo "nothing to do for $DSTFILE"
            # FIXME: check if we use the right symlink
        elif [ -f "$DSTFILE" ] ; then
            echo "warning: $DSTFILE already exists, please get rid of it and restart this script"
        else
            echo "linking $DSTFILE"
            ln -s "$i" "$DSTFILE"
        fi
    done

    for i in `find $SRC -mindepth 1 -maxdepth 1 -type d` ; do
        DIRNAME=`basename $i`
        test -d $DST/$DIRNAME || mkdir $DST/$DIRNAME
        deploy $SRC/$DIRNAME $DST/$DIRNAME
    done
}

if [ x"$(dirname $0)" = x"." ] ; then
    FILESDIR="$PWD/files"
else
    FILESDIR="$PWD/$(dirname $0)/files"
fi

deploy $FILESDIR $HOME
