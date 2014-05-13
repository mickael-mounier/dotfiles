#!/bin/sh

# TODO: handle directories for .emacs.d & friends!

if [ x"$(dirname $0)" = x"." ] ; then
    FILESDIR="$PWD/files"
else
    FILESDIR="$PWD/$(dirname $0)/files"
fi

for i in `find $FILESDIR -maxdepth 1 -type f` ; do
    FILENAME=`basename $i`
    if [ -h "$HOME/$FILENAME" ] ; then
        echo "nothing to do for \"$FILENAME\""
        # FIXME: maybe check if we use the right symlink?
    elif [ -f "$HOME/$FILENAME" ] ; then
        echo "WARNING: \"$FILENAME\" already exists, please get rid of it and restart this script"
    else
        echo "linking \"$FILENAME\""
        ln -s "$i" "$HOME/$FILENAME"
    fi
done
