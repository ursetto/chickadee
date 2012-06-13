#!/bin/sh

CHICKEN=/usr/local/chicken-4.7.0-st
LOGDIR=/var/log/chickadee

exec \
$CHICKEN/bin/chickadee serve \
   -A $LOGDIR/access.log -E $LOGDIR/error.log -D "" -J "" \
   $CHICKEN/share/chicken/chickadee/config-nginx.scm \
#   < /dev/null > /dev/null 2> /dev/null
