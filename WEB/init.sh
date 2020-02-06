#!/bin/sh

rm -rf /home/box/web/*
mkdir -p /home/box/web/public
mkdir -p /home/box/web/public/img
mkdir -p /home/box/web/public/css
mkdir -p /home/box/web/public/js
mkdir -p /home/box/web/uploads
mkdir -p /home/box/web/etc

BASEDIR=$(dirname "$0")
unlink /etc/nginx/sites-enabled/default
ln -fs "$(realpath "$BASEDIR")/nginx.conf"  /etc/nginx/sites-enabled/test.conf
/etc/init.d/nginx restart