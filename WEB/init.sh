#!/bin/bash

cd "$( realpath "$(dirname "$0")" )" || exit 1
FROMHERE="$( pwd )"

rm -rf /home/box/web/*
mkdir -p /home/box/web/public
mkdir -p /home/box/web/public/img
mkdir -p /home/box/web/public/css
mkdir -p /home/box/web/public/js
mkdir -p /home/box/web/uploads
mkdir -p /home/box/web/etc

if [ -f /etc/nginx/sites-enabled/default ]; then
    unlink /etc/nginx/sites-enabled/default
fi

ln -fs "$FROMHERE/nginx.conf" /etc/nginx/sites-enabled/test.conf
systemctl restart nginx.service

cp "hello.py" /home/box/web/hello.py

gunicorn -b 0.0.0.0:8080 -w 2 "hello:application"
