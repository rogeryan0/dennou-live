#!/bin/sh
LANG=ja_JP.UTF-8 ; export LANG
EDITOR=vim; export EDITOR
# XMODIFIERS=@im=uim ; export XMODIFIERS
xrdb -merge .Xresources
x-session-manager
