#!/bin/bash
# -*- mode: sh -*-

[[ -f ~/perl5/perlbrew/etc/bashrc ]] || {
    \curl -L http://install.perlbrew.pl | bash
}

# No use now, use $DS3_HOME/bin/ds3.rc
function init-bashrc() {
    [[ -f ~/perl5/perlbrew/etc/bashrc ]] &&
        ! grep  "~/perl5/perlbrew/etc/bashrc" ~/.bash_profile && {
            echo -e "\nsource ~/perl5/perlbrew/etc/bashrc" >> ~/.bash_profile
        }
}
