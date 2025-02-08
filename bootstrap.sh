#!/bin/bash

log() {
    echo " [*] $@"
}

die() {
    log "$@" >&2
    exit 1
}

require() {
    which $1 &>/dev/null || die "$1 is not installed"
}

maybe_backup_emacs_user_dir() {
    if [ -d "$HOME/.emacs.d" ]; then
        new_name=".emacs.d.backup_$(date "+%s")"
        log "User emacs directory detected, moving to ~/$new_name"
        mv -v "$HOME/.emacs.d" "$HOME/$new_name" || die "Failed to backup emacs directory. Aborting!"
    fi
}

require git
require emacs

set +xe
git clone https://github.com/sugol-a/coremacs.git "$HOME/.emacs.d"
emacs&
