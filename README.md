Installation:
---
    pip install pyflakes
    cd ~
    git clone git://github.com/twik/emacs.git .emacs.d
    cd ~/.emacs.d/vendor/auto-complete-1.3.1
    make byte-compile
    cd ~/.emacs.d
    git submodule sync
    git submodule update --init --recursive

