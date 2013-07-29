INTRODUCTION

You need this if you are writing Ubuntu Phone apps with Emacs.


USAGE

Type `M-x package-install qml-mode`, and add this to your init file:

    (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
    (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
