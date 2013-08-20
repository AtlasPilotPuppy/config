A powerful virtualenv tool for Emacs.  See documentation at
https://github.com/porterjamesj/virtualenvwrapper.el

POTENTIAL TODOS:
- Figure out a better way to make M-x shell work than
  advising it.  This could be done if Emacs had pre-
  and post- shell activation hooks.
- Implement the option to have eshell work in a separate
  namespace.  This would be a substantial refactor.
- Add an option for `venv-location' to be an alist.
- Add autocompletion of virtualenvs at the eshell prompt.
- Add ability to create and destroy multiple virtualenvs
  at once.
