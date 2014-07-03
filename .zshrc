# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git python pip command-not-found git-extras virtualenvwrapper urltools zsh-syntax-highlighting cpv history-substring-search cp copyfile copydir git-prompt)

PROMPT='%B%m%~%b$(git_super_status) %# '

export ALTERNATE_EDITOR=""

source $ZSH/oh-my-zsh.sh

alias emacs="emacs -nw"
alias emacsclient="emacsclient -nw"

alias ls="ls --color"

alias ack="ack-grep --color"

alias ackpy="ack-grep --color --py"

alias diff="colordiff"
# Customize to your needs...

source /usr/local/bin/virtualenvwrapper.sh

mkcd() { mkdir -p "$@" && eval cd "\"\$$#\""; }

mktags() {  ctags-exuberant -e -R --languages=Python,HTML,JavaScript --exclude="__init__.py"}
##
export BC_ENV_ARGS=~/.bcrc

EDITOR="vim"

## random_fun_fact

fun_fact(){ elinks -dump randomfunfacts\.com | sed -n '/^| /p' |sed 's/|//g'}

excuse(){elinks -dump developerexcuses.com| grep '\[2\]'| sed -e 's/^[ \t]*//' -e 's/\[2\]//g' }

alias cdp="cd ~/projects/proton"

alias cdb="cd ~/projects/BackcountrySkiBlog"

#less pips source highlight

export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"

export LESS=' -R'

export HADOOP_HOME='/usr/local/hadoop'

calc(){ echo "scale=4;$@" | bc;}

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

#EC2 stuf
export AWS_ACCESS_KEY=AKIAJJU6SHFSTTLKIPNA
export AWS_SECRET_KEY=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
export JAVA_HOME=/usr/lib/jvm/jdk1.7.0_51/
export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.6.13.0
export PATH=$PATH:$EC2_HOME/bin 
export EC2_URL=https://search-reddit-cloudsearch-7xlvxu43zunfqclx55y5ycpj6a.us-west-2.cloudsearch.amazonaws.com


PATH=$PATH:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:$HADOOP_HOME/bin:$JAVA_HOME/bin/java
PATH=$PATH:/home/anant/projects/phantomjs-1.9.7-linux-x86_64/bin/phantomjs

export HADOOP_COMMON_LIB_NATIVE_DIR=$HADOOP_HOME/lib/native
export HADOOP_OPTS="-Djava.library.path=$HADOOP_HOME/lib"

export HBASE_HOME=/usr/lib/hbase/hbase-0.94.8
