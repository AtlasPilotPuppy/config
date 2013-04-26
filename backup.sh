#!/bin/bash
if [ $# -eq 0 ]
then
    dpkg --get-selections > ~/Package.list
    sudo cp /etc/apt/sources.list ~/sources.list
    sudo apt-key exportall > ~/Repo.keys
    rsync -az --progress --exclude 'Downloads' --exclude 'Music' --exclude 'Videos' --exclude 'Public' --exclude 'Pictures' --exclude 'Mail' --exclude '*chromium*' --exclude '*thunderbird*' --exclude '*mozilla*'  --exclude '*cache*' -r /home/anant/ /media/anant/Seagate\ Backup\ Plus\ Drive/Backup/profile
    rsync -a --progress --exclude 'Downloads' --exclude 'M*' --exclude 'V*' --exclude 'P*' --exclude '*cache*' --exclude '*proton*' -r /home/anant/projects -vz -e ssh vps:~/
    mv git_bak/.git .
    git commit -am "Commit for $(date)"
    git push origin master
    mv .git git_bak
else
    if [$1 = 'restore']
    then
	rsync --progress /media/anant/Seagate\ Backup\ Plus\ Drive/Backup/profile /home/`whoami`
	sudo apt-key add ~/Repo.keys
	sudo cp ~/sources.list /etc/apt/sources.list 
	sudo apt-get update
	sudo apt-get install dselect
	sudo dpkg --set-selections < ~/Package.list
	sudo dselect
    fi
fi
