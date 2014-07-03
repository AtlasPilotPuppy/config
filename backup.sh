#!/bin/bash
if [ $# -eq 0 ]
then
    echo -e "\nstarting backup for  $(date)\n">> /home/anant/backup.log
    dpkg --get-selections > ~/Package.list
    sudo cp /etc/apt/sources.list ~/sources.list
    sudo apt-key exportall > ~/Repo.keys
    rsync -arWz --progress --exclude 'Downloads' --exclude 'Music' --exclude 'Videos' --exclude 'Public'  --exclude 'Mail' --exclude '*chromium*' --exclude '*thunderbird*' --exclude '*mozilla*'  --exclude '*cache*' --exclude ".bitcoin" --exclude ".AndroidStudioPreview" --exclude ".atom" --exclude ".dogecoin/" --exclude ".vagrant.d" --exclude ".thumbnails" --exclude ".sbt" --exclude "rvm" 
    --exclude ".config" --exclude ".gradle" --exclude ".local" --exclude ".m2" --exclude ".node" --exclude ".npm" -r /home/anant/ /media/anant/Seagate\ Backup\ Plus\ Drive/Backup/profile
    rsync -arWz --progress  --exclude ".bitcoin" --exclude ".AndroidStudioPreview" --exclude ".gradle" --exclude ".local" --exclude ".m2" --exclude ".atom" --exclude ".node*" --exclude ".npm" --exclude ".vagrant.d" --exclude ".thumbnails" --exclude ".sbt" --exclude "rvm" --exclude ".dogecoin/" --exclude ".config" --exclude 'Downloads' --exclude 'M*' --exclude 'V*' --exclude 'P*' --exclude '*cache*' --exclude '*proton*' -vz -e ssh reddit-vm:~/
    rsync -arWz --progress  -r /home/anant/Downloads/ebooks -vz -e ssh reddit-vm:~/    
    mv /home/anant/git_bak/.git /home/anant/
    git add .emacs.d
    git commit -am "Commit for $(date)"
    git push origin master
    mv /home/anant/.git /home/anant/git_bak/
    echo -e "\nfinishing backup for  $(date)\n">> /home/anant/backup.log
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
