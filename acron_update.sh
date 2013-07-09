#!/bin/bash
echo -e "\nstarting backup for  $(date)\n">> /home/anant/anacron.log
unison -batch -ignorearchives ssh://anant@vps/ebooks /home/anant/Downloads/ebooks &>> /home/anant/anacron.log

echo -e "completed backup for  $(date)\n">> /home/anant/anacron.log
