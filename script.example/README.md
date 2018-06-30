# Register service

````
sudo systemctl daemon-reload
sudo systemctl enable {{path-to-kknotify-server}}/script/kknotify-server.service
````

# Service commands

````
sudo systemctl start kknotify-server
sudo systemctl status kknotify-server
sudo systemctl stop kknotify-server
````

# Read logs

````
sudo journalctl -f -u kknotify-server
````
