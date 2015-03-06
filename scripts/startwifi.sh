#!/bin/bash
sudo nmcli r wifi off
sudo rfkill unblock wlan
sudo create_ap -m bridge wlp5s1 enp3s0 lwifi 12345678

