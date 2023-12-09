```
                        _   _____    ______
                       / | / /   |  / ____/
                      /  |/ / /| | / /_    
                     / /|  / ___ |/ __/    
                    /_/ |_/_/  |_/_/       
```
# Intro
Platform to expose UDP packets from the ethernet port of a ColorLight board to a custom core written in Clash (or anything else)

# Building
Initial set up:
```
cd soc/
wget https://raw.githubusercontent.com/enjoy-digital/litex/master/litex_setup.py
chmod +x litex_setup.py
virtualenv .env
source .env/bin/activate
./litex_setup.py init install
```
Clone, make, and install Yosys, prjtrellis, NextPNR, and the clash-compiler according to their respective install instructions
