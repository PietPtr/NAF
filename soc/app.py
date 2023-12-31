# sudo `pwd`/.env/bin/python3.10 app.py

import os
from common import ip_hacks

SIM = True
own_ip = ip_hacks.probable_ip()
# TODO: get defined IP from build products
arp_sim = f"arping -c 1 -i tap0 -U -P 172.30.28.50 -S {own_ip}"
arp_naf = ""
arp = arp_sim if SIM else arp_naf
print(os.popen(arp).read())

import socket

# IP = "0.0.0.0" # "192.168.50.111"
# PORT = 50060    

# sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
# sock.bind((IP, PORT))

# print(f"Listening for UDP packets on {IP}:{PORT}")

# while True:
#     data, addr = sock.recvfrom(1024)  # Buffer size is 1024 bytes
#     print(f"Received message: {data} from {addr}")

# import socket

# s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.ntohs(0x0003))
# s.bind(('tap0', 0))

# try:
#     while True:
#         packet, addr = s.recvfrom(65565)
#         print(packet)
# except KeyboardInterrupt:
#     print("\nStopped packet capture.")

from scapy.all import *
import threading
import time
import socket


MAX_PACKETS = 30
pkt_ctr = 0

# TODO: get these from some common config
IP_ADDR = "172.30.28.50" 
UDP_PORT = 50059
    
def packet_callback(packet):
    if IP in packet and UDP in packet:
        if packet[IP].src == IP_ADDR:
            # Do something with the packet
            print(packet.summary())
            payload = packet[UDP].payload
            print(hexdump(payload))
            print(f"length={len(payload)}")

            global pkt_ctr
            pkt_ctr += 1
            if pkt_ctr > MAX_PACKETS:
                exit()

def start_sniffing():
    sniff(iface="tap0", prn=packet_callback)

threading.Thread(target=start_sniffing).start()

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

while True:
    time.sleep(1)

    message = b"Hello, UDP!"
    sock.sendto(message, (IP_ADDR, UDP_PORT))
    print(f"{time.time()} Sent message.")
