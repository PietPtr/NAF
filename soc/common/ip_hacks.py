import os

def probable_ip():
    ip_addr = os.popen("ip -br addr | awk 'NR==2{print $3}'").read()
    print(f"found ip address: {ip_addr}")
    return ip_addr.split('/')[0]