import subprocess

def build():
    process = subprocess.Popen("./build_cla.sh")
    process.communicate()

    if process.returncode != 0:
        exit(process.returncode)