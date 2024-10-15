#!/usr/bin/env nix-shell
#!nix-shell -p python3 -i python3
import shutil
import subprocess

shutil.rmtree("dist", ignore_errors=True)
shutil.copytree("public", "dist")

with open("dist/index.html", "r") as file:
    template = file.read()

output = subprocess.check_output("cargo run", shell=True).decode("utf-8")
result = template.replace("$CONTENT", output)

with open("dist/index.html", "w") as file:
    file.write(result)
