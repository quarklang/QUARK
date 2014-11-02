#!/usr/bin/env bash

sudo apt-get -y update
apt-get install -y python-software-properties
add-apt-repository ppa:avsm/ppa && apt-get update
apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam
rm -rf /var/www
ln -fs /vagrant /var/www
