#!/usr/bin/env bash

sudo apt-get -y update
apt-get install -y python-software-properties
add-apt-repository ppa:avsm/ppa && apt-get update
apt-get install -y ocaml ocaml-native-compilers camlp4-extra opam
apt-get install -y vim
apt-get install gcc-4.8 g++-4.8 -y
rm /usr/bin/gcc
rm /usr/bin/g++
ln -s /usr/bin/gcc-4.8 /usr/bin/gcc
ln -s /usr/bin/g++-4.8 /usr/bin/g++
rm -rf /var/www
ln -fs /vagrant /var/www
cd /vagrant/
