#!/bin/bash

dir=$(pwd)
bold=$(tput bold)
normal=$(tput sgr0)
red='\033[0;31m'
green='\033[0;32m'
nocolor='\033[0m'


#Compilando
cd code
make clean
make 
make clean
cd ..
mv $dir/code/tiger $dir/tiger
clear

#Corriendo Casos
clear
echo -e "${green}================${nocolor}"
echo -e "${green}${bold}+++GOOD CASES+++${normal}"
echo -e "${green}================${nocolor}"
for i in $(ls "$dir/testcases/Bien" -C1)
do
	echo "------------------------------------------"
	echo -e "Programa ${green}${bold}$i${normal}${nocolor}:"
	./tiger -inter -canon < "$dir/testcases/Bien/$i"
	echo "------------------------------------------"
done

echo -e "${red}===============${nocolor}"
echo -e "${red}${bold}+++BAD CASES+++${normal}"
echo -e "${red}===============${nocolor}"
for i in $(ls "$dir/testcases/Mal" -C1)
do
	echo "------------------------------------------"
	echo -e "Programa ${red}${bold}$i${normal}${nocolor}:"
	./tiger -inter -canon < "$dir/testcases/Mal/$i"
	echo "------------------------------------------"
done
