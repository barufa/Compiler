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
mv "$dir/code/tiger" "$dir/tiger"
cp "$dir/code/runtime.c" "$dir/runtime.c"
make clean
cd ..
clear

#Corriendo Casos
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

clear
echo -e "${green}================${nocolor}"
echo -e "${green}${bold}+++GOOD CASES+++${normal}"
echo -e "${green}================${nocolor}"
for i in $(ls "$dir/testcases/Bien" -C1)
do
	file=$(basename $i .tig)
	echo "------------------------------------------"
	echo -e "Programa ${green}${bold}$file${normal}${nocolor}:"
	./tiger "$dir/testcases/Bien/$i"
	./$file <<< "0 1 3 5 7 9 10 12 1024 x 2 4 6 8 11 77 x" #Input only use for merge.tig
	rm -f "$dir/$file" "$dir/$file.s"
	echo "------------------------------------------"
done

rm -f "$dir/runtime.c" "$dir/tiger"
