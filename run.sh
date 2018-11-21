#!/bin/bash

dir=$(pwd)

#Compilando
rm tiger
make clean
make 
make clean

#Corriendo Casos
clear
echo "Deberian Funcionar:"
for i in $(ls "$dir/testcases/Bien" -C1)
do
	echo "$i:"
	./tiger -inter -canon < "$dir/testcases/Bien/$i"
done
echo ""
echo "Deberian Fallar:"
for i in $(ls "$dir/testcases/Mal" -C1)
do
	echo "$i:"
	./tiger -inter -canon < "$dir/testcases/Mal/$i"
done
