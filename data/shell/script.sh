#!/bin/bash

cat acessosTratados.csv | awk -F ';' '{ print " " }' | sort -n | sort -t" "  -k2 | uniq -c | sort -k2 > frequencias_de_uso.csv
