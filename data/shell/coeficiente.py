# coding: utf-8

import os

file = open("users_que_mais_usaram_o_lcc.txt", 'r')
file2 = open("coeficiente_de_uso_total.csv", 'wb')

content = file.readlines()
d = {}
_min = float(content[1].split()[1])
_max = float(content[len(content)-1].split()[1])

for line in content:
	s = line.split()
	d[s[0]] = float(s[1])/_max
	file2.write(s[0] + ',' + str(d[s[0]]) + '\n')

