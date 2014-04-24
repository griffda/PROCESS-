#! /usr/bin/python
"""

	Modifies the PROCESS input file IN.DAT so all the iteration variables are given their values from the output file OUT.DAT. 

   Michael Kovari 9/8/13, based on code by J C Rivas, 16/7/2013
   
	Input files:
		IN.DAT, OUT.DAT in the working directory
		
	Output files:
		All of them in the working directory   
		IN.DAT
		OLD.IN.DAT
		
   Execution procedure:
   	Make sure this file has execution permission
   	Save this file either in the current directory or in your bin directory or wherever you have a path.
   	Execute from the command line: WriteNEW.IN.DAT.py
   	
   Algorithm:
		The final values of the iteration variables in OUT.DAT are set in a block at the end of IN.DAT.
		Any other definitions of these variables are commented out.
		Original IN.DAT is renamed OLD.IN.DAT
		
"""

import sys, os
import fileinput
from subprocess import call
import string
	
"""Reads the final values of the iteration variables in OUT.DAT and saves them to 
	arrays tabla_valores and tabla_vars. """
def readVars():
	bandera=0
	i=0
	for line in fileinput.input("OUT.DAT",inplace=1):
		if "value       change    multiplier  multiplier" in line:
			bandera=1
			i=0
		elif ("residues should be close to zero" in line):
			bandera=0
		elif (bandera==1 and len(line)>10):
			tabla_valores[i]=line[-47:-37]
			tabla_vars[i]=line[-60:-47]
			tabla_vars[i]=tabla_vars[i].replace(' ','')
			i=i+1 
		sys.stdout.write(line)
	"""The previous line seems to be needed to rewrite the line in OUT.DAT."""
	print "%d iteration variables found  \n " %(i)
	print "The first iteration found is %s=%s  \n " %(tabla_vars[0],tabla_valores[0])	
	print "The last iteration found is %s=%s  \n " %(tabla_vars[i-1],tabla_valores[i-1])
	
"""Reads through file, looking for strings in the array searchExp.
	When it finds one, it places two asterisks at the beginning of the line"""
def replaceall(file,searchExp):
	LinesReplaced=0
	for line in fileinput.input(file,inplace=1):
		if "$END" in line:
			line=""
		i=0
		for it in searchExp:
			if it!='0':
				if line[:1]!="*":
					encontrado=find_substring(searchExp[i].lower(),line.lower())
					if (encontrado==True) :
						"""line=replaceExp[i]"""
						line="*" + line
						LinesReplaced=LinesReplaced+1
			i=i+1	
		sys.stdout.write(line)	
		
""" find substring only whole word """
def find_substring(needle, haystack):
	puntuacion = [" ", ",", ";", ".", ":","="]
	index = haystack.find(needle)
	if index == -1:
		return False
	L = index + len(needle)
	if L < len(haystack) and haystack[L] not in puntuacion:
		return False
	return True
    	


"""Main program
----------------------------------------"""
""" Initialise arrays for names and values of the previous solution in OUT.DAT file, to be fed back to the IN.DAT file"""
tabla_vars=["0" for i in range(100)]
tabla_valores=["0" for i in range(100)]
tabla_frases=["0" for k in range(100)]

os.system("cp IN.DAT OLD.IN.DAT ")

"""Reads the final values of the iteration variables in OUT.DAT and saves them to arrays """
readVars()	


"""Comments out the relevant lines in IN.DAT """
replaceall("IN.DAT",tabla_vars)	
"""Adds a new block at the end of IN.DAT"""
k=0
g=open("IN.DAT",'a')
for it in tabla_vars:
	if it != "0":
		NewLine = tabla_vars[k]+" = "+tabla_valores[k]+",\n"
		g.write(NewLine)	
		k=k+1
print "%d Modified lines created, \n " %(k)
g.write("$END")
g.close()
