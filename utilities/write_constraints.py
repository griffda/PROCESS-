#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
	Modifies the PROCESS input file IN.DAT to include the constraints marked
        "Y" in write_constraints.conf. 

	Also automatically includes the required iteration variable for each
        constraint, and additional iteration variables selected.

	Blank upper and lower bound equations are also created - these need
        to be completed or deleted.

	Original IN.DAT is renamed OLD.IN.DAT

   Michael Kovari 23/10/13, revised 21/2/14
   
	Input files:
		IN.DAT, write_constraints.conf in the working directory
		
	Output files:
		All of them in the working directory   
		IN.DAT
		OLD.IN.DAT
		
    Execution procedure:
		Make sure this file has execution permission
		Save this file either in the current directory or in your bin
                directory or wherever you have a path.
		Execute from the Linux command line: write_constraints.py	

    PJK 15/05/2014: Updated to python3 using 2to3 tool

    Compatible with PROCESS version 274
"""

import sys, os
import fileinput
from subprocess import call
import string
	
def readConstraints():
	# Reads the constraints defined using "Y" in first column of
        # write_constraints.conf and saves them to the array constraints.
	i=0
	j=0
	for line in fileinput.input("write_constraints.conf"):
		line=line.lower()
		if line[0]=="y":	
			if "icc" in line:
				index=line.find("icc=")
				constraint[i]=line[index+4:index+6]
				i=i+1 
			if "ixc" in line:
				index=line.find("ixc=")				
				IterationVariable[j]=line[index+4:index+6]
				description[j]=line[index+7:index+100]
				j=j+1
	print("%d constraints found  \n " %(i))
	print("The first constraint found is %2s  \n " %(constraint[0]))	
	print("The last constraint found is %2s  \n " %(constraint[i-1]))
	print("%d iteration variables found  \n " %(j))
	return i, j
	
def tidy(file):
	#Reads through file, looking for $END, NEQNS, NVAR, IXC or ICC.
	#Whenever it finds one, it comments it out by placing an asterisk at the beginning of the line
	for line in fileinput.input(file,inplace=1):
		if "$END" in line or "NEQNS" in line or "NVAR" in line or "IXC" in line or "ICC" in line:
			if line[:0] != "*":
				line="*" + line
		sys.stdout.write(line)	

		
#Main program
#----------------------------------------
#Initialise arrays 
constraint=["0" for i in range(100)]
IterationVariable=["0" for i in range(100)]
description=["0" for i in range(100)]
os.system("cp IN.DAT OLD.IN.DAT ")
tidy("IN.DAT")

NumberConstraints, NumberIterationVariables = readConstraints()
NEQNSLine = "NEQNS = " + str(NumberConstraints) + "\n"		
NVARLine = "NVAR = " + str(NumberIterationVariables) + "\n"

#Adds a new constraint statement in IN.DAT
g=open("IN.DAT",'a')
k=0
ConstraintLine= "ICC = "
IterationVariableLine="IXC = "
for c in constraint:
	if c != "0":
		ConstraintLine = ConstraintLine + constraint[k] + ", "
		k=k+1
ConstraintLine = ConstraintLine + " \n"

# Adds a new iteration variable statement in IN.DAT
l=0
BoundLine=""
for iv in IterationVariable:
	if iv != "0":
		IterationVariableLine = IterationVariableLine + IterationVariable[l] + ", "
		BoundLine=BoundLine+"boundl(" + IterationVariable[l] + ") =  " + "\t\t*"+description[l] + "boundu(" + IterationVariable[l] + ") =  " + " \n"
		l=l+1
IterationVariableLine = IterationVariableLine +  " \n"


g.write(NEQNSLine)
g.write(NVARLine)	
g.write(ConstraintLine)	
g.write(IterationVariableLine)	
g.write(BoundLine)
g.write("$END")
g.close()
