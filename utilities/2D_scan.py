#!/usr/bin/env python3
"""

	Executes a  2D parametric study with PROCESS code

   J C Rivas, 16/7/2013
   
	Input files:
		CONFIG.DAT, in the same directory as this file
		IN.DAT, PROCESS input file, in the working directory
		
	Output files:
		All of them in the working directory   
		OUT.DAT, PROCESS output
		PLOT.DAT, PROCESS output
   	ERROR.DAT, Error matrix for all the iterations, 
   					0: no error
   					1: solution not found
   					2: increase in precision recommended
   					3,4,5: the previous codes+3,
                                           the previous message and additionally
                                           there was a change in the EDGEVARS vector
   					
   	EDGETABLE.DAT, Marks the iteration variables with 2 if they are at their
                       upper bound, 1 at the lower or 0 at no bound
   	MATRICES.DAT,  Solution matrices for each output variable
   	EDGEVARS.DAT,  For each iteration, the names of variables which are at
                       their bound value, and the bound (upper or lower)
   	/DATA directory: OUT.DAT and PLOT.DAT from each iteration are stored
                       in this directory if the flag DATA is activated in CONFIG.DAT 
   	
   Execution procedure:
   	cd to the file's path
   	Make sure the file has execution permission
   	execute from the command line: ./2DScan.py
   	
   Algorithm:
		For each iteration, this script changes the IN.DAT file with the
                data in CONFIG.DAT, and calls PROCESS code  
		The data in OUT.DAT from the previous iteration is fed back to
                IN.DAT so PROCESS code starts the calculations from the
                previous solution

   Compatible with PROCESS version ???

   CODE CURRENTLY NOT WORKING AS SEVERAL DIRECTORIES ARE HARDWIRED INCORRECTLY

"""

import sys, os
import fileinput
#from subprocess import call
import subprocess
import string
	
""" LOAD INPUT VARIABLES FROM CONFIG.DAT"""
data=False

lista_inp=[]
lista_outp=[]
flag_input=False
for line in fileinput.input("2D_scan.conf"):
	if "*" in line:
		""" * is a comment """
	elif "N1" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		Vars1=int(auxvar)
	elif "LB1" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		LB1=float(auxvar)
	elif "UB1" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		UB1=float(auxvar)	
	elif "NAME1" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		lista_inp.append(auxvar)
	elif "N2" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		Vars2=int(auxvar)
	elif "LB2" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		LB2=float(auxvar)
	elif "UB2" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		UB2=float(auxvar)	
	elif "NAME2" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		lista_inp.append(auxvar)
	elif "/OUT_VARS" in line:
		flag_input=False
	elif "OUT_VARS" in line:
		flag_input=True
	elif "PATH" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		DIR=auxvar	
	elif "DATA" in line:
		auxvar=line[line.find("=")+1:-1]
		auxvar=auxvar.replace(' ','')	
		data=int(auxvar)		
	if flag_input and len(line)>1 and not "OUT_VARS" in line:
		lista_outp.append(line.strip())
			
valores1=[LB1+j*(UB1-LB1)/(Vars1-1) for j in range(Vars1)]
valores2=[LB2+j*(UB2-LB2)/(Vars2-1) for j in range(Vars2)]					
					
					
""" 2D arrays for output variables"""
tabla=[[0 for j in range(Vars2)] for i in range(Vars1)]
tabla2=[[0 for j in range(Vars2)] for i in range(Vars1)]
""" 2D array for error values"""
tabla_error=[[0 for j in range(Vars2)] for i in range(Vars1)]
""" 1D vectors for names and values of the previous solution in OUT.DAT file,
    to be fed back to the IN.DAT file"""
tabla_vars=["0" for i in range(100)]
tabla_valores=["0" for i in range(100)]

print("AXIS 1")
print("Variable: %s" %(lista_inp[0]))
print(valores1)
print("AXIS 2")
print("Variable: %s" %(lista_inp[1]))
print(valores2)
print("Output Variables:")
print(lista_outp)
print("Working directory: %s" %(DIR))
input("Press <ENTER> to continue")

""" key words to find in error analysis"""
lista_err=["and found a feasible set of parameters.", "but could not find a feasible set of parameters.", "WARNING: Constraint residues are HIGH; consider re-running" ]
"""lista_outp=["Major Radius"]"""
""" pivot variables """
tabla_EDGEVARS=["0" for i in range(100)]
tabla_EDGEVARS_ant=["0" for i in range(100)]


""" find substring only whole word """
def find_substring(needle, haystack):
	puntuacion = [" ", ",", ";", ".", ":"]
	index = haystack.find(needle)
	if index == -1:
		return False
	if index != 0 and haystack[index-1] not in puntuacion:
		return False
	L = index + len(needle)
	if L < len(haystack) and haystack[L] not in puntuacion:
		return False
	return True
    
""" read out.dat vars and changes in.dat """
def readVars():
	bandera=0
	i=0
	for line in fileinput.input("OUT.DAT",inplace=1):
		if "value       change    multiplier  multiplier" in line:
			bandera=1
			i=0
		elif "The following constraint residues should be close to zero" in line:
			bandera=0
		elif (bandera==1 and len(line)>10):
			tabla_valores[i]=line[-47:-37]
			tabla_vars[i]=line[-60:-47]
			tabla_vars[i]=tabla_vars[i].replace(' ','')
			i=i+1 
		sys.stdout.write(line)

""" elements that are different from "0" """
def non_zero_length(a):
	i=0
	for it in a:
		if a[i]!="0":
			i=i+1
		else:
			return i
	return i
"""	remove non"0" duplicates """
def remove_duplicates(a):
	aa=list(set(a)-set([0]))
	i=0
	for it in a:
		a[i]="0"
		i=i+1 	
	i=0
	for it in aa:
		a[i]=aa[i]
		i=i+1 
	
""" compare two lists """
def compare_lists(a,b):
	if non_zero_length(a)!=non_zero_length(b): 
		return False
	else:
		for it in a:
			if it in b:
				""" """
			else:
				return False
		for it in b:
			if it in a:
				""" """
			else:
				return False				
	return True			
	
""" detect direction change """
def check_change_direction():

	tabla_EDGEVARS_ant=list(tabla_EDGEVARS)
	i=0
	for it in tabla_EDGEVARS:
		tabla_EDGEVARS[i]="0"
		i=i+1 
	bandera=0
	i=0		
	for line in fileinput.input("OUT.DAT",inplace=1):
		if "Scan point  " in line:
			bandera=1
			i=0		
			for it in tabla_EDGEVARS:
				it = "0"
		elif "value       change    multiplier  multiplier" in line:
			bandera=0	
		elif (bandera==1 and "Variable" in line):
			tabla_EDGEVARS[i]=line[18:26]
			tabla_EDGEVARS[i]=tabla_EDGEVARS[i].replace(' ','')
			tabla_EDGEVARS[i]=tabla_EDGEVARS[i]+", "+line[39:45]
			i=i+1 							
		sys.stdout.write(line)	
		remove_duplicates(tabla_EDGEVARS)
	return not compare_lists(tabla_EDGEVARS_ant,tabla_EDGEVARS)
		
				
""" change input file	"""
def replaceall(file,searchExp,replaceExp):
	for line in fileinput.input(file,inplace=1):
		i=0
		for it in searchExp:
			if it!='0':
				"""if line[:1]=="*":
					if searchExp in line:
						line=line.replace(searchExp,replaceExp)
				elif ((searchExp[i].lower()+"=" in line.lower()) or (searchExp[i].lower()+" " in line.lower())) :"""
				if line[:1]!="*":
					encontrado=find_substring(searchExp[i].lower(),line.lower())
					if (encontrado==True) :
						line=replaceExp[i]
			i=i+1	
		sys.stdout.write(line)	
		
		
"""os.chdir("/home/jcrivas/process/2013_demos")"""
os.chdir(DIR)

if data:
	try:
		os.stat(DIR+"/DATA")
	except:
		os.mkdir(DIR+"/DATA")

lista_cambio=[0,0]
i=0

f=open("EDGEVARS.DAT",'w')
g=open("EDGETABLE.DAT",'w')
g.write("\t\t")
numvar=0
for line in fileinput.input("IN.DAT",inplace=1):
	if "NVAR" in line:
		numvar=line[line.find("=")+1:-2]
		numvar=numvar.replace(' ','')
	sys.stdout.write(line)
for ii in range(1,int(numvar)+1):
	g.write("\t%d"%(ii))
g.write("\n")
	
for it1 in valores1:
	j=0
	for it2 in valores2:
		print("i:%d, j:%d \n " %(i,j))

		lista_cambio[0]=lista_inp[0]+" = %f,\n" % (it1)
		lista_cambio[1]=lista_inp[1]+" = %f,\n" % (it2)
		replaceall("../2013_demos/IN.DAT",lista_inp,lista_cambio)
		
		""" executes PROCESS code """		
		subprocess.call(["~pknight/process/bin/process20130410"])
		#os.system("~pknight/process/bin/process20130410")
		
		""" copy PLOT.DAT and OUT.DAT to DATA directory"""
		if data:
			destination = "DATA/PLOT.DAT"	+ ".%f" % (it1) + ".%f" % (it2)
			subprocess.call(["cp PLOT.DAT " + destination])
			#os.system("cp PLOT.DAT "+destination)
			destination = "DATA/OUT.DAT"	+ ".%f" % (it1) + ".%f" % (it2)
			subprocess.call(["cp OUT.DAT " + destination])
			#os.system("cp OUT.DAT "+destination)
		
		""" the previous solution is introduced in the next IN.DAT"""
		readVars()
		print("i:%d, j:%d \n " %(i,j))
		tabla_frases=["0" for k in range(100)]
		k=0
		for it in tabla_vars:
			if it != "0":
				tabla_frases[k]= tabla_vars[k]+" = "+tabla_valores[k]+",\n"
				k=k+1
		print("i:%d, j:%d \n " %(i,j))

		replaceall("../2013_demos/IN.DAT",tabla_vars,tabla_frases)
		
		
		print("i:%d, j:%d \n " %(i,j))
		""" read the matrices values """
		for line in fileinput.input("PLOT.DAT",inplace=1):
			if lista_outp[0] in line:
				tabla[i][j]=line[-10:-1]
			if lista_outp[1] in line:
				tabla2[i][j]=line[-10:-1]
			sys.stdout.write(line)	

		for line in fileinput.input("OUT.DAT",inplace=1):
			if lista_outp[0] in line:
				tabla[i][j]=line[-10:-1]
			if lista_outp[1] in line:
				tabla2[i][j]=line[-10:-1]	
			if lista_err[0] in line:
				tabla_error[i][j]=0
			if lista_err[1] in line:
				tabla_error[i][j]=1		
			if lista_err[2] in line:
				tabla_error[i][j]=2							
			sys.stdout.write(line)		
		
		""" for the case we are looking for an iteration variable"""
		k=0
		for it in tabla_vars:
			if it != "0":
				if lista_outp[0] in it:
					tabla[i][j]=tabla_valores[k]
				if lista_outp[1] in line:
					tabla2[i][j]=tabla_valores[k]		
			k=k+1				
					
		if check_change_direction():
			tabla_error[i][j]=tabla_error[i][j]+3
		
		""" table of saturated iteration variables """
		g.write("%d,%d\t%.3f\t%.3f\t"%(i,j,it1,it2))	
		for jt in tabla_vars:	
			found=False
			if jt!="0":
				for it in tabla_EDGEVARS:	
					if find_substring(jt, it) and find_substring("upper", it):
						g.write("2\t")
						found=True
					elif find_substring(jt, it) and find_substring("lower", it):
						g.write("1\t")
						found=True	
				if not found:
					g.write("0\t")	
		g.write("\n")			
		
		
		
		f.write("%d,%d\n"%(i,j))		
		for it in tabla_EDGEVARS:	
			if it!="0":	
				f.write(it+"\n")	
		f.write("\n\n")
											
		j=j+1
	i=i+1
	
f.close()	
g.close()
	
""" write matrix in a file """


f=open("MATRICES.DAT",'w')
i=0
for it1 in valores1:
	j=0
	for it2 in valores2:
		f.write(tabla[i][j] + " ")
		j=j+1
	f.write("\n")
	i=i+1

f.write("\n")
f.write("\n")
f.write("\n")
	
i=0
for it1 in valores1:
	j=0
	for it2 in valores2:
		f.write(tabla2[i][j] + " ")
		j=j+1
	f.write("\n")
	i=i+1
		
f.close()

f=open("ERROR.DAT",'w')
i=0
for it1 in valores1:
	j=0
	for it2 in valores2:
		f.write("%d " %(tabla_error[i][j]))
		j=j+1
	f.write("\n")
	i=i+1

f.close()


