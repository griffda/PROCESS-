#!/usr/bin/env python

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as bpdf


f = open("output_divertor.txt", 'r')
lines = f.readlines()
n = len(lines[8:])

# get headings
headings = list(item.strip("\n") for item in lines[7].split(" ") if item != "")
m = len(headings)
# print(headings)

per_row = []
for line in lines[8:]:
    per_row.append([it.strip("\n") for it in line.split(' ') if it != ""])

per_column = list(zip(*per_row))

# Plotting
# 0 - steps
# 1 - x//b
# 2 - te
# 3 - ne
# 4 - Ptherm
# 5 - Ptotal
# 6 - v
# 7 - mach
# 8 - n0
# 9 - power
# 10 - perparea
# 11 - qtot
# 12 - qconv
# 13 - qcond
# 14 - CX
# 15 - Ion
# 16 - Hrad
# 17 - imrad
# 18 - y7
# 19 - y8
# 20 - y9
# 21 - y10
# 22 - He
# 23 - Be
# 24 - C
# 25 - N
# 26 - O
# 27 - Ne
# 28 - Si
# 29 - Ar
# 30 - Fe
# 31 - Ni
# 32 - Kr
# 33 - Xe
# 34 - W
# 35 - n01/1e20
# 36 - n02/1e20
# 37 - nv24
# 38 - v

# First page
fig = plt.figure(figsize=(12, 9))
plt.subplots_adjust(hspace=0.20)

# Second page plots
page2 = plt.figure(figsize=(12, 9))

# find max x||b
x_max = per_column[1][-1]

# Create numpy array of connection length
xpar = np.array([float(x) for x in per_column[1]])
xpar[0] = 99999

# convert to MW
hrad_mw = [float(x)/1e6 for x in per_column[16]]
cx_mw = [float(x)/1e6 for x in per_column[14]]
im_mw = [float(x)/1e6 for x in per_column[17]]
ion_mw = [float(x)/1e6 for x in per_column[15]]
# Power loss integrals are already in MW
y7_mw = np.array([float(x) for x in per_column[18]])
y8_mw = np.array([float(x) for x in per_column[19]])
y9_mw = np.array([float(x) for x in per_column[20]])
y10_mw = np.array([float(x) for x in per_column[21]])

He_mw = [float(x)/1e6 for x in per_column[22]]
Be_mw = [float(x)/1e6 for x in per_column[23]]
C_mw = [float(x)/1e6 for x in per_column[24]]
N_mw = [float(x)/1e6 for x in per_column[25]]
O_mw = [float(x)/1e6 for x in per_column[26]]
Ne_mw = [float(x)/1e6 for x in per_column[27]]
Si_mw = [float(x)/1e6 for x in per_column[28]]
Ar_mw = [float(x)/1e6 for x in per_column[29]]
Fe_mw = [float(x)/1e6 for x in per_column[30]]
Ni_mw = [float(x)/1e6 for x in per_column[31]]
Kr_mw = [float(x)/1e6 for x in per_column[32]]
Xe_mw = [float(x)/1e6 for x in per_column[33]]
W_mw = [float(x)/1e6 for x in per_column[34]]
n01 = [float(x) for x in per_column[35]]   #/1e20 m-3
n02 = [float(x) for x in per_column[36]]   #/1e20 m-3
# Particle flux and velocity are both negative in the code
nv = [-float(x) for x in per_column[37]]    #/1e24 sm-2
v = [-float(x) for x in per_column[38]]     # ms-1

# Total power emitted by the SOL does not include the ionisation loss
power_loss_integral = y7_mw + y8_mw + y9_mw
spherical_power_load = power_loss_integral / (4*3.142*(xpar*0.5)**2)

# Left hand plots (odd numbers)
ax_1 = fig.add_subplot(421)
ax_1.semilogy(per_column[1], hrad_mw, label="H rad")
ax_1.semilogy(per_column[1], cx_mw, label="CX")
ax_1.semilogy(per_column[1], im_mw, label="Imp rad")
ax_1.semilogy(per_column[1], ion_mw, label="Ionisation")
ax_1.set_xlim([0.0, 0.015])
ax_1.set_ylim(ymin=1)
#ax_1.set_ylim([10, 20000])
ax_1.set_ylabel("power dens. (MWm$^{-3}$)")
ax_1.legend(loc=1, prop={'size': 8})

ax_3 = fig.add_subplot(423)
ax_3.plot(per_column[1], per_column[5], label="$P_{total}$")
ax_3.plot(per_column[1], per_column[4], label="$P_{thermal}$")
#ax_3.set_ylim([0.0, 5000])
ax_3.set_xlim([0.0, 0.015])
ax_3.set_ylim(ymin=0)
ax_3.set_ylabel("pressure (Pa)")
ax_3.legend(loc=4, prop={'size': 12})

ax_5 = fig.add_subplot(425)
ax_5.set_ylim([0.0, 14.0])
ax_5.plot(per_column[1], per_column[2], label="$T_e$")
ax_5.set_xlim([0.0, 0.015])
ax_5.set_ylabel("$T_e$ (eV)", fontsize=14)
ax_5.legend(loc=4, prop={'size': 12})

ax_7 = fig.add_subplot(427)
ax_7.semilogy(per_column[1], per_column[3], label="$n_e$")
ax_7.semilogy(per_column[1], per_column[8], label="$n_0$")
ax_7.semilogy(per_column[1], n01, label="$n_01/1e20 m-3$", ls='dashed')
ax_7.semilogy(per_column[1], n02, label="$n_02/1e20 m-3$", ls='dashed')
ax_7.semilogy(per_column[1], per_column[7], label="mach")
#ax_7.set_ylim([0.1, 100])
ax_7.set_xlim([0.0, 0.015])
ax_7.set_ylim(ymin=0.1)
ax_7.set_ylabel("$n_e$, $n_0$, mach", fontsize=14)
ax_7.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_7.legend(loc=1, prop={'size': 8})

# Right
ax_2 = fig.add_subplot(422)
ax_2.loglog(per_column[1], hrad_mw, label="H rad")
ax_2.loglog(per_column[1], cx_mw, label="CX")
ax_2.loglog(per_column[1], im_mw, label="Imp rad")
ax_2.loglog(per_column[1], ion_mw, label="Ionisation")
ax_2.set_xlim([0.014, 200])
ax_2.set_ylim(ymin=1)
ax_2.legend(loc=1, prop={'size': 8})
ax_2.plot((x_max, x_max), (0.001, 10000), ls='dashed', color="black")

ax_4 = fig.add_subplot(424)
ax_4.semilogx(per_column[1], per_column[5], label="$P_{total}$")
ax_4.semilogx(per_column[1], per_column[4], label="$P_{thermal}$")
ax_4.set_xlim([0.014, 200])
ax_4.legend(loc=3, prop={'size': 12})
ax_4.plot((x_max, x_max), (0.0, 4500), ls='dashed', color="black")

ax_6 = fig.add_subplot(426)
ax_6.semilogx(per_column[1], per_column[2], label="$T_e$")
ax_6.set_xlim([0.014, 200])
#ax_6.set_ylim([0.0, 350.0])
ax_6.set_ylim(ymin=0.0)
ax_6.legend(loc=2, prop={'size': 12})
ax_6.plot((x_max, x_max), (0.0, 200), ls='dashed', color="black")

ax_8 = fig.add_subplot(428)
ax_8.loglog(per_column[1], per_column[3], label="$n_e$")
ax_8.loglog(per_column[1], per_column[8], label="$n_0$")
ax_8.loglog(per_column[1], n01, label="$n_01/1e20 m-3$", ls='dashed')
ax_8.loglog(per_column[1], n02, label="$n_02/1e20 m-3$", ls='dashed')
ax_8.loglog(per_column[1], per_column[7], label="mach")
#ax_8.set_ylim([0.1, 10])
ax_8.set_xlim([0.014, 200])
ax_8.set_ylim(ymin=0.1)
ax_8.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_8.legend(loc=1, prop={'size': 8})
ax_8.plot((x_max, x_max), (0.1, 1), ls='dashed', color="black")

# Second page plots
# Row 1
# Left
ax_9 = page2.add_subplot(421)
ax_9.plot(per_column[1], power_loss_integral, label="Integrated power emission from SOL")
ax_9.set_xlim([0.0, 0.015])
ymax = power_loss_integral[-1]
ymax = round(ymax/50 + 0.5) * 50
#ax_9.set_ylim([0, ymax])
ax_9.set_ylabel("(MW)")
ax_9.legend(loc=1, prop={'size': 8})

# Right
ax_10 = page2.add_subplot(422)
ax_10.semilogx(per_column[1], power_loss_integral, label="Integrated power emission from SOL\n=radiation + charge exchange")
ax_10.set_xlim([0.014, 200])
#ax_10.set_ylim([0, ymax])
ax_10.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_10.legend(loc=4, prop={'size': 8})
ax_10.plot((x_max, x_max), (0.001, 50), ls='dashed', color="black")

# Row 2
# Left
ax_11 = page2.add_subplot(423, sharex=ax_9)
ax_11.set_xlim([0.0, 0.015])
ax_11.set_ylim([1, 1000])
ax_11.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_11.set_ylabel("power dens. (MWm$^{-3}$)")
if max(He_mw)>0.001:
    ax_11.semilogy(per_column[1], He_mw, label="He")
if max(Be_mw)>0.001:
    ax_11.semilogy(per_column[1], Be_mw, label="Be")
if max(C_mw)>0.001:
    ax_11.semilogy(per_column[1], C_mw, label="C")
if max(N_mw)>0.001:
    ax_11.semilogy(per_column[1], N_mw, label="N")
if max(O_mw)>0.001:
    ax_11.semilogy(per_column[1], O_mw, label="O")
if max(Ne_mw)>0.001:
    ax_11.semilogy(per_column[1], Ne_mw, label="Ne")
if max(Si_mw)>0.001:
    ax_11.semilogy(per_column[1], Si_mw, label="Si")
if max(Ar_mw)>0.001:
    ax_11.semilogy(per_column[1], Ar_mw, label="Ar")
if max(Fe_mw)>0.001:
    ax_11.semilogy(per_column[1], Fe_mw, label="Fe")
if max(Ni_mw)>0.001:
    ax_11.semilogy(per_column[1], Ni_mw, label="Ni")
if max(Kr_mw)>0.001:
    ax_11.semilogy(per_column[1], Kr_mw, label="Kr")
if max(Xe_mw)>0.001:
    ax_11.semilogy(per_column[1], Xe_mw, label="Xe")
if max(W_mw)>0.001:
    ax_11.semilogy(per_column[1], W_mw, label="W")
ax_11.legend(loc=1, prop={'size': 8})
ax_11.plot((x_max, x_max), (1, 10000), ls='dashed', color="black")

# Row 2 Right
ax_12 = page2.add_subplot(424, sharex=ax_10)
ax_12.set_xlim([0.014, 200])
ax_12.set_ylim([1, 1000])
ax_12.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_12.set_ylabel("power dens. (MWm$^{-3}$)")
if max(He_mw)>0.001:
    ax_12.semilogy(per_column[1], He_mw, label="He")
if max(Be_mw)>0.001:
    ax_12.semilogy(per_column[1], Be_mw, label="Be")
if max(C_mw)>0.001:
    ax_12.semilogy(per_column[1], C_mw, label="C")
if max(N_mw)>0.001:
    ax_12.semilogy(per_column[1], N_mw, label="N")
if max(O_mw)>0.001:
    ax_12.semilogy(per_column[1], O_mw, label="O")
if max(Ne_mw)>0.001:
    ax_12.semilogy(per_column[1], Ne_mw, label="Ne")
if max(Si_mw)>0.001:
    ax_12.semilogy(per_column[1], Si_mw, label="Si")
if max(Ar_mw)>0.001:
    ax_12.semilogy(per_column[1], Ar_mw, label="Ar")
if max(Fe_mw)>0.001:
    ax_12.semilogy(per_column[1], Fe_mw, label="Fe")
if max(Ni_mw)>0.001:
    ax_12.semilogy(per_column[1], Ni_mw, label="Ni")
if max(Kr_mw)>0.001:
    ax_12.semilogy(per_column[1], Kr_mw, label="Kr")
if max(Xe_mw)>0.001:
    ax_12.semilogy(per_column[1], Xe_mw, label="Xe")
if max(W_mw)>0.001:
    ax_12.semilogy(per_column[1], W_mw, label="W")
ax_12.legend(loc=1, prop={'size': 8})
ax_12.plot((x_max, x_max), (1, 10000), ls='dashed', color="black")

# Row 3 Left
ax_13 = page2.add_subplot(425)
ax_13.plot(per_column[1], nv, label="Plasma flux [1e24 s-1 m-2]")
ax_13.set_xlim([0.0, 0.015])
#ymax = v[-1]
#ymax = round(ymax/50 + 0.5) * 50
#ax_13.set_ylim([0, ymax])
ax_13.legend(loc=1, prop={'size': 8})

# Row 3 Right
ax_14 = page2.add_subplot(426)
ax_14.semilogx(per_column[1], nv, label="Plasma flux [1e24 s-1 m-2]")
ax_14.set_xlim([0.014, 200])
ax_14.set_xlabel("$x\parallel B$ (m)", fontsize=14)
#ax_14.set_ylim([0, ymax])
ax_14.legend(loc=1, prop={'size': 8})
ax_14.plot((x_max, x_max), (0.001, 50), ls='dashed', color="black")

# Row 4 Left
ax_15 = page2.add_subplot(427)
ax_15.plot(per_column[1], v, label="Plasma speed [ms-1]")
ax_15.set_xlim([0.0, 0.015])
ax_15.legend(loc=1, prop={'size': 8})

# Row 4 Right
ax_16 = page2.add_subplot(428)
ax_16.semilogx(per_column[1], v, label="Plasma speed [ms-1]")
ax_16.set_xlim([0.014, 200])
ax_16.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_16.legend(loc=1, prop={'size': 8})
ax_16.plot((x_max, x_max), (0.001, 15000), ls='dashed', color="black")


plt.show(fig)
plt.show(page2)
with bpdf.PdfPages("1D divertor profiles.pdf") as pdf:
        pdf.savefig(fig)
        pdf.savefig(page2)
