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
# Total power emitted by the SOL does not include the ionisation loss
power_loss_integral = y7_mw + y8_mw + y9_mw
spherical_power_load = power_loss_integral / (4*3.142*(xpar*0.5)**2)

ax_1 = fig.add_subplot(421)
ax_1.semilogy(per_column[1], hrad_mw, label="H rad")
ax_1.semilogy(per_column[1], cx_mw, label="CX")
ax_1.semilogy(per_column[1], im_mw, label="Imp rad")
ax_1.semilogy(per_column[1], ion_mw, label="Ionisation")
#ax_1.set_ylim([10, 20000])
ax_1.set_ylim(ymin=1)
ax_1.set_ylabel("power dens. (MWm$^{-3}$)")
ax_1.legend(loc=1, prop={'size': 8})

ax_3 = fig.add_subplot(423, sharex=ax_1)
ax_3.plot(per_column[1], per_column[5], label="$P_{total}$")
ax_3.plot(per_column[1], per_column[4], label="$P_{thermal}$")
#ax_3.set_ylim([0.0, 5000])
ax_3.set_ylim(ymin=0)
ax_3.set_ylabel("pressure (Pa)")
ax_3.legend(loc=4, prop={'size': 12})

ax_5 = fig.add_subplot(425, sharex=ax_1)
ax_5.plot(per_column[1], per_column[2], label="$T_e$")
ax_5.set_ylim([0.0, 14.0])
ax_5.set_ylabel("$T_e$ (eV)", fontsize=14)
ax_5.legend(loc=4, prop={'size': 12})

ax_7 = fig.add_subplot(427, sharex=ax_1)
ax_7.semilogy(per_column[1], per_column[3], label="$n_e$")
ax_7.semilogy(per_column[1], per_column[8], label="$n_0$")
ax_7.semilogy(per_column[1], per_column[7], label="mach")
#ax_7.set_ylim([0.1, 100])
ax_7.set_ylim(ymin=0.1)
ax_7.set_xlim([0.0, 0.015])
ax_7.set_ylabel("$n_e$, $n_0$, mach", fontsize=14)
ax_7.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_7.legend(loc=1, prop={'size': 12})

ax_2 = fig.add_subplot(422)
ax_2.loglog(per_column[1], hrad_mw, label="H rad")
ax_2.loglog(per_column[1], cx_mw, label="CX")
ax_2.loglog(per_column[1], im_mw, label="Imp rad")
ax_2.loglog(per_column[1], ion_mw, label="Ionisation")
#ax_2.set_ylim([1, 20000])
ax_2.set_ylim(ymin=1)
ax_2.legend(loc=1, prop={'size': 8})
ax_2.plot((x_max, x_max), (0.001, 10000), ls='dashed', color="black")

ax_4 = fig.add_subplot(424, sharex=ax_2)
ax_4.semilogx(per_column[1], per_column[5], label="$P_{total}$")
ax_4.semilogx(per_column[1], per_column[4], label="$P_{thermal}$")
ax_4.legend(loc=3, prop={'size': 12})
ax_4.plot((x_max, x_max), (0.0, 4500), ls='dashed', color="black")

ax_6 = fig.add_subplot(426, sharex=ax_2)
ax_6.plot(per_column[1], per_column[2], label="$T_e$")
#ax_6.set_ylim([0.0, 350.0])
ax_6.set_ylim(ymin=0.0)
ax_6.legend(loc=2, prop={'size': 12})
ax_6.plot((x_max, x_max), (0.0, 200), ls='dashed', color="black")

ax_8 = fig.add_subplot(428, sharex=ax_2)
ax_8.loglog(per_column[1], per_column[3], label="$n_e$")
ax_8.loglog(per_column[1], per_column[8], label="$n_0$")
ax_8.loglog(per_column[1], per_column[7], label="mach")
#ax_8.set_ylim([0.1, 10])
ax_8.set_ylim(ymin=0.1)
ax_8.set_xlim([0.014, 110])
ax_8.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_8.legend(loc=3, prop={'size': 12})
ax_8.plot((x_max, x_max), (0.1, 1), ls='dashed', color="black")

# Second page plots: First row
ax_9 = page2.add_subplot(421)
ax_9.plot(per_column[1], power_loss_integral, label="Integrated power emission from SOL")
ax_9.set_xlim([0.0, 0.015])
ymax = power_loss_integral[-1]
ymax = round(ymax/50 + 0.5) * 50
#ax_9.set_ylim([0, ymax])
ax_9.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_9.set_ylabel("(MW)")
ax_9.legend(loc=1, prop={'size': 8})

ax_10 = page2.add_subplot(422)
ax_10.semilogx(per_column[1], power_loss_integral, label="Integrated power emission from SOL\n=radiation + charge exchange")
ax_10.set_xlim([0.014, 110])
#ax_10.set_ylim([0, ymax])
ax_10.set_xlabel("$x\parallel B$ (m)", fontsize=14)
ax_10.legend(loc=4, prop={'size': 8})
ax_10.plot((x_max, x_max), (0.001, 50), ls='dashed', color="black")


plt.show(fig)
plt.show(page2)
with bpdf.PdfPages("1D divertor profiles.pdf") as pdf:
        pdf.savefig(fig)
        pdf.savefig(page2)

