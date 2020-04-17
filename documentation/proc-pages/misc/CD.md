# Electron Cyclotron Current Drive

[PDF Doc](./media/CD-doc.pdf)

A model for electron cyclotron current drive (ECCD) is implemented, known as HARE. The model is described in the paper[^1] and implemented through the source file `hare.f90`.

The switch for the current drive efficiency model `iefrf` is used to include the ECCD model. This is done by selected `iefrf = 11` and including the new input variable `rho ecrh`, which is the normalised minor radius at which electron cyclotron current drive is maximum and which is also available as scan variable 43. The new input and output parameters used by HARE are summarised in table 1.

| output parameter | description |
| --- | --- |
| `rho ecrh` | Normalised minor radius at which ECCD is maximum |
| `fshift` | Frequency shift = wave frequency / cold cyclotron resonance frequency on plasma axis |
| `xf` | Optimum wave frequency for ECCD (GHz) |
| `enpa` | Nparallel |
| `ftherm` | fT = Energy of resonant electrons / $k_BT_e$ at point of max absorption |
| ` fpp` | Ratio of energy of resonance electrons to $k_BT_e$ at pinch point |
| `ampperwatt` | Driven current per unit absorbed power (AW<sup>-1</sup>) |

Table 3.5: *Summary of the variables in PROCESS that relate to the ECCD model HARE.*

There is also a test, triggered by `run tests = 1`. If this test is triggered then one test of the physics in HARE is performed, this is done using the parameters shown in table 2.

| Test parameter | value |
| --- | --- |
| `dens` | 10.5e<sup>19</sup> |
| `bfield` | 5.66 |
| `R0` | 9.072<sup>2</sup> |
| `a` | 2.920e<sup>2</sup> |
| `rho` | 0.1 |
| `te` | 32. |
| `zeff` | 2.e<sup>0</sup> |

Table 3.6: *Summary of the input variables used in HARE runtest subroutine.*

The output in OUT.DAT is either `PASS: ECCD calaculation using "HARE"` or we find the output for a test fail, with an example being

| `FAIL: ECCD calculation using "HARE"` | | |
| --- | --- | --- |
| `fshift` | `1.42523289` | `9.990E+02` |
| `xf` | `225870909081` | `2.259E+11` |
| `enpa` | `0.73312287` | `7.331E-01` |
| `ftherm` | `4.14743724` | `4.147E+00` |
| `fpp` | `8.0121430001` | `8.012E+00` |
| `cdeff` | `-242.42583879` | `-2.424E+02` |
| `ampperwatt` | `-4.2530060e-2` | `-4.253E-02` |

Where here the first column is the output parameter, the second column is the test expected output parameter values and the final third is the resultant output of the HARE calcaultion.

[^1]: E. Poli, M. Muller, H. Zohm, M. Kovari, "Fast evaluation of the current driven by electron cyclotron waves for reactor studies", Physics of Plasmas,