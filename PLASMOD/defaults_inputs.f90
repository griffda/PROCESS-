!INPUTS with typical default values

                          num%dtminmin=1.0d-9 !1.0d-9
                          num%dtminmax=0.001
                          num%dtmaxmax=100.0
                          num%tolopt=0.0001
                          num%etol=100.0d0
                          num%etol0=100.d0
                          num%etolm=100.0d0
                          num%tolmax=0.1d0
                          num%biggest=huge(kind(1.0d0))
																						    num%tol=0.00001d0 !tolerance to be reached, in % variation at each time step
                          num%dtmin=0.001d0 !min time step
                          num%dtmax=0.5d0 !max time step
                          num%dt=0.01d0 !time step
                          num%dtinc=2.d0 !decrease of dt
                          num%Ainc=1.1d0 !increase of dt
                          num%test=100000. !max iteration number
                          num%tolmin=10.1d0 ! multiplier of etolm that should not be overcome
                          num%eopt=0.1d0 !exponent of jipperdo
                          num%dtmaxmin=0.1d0 !exponent of jipperdo2
                          num%capA=0.1d0 !first radial grid point
                          num%maxA=0.d0 !diagz 0 or 1
                          num%dgy=1.e-5 !Newton differential
                          num%i_modeltype=1 !1 - simple gyrobohm scaling
                          num%i_equiltype=1 !1 - EMEQ, solve equilibrium with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
                          num%nx=41        !number of interpolated grid points
                          num%nxt=5 !number of reduced grid points
                          num%nchannels=3  !leave this at 3
                          num%ipedestal=2 !1 - fixed temperature pedestal. 2 - Sareelma scaling
																										num%i_impmodel=1 !impurity model: 0 - fixed concentration, 1 - concentration fixed at pedestal top, then fixed density.
																										
    geom%A = 3.1d0  !aspect ratio
    geom%R = 9.072d0  ! major radius in m
    geom%bt = 5.667 !magnetic field
    geom%k = 1.7513d0 !edge elongation
    geom%d = 0.383d0 !edge triangularity
    geom%k95 = 1.65d0 !edge elongation
    geom%d95 = 0.333d0 !edge triangularity
    geom%Ip =  19.6 !9.19727561008985 !19.6 !plasma current in MA : USED if equiltype=2, q95 is used if equiltype=1
				geom%q95 = 3.5 !safety factor. 
				geom%counter=0.

    geom%Aold = geom%A  !aspect ratio
    geom%Rold = geom%R  ! major radius in m
    geom%kold = geom%k !edge elongation
    geom%dold = geom%d !edge triangularity
    geom%Ipold = geom%ip !plasma current in MA : USED if equiltype=2, q95 is used if equiltype=1
    geom%btold = geom%bt !magnetic field

    comp%globtau(1) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(2) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(3) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(4) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(5) = 1. !tauparticle/tauE for D, T, He, Xe, Ar

    comp%psepplh_inf = 1.1 !Psep/PLH if below this, use nbi
    comp%psepplh_sup = 1.2d0 !Psep/PLH if above this, use Xe
    comp%psepb_q95AR = 1000.d0 !Psep B/qaR max value
    comp%psep_r = 10000. !Psep/R max value
    comp%qdivt = 0. !divertor heat flux in MW/m^2, if 0, dont use SOL model

    comp%car = 0. !argon concentration, used if qdivt=0.
    comp%c_car = 10. !compression factor between div and core: e.g. 10 means there is 10 more Argon concentration in the divertor than in the core
    comp%cxe = 0. !xenon concentration, if negative uses Psepplh as criterion
    comp%che = 0.08 !helium concentration, used if globtau(3)=0.

    comp%fuelmix = 0.5d0 !fuel mix

    comp%pradpos = 0.d0 ! position after which radiation is counted 0. for tau and other global quantities, i.e. position after which radiation is "edge"

				inp0%f_gw=0.9 !pedestal top greenwald fraction
				inp0%f_gws=0.5 !separatrix greenwald fraction

    ped%teped=5.5  !pedestal top temperature
    ped%tesep=0.3  !separatrix temperature
    ped%rho_t=0.96 !pedestal top position T
    ped%rho_n=0.96 !pedestal top position n

				inp0%Hfac_inp=1. !input H factor, if 0., this is not used. This is radiation corrected H factor

				inp0%nbcdeff=5. !CD = this * PCD   units: m*MA/MW (MA/m^2 * m^3/MW)
				inp0%eccdeff=0.3 !CD = this * PCD * TE/NE !not used for now
				inp0%pheatmax=200. !max allowed power for heating+CD+fusion control
				inp0%q_control=50. !minimal power required for control

				inp0%V_loop=-1.e6 !target loop voltage. If lower than -1.e5, dont use this
				inp0%f_ni=0. !required fraction of non inductive current, if 0, dont use CD
				inp0%pfus=0. !if 0., not used (otherwise it would be controlled with Pauxheat)

				inp0%pech=0.d0 !ech power !not used for now
				inp0%pnbi=0.d0 !nbi power
				inp0%qheat=0.d0 !nbi power
				inp0%qcd=0.d0 !nbi power
				inp0%qfus=0.d0 !nbi power

				inp0%spellet=0.d0 !pellet mass in particles of D in 10^19
				inp0%fpellet=0.5d0 !pellet frequency in Hz

				inp0%qnbi_psepfac=50. !dqnbi/d(1-Psep/PLH)
				inp0%cxe_psepfac=1.e-5 !dcxe/d(1-Psep/PLH)
				inp0%car_qdivt=1.e-6 !dcar/d(qdivt)





! actual values for testing
																						    num%tol=0.00001d0 !tolerance to be reached, in % variation at each time step
                          num%dtmin=0.01d0 !min time step
                          num%dtmax=0.1d0 !max time step
                          num%dt=0.01d0 !time step
                          num%dtinc=2.d0 !decrease of dt
                          num%Ainc=1.1d0 !increase of dt
                          num%test=100000. !max iteration number
                          num%tolmin=10.1d0 ! multiplier of etolm that should not be overcome
                          num%eopt=0.1d0 !exponent of jipperdo
                          num%dtmaxmin=0.1d0 !exponent of jipperdo2
                          num%capA=0.1d0 !first radial grid point
                          num%maxA=0.d0 !diagz 0 or 1
                          num%dgy=1.e-5 !Newton differential
                          num%i_modeltype=1 !1 - simple gyrobohm scaling
                          num%i_equiltype=1 !1 - EMEQ, solve equilibrium with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
                          num%nx=51        !number of interpolated grid points
                          num%nxt=11 !number of reduced grid points
                          num%nchannels=3  !leave this at 3
                          num%ipedestal=2 !1 - fixed temperature pedestal. 2 - Sareelma scaling
																										num%i_impmodel=1 !impurity model: 0 - fixed concentration, 1 - concentration fixed at pedestal top, then fixed density.

!geometry
    geom%A = 3.1d0  !aspect ratio
    geom%R = 9.002d0  ! major radius in m
    geom%bt = 5.85 !magnetic field
    geom%k = 1.7513d0 !edge elongation
    geom%d = 0.383d0 !edge triangularity
    geom%k95 = 1.65d0 !edge elongation
    geom%d95 = 0.333d0 !edge triangularity
    geom%Ip =  17.75 !9.19727561008985 !19.6 !plasma current in MA : USED if equiltype=2, q95 is used if equiltype=1
    geom%q95 = 3.88 !safety factor. 
	geom%counter=0.

    comp%globtau(1) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(2) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(3) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(4) = 5. !tauparticle/tauE for D, T, He, Xe, Ar
    comp%globtau(5) = 1. !tauparticle/tauE for D, T, He, Xe, Ar

    comp%car = 0. !argon concentration, used if qdivt=0.
    comp%c_car = 100. !compression factor between div and core: e.g. 10 means there is 10 more Argon concentration in the divertor than in the core
    comp%cxe = 0. !xenon concentration, if negative uses Psepplh as criterion
    comp%che = 0. !helium concentration, used if globtau(3)=0.

    comp%fuelmix = 0.5d0 !fuel mix

    comp%pradpos = 0.75d0 ! position after which radiation is counted 0. for tau and other global quantities, i.e. position after which radiation is "edge"
    comp%pradfrac = 1.d0 ! position after which radiation is counted 0. for tau and other global quantities, i.e. position after which radiation is "edge"

				inp0%f_gw=0.905 !pedestal top greenwald fraction
				inp0%f_gws=0.5 !separatrix greenwald fraction

    ped%teped=5.5  !pedestal top temperature
    ped%tesep=0.3  !separatrix temperature
    ped%rho_t=0.96 !pedestal top position T
    ped%rho_n=0.96 !pedestal top position n


				inp0%nbcdeff=2.6 !CD = this * PCD   units: m*MA/MW (MA/m^2 * m^3/MW)
				inp0%eccdeff=0.3 !CD = this * PCD * TE/NE !not used for now
				inp0%pheatmax=200. !max allowed power for heating+CD+fusion control
				inp0%pech=0.d0 !ech power !not used for now
				inp0%pnbi=0.d0 !nbi power
				inp0%qheat=0.d0 !nbi power
				inp0%qcd=0.d0 !nbi power
				inp0%qfus=0.d0 !nbi power
				inp0%spellet=0.d0 !pellet mass in particles of D in 10^19
				inp0%fpellet=0.5d0 !pellet frequency in Hz
				inp0%q_control=50.d0 !minimal power required for control


!constraints
				inp0%V_loop=-1.e6 !target loop voltage. If lower than -1.e5, dont use this
				inp0%Hfac_inp=1.1d0 !input H factor, if 0., this is not used. This is radiation corrected H factor
				inp0%f_ni=0. !required fraction of non inductive current, if 0, dont use CD
				inp0%pfus=0. !if 0., not used (otherwise it would be controlled with Pauxheat)
    comp%psepplh_inf = 1.1 !Psep/PLH if below this, use nbi
    comp%psepplh_sup = 1.2d0 !Psep/PLH if above this, use Xe
    comp%psepb_q95AR = 9.4d0 !Psep B/qaR max value
    comp%psep_r = 19.d0 !Psep/R max value
    comp%qdivt = 0. !divertor heat flux in MW/m^2, if 0, dont use SOL model


!derivatives
				inp0%qnbi_psepfac=50. !dqnbi/d(1-Psep/PLH)
				inp0%cxe_psepfac=1.e-4 !dcxe/d(1-Psep/PLH)
				inp0%car_qdivt=1.e-4 !dcar/d(qdivt)

!deposition locations
				inp0%x_heat(1)=0. !nbi
				inp0%x_heat(2)=0. !ech
				inp0%x_cd(1)=0. !nbi
				inp0%x_cd(2)=0. !ech
				inp0%x_fus(1)=0. !nbi
				inp0%x_fus(2)=0. !ech
				inp0%x_control(1)=0. !nbi
				inp0%x_control(2)=0. !ech
				inp0%dx_heat(1)=0.2 !nbi
				inp0%dx_heat(2)=0.03 !ech
				inp0%dx_cd(1)=0.2 !nbi
				inp0%dx_cd(2)=0.03 !ech
				inp0%dx_fus(1)=0.2 !nbi
				inp0%dx_fus(2)=0.03 !ech
				inp0%dx_control(1)=0.2 !nbi
				inp0%dx_control(2)=0.03 !ech
				inp0%nbi_energy=1000. !in keV

