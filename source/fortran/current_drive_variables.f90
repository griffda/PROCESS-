module current_drive_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the current drive system
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

#ifdef use_intrinsic
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

  implicit none

  public

  real(8) :: beamwd
  !! width of neutral beam duct where it passes between the TF coils (m)
  !! T Inoue et al, Design of neutral beam system for ITER-FEAT, 
  !! <A HREF=http://dx.doi.org/10.1016/S0920-3796(01)00339-8>
  !! Fusion Engineering and Design, Volumes 56-57, October 2001, Pages 517-521</A>)

  real(8) :: bigq
  !! Fusion gain; P_fusion / (P_injection + P_ohmic)

  real(8) :: bootipf
  !! bootstrap current fraction (enforced; see ibss)

  real(8) :: bscfmax
  !! maximum fraction of plasma current from bootstrap; if `bscfmax < 0`, 
  !! bootstrap fraction = abs(bscfmax)

  real(8) :: bscf_iter89
  !! bootstrap current fraction, ITER 1989 model

  real(8) :: bscf_nevins
  !! bootstrap current fraction, Nevins et al model

  real(8) :: bscf_sauter
  !! bootstrap current fraction, Sauter et al model

  real(8) :: bscf_wilson
  !! bootstrap current fraction, Wilson et al model

  real(8) :: cboot
  !! bootstrap current fraction multiplier (`ibss=1`)

  real(8) :: cnbeam
  !! neutral beam current (A)

  real(8) :: diacf_hender
  !! diamagnetic current fraction, Hender fit

  real(8) :: diacf_scene
  !! diamagnetic current fraction, SCENE fit

  real(8) :: diaipf
  !! diamagnetic current fraction

  real(8) :: echpwr
  !! ECH power (MW)

  real(8) :: echwpow
  !! ECH wall plug power (MW)

  real(8) :: effcd
  !! current drive efficiency (A/W)

  real(8) :: harnum
  !! cyclotron harmonic frequency number, used in EBW cut-off

  real(8) :: enbeam
  !! neutral beam energy (keV) (`iteration variable 19`)

  real(8) :: etacd
  !! auxiliary power wall plug to injector efficiency

  real(8) :: etacdfix
  !! secondary auxiliary power wall plug to injector efficiency

  real(8) :: etaech
  !! ECH wall plug to injector efficiency

  real(8) :: etalh
  !! lower hybrid wall plug to injector efficiency

  real(8) :: etanbi
  !! neutral beam wall plug to injector efficiency

  real(8) :: fpion
  !! fraction of beam energy to ions

  real(8) :: pnbitot
  !! neutral beam power entering vacuum vessel

  real(8) :: pscf_scene
  !! Pfirsch-Schlüter current fraction, SCENE fit

  real(8) :: nbshinemw
  !! neutral beam shine-through power

  real(8) :: feffcd
  !! current drive efficiency fudge factor (`iteration variable 47`)

  real(8) :: forbitloss
  !! fraction of neutral beam power lost after ionisation but before 
  !! thermalisation (orbit loss fraction)

  real(8) :: frbeam
  !! R_tangential / R_major for neutral beam injection

  real(8) :: ftritbm
  !! fraction of beam that is tritium

  real(8) :: gamcd
  !! normalised current drive efficiency (1.0e20 A/(W m^2))

  real(8) :: gamma_ecrh
  !! User input ECRH gamma (1.0e20 A/(W m^2))

  real(8) :: rho_ecrh
  !! normalised minor radius at which electron cyclotron current drive is maximum

  integer :: iefrf
  !! Switch for current drive efficiency model:
  !!
  !!  - =1 Fenstermacher Lower Hybrid
  !!  - =2 Ion Cyclotron current drive
  !!  - =3 Fenstermacher ECH
  !!  - =4 Ehst Lower Hybrid
  !!  - =5 ITER Neutral Beam
  !!  - =6 new Culham Lower Hybrid model
  !!  - =7 new Culham ECCD model
  !!  - =8 new Culham Neutral Beam model
  !!  - =9 RFP option removed in PROCESS (issue #508)
  !!  - =10 ECRH user input gamma
  !!  - =11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019)
  !!  - =12 EBW scaling (S. Freethy)

  integer :: iefrffix
  !! Switch for 2nd current drive efficiency model:
  !! 
  !! - =0 No fixed current drive
  !! - =1 Fenstermacher Lower Hybrid
  !! - =2 Ion Cyclotron current drive
  !! - =3 Fenstermacher ECH
  !! - =4 Ehst Lower Hybrid
  !! - =5 ITER Neutral Beam
  !! - =6 new Culham Lower Hybrid model
  !! - =7 new Culham ECCD model
  !! - =8 new Culham Neutral Beam model
  !! - =9 RFP option removed in PROCESS (issue #508)
  !! - =10 ECRH user input gamma
  !! - =11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019)
  !! - =12 EBW scaling (S. Freethy)

  integer :: irfcd
  !! Switch for current drive calculation:
  !!
  !! - =0 turned off
  !! - =1 turned on

  real(8) :: nbshinef
  !! neutral beam shine-through fraction

  real(8) :: nbshield
  !! neutral beam duct shielding thickness (m)

  real(8) :: pheat
  !! heating power not used for current drive (MW) (`iteration variable 11`)

  real(8) :: pheatfix
  !! secondary fixed heating power not used for current drive (MW)

  real(8) :: pinjalw
  !! maximum allowable value for injected power (MW) (`constraint equation 30`)

  real(8) :: pinjemw
  !! auxiliary injected power to electrons (MW)

  real(8) :: pinjimw
  !! auxiliary injected power to ions (MW)

  real(8) :: pinjmw
  !! total auxiliary injected power (MW)

  real(8)  :: pinjfixmw
  !! secondary total fixed auxiliary injected power (MW)

  real(8) :: plasipf
  !! plasma driven current fraction (Bootstrap + Diamagnetic + PS)

  real(8) :: plhybd
  !! lower hybrid injection power (MW)

  real(8) :: pnbeam
  !! neutral beam injection power (MW)

  real(8) :: porbitlossmw
  !! neutral beam power lost after ionisation but before thermalisation (orbit loss power) (MW)

  real(8) :: psipf
  !! Pfirsch-Schlüter current fraction

  real(8) :: pwplh
  !! lower hybrid wall plug power (MW)

  real(8) :: pwpnb
  !! neutral beam wall plug power (MW)

  real(8) :: rtanbeam
  !! neutral beam centreline tangency radius (m)

  real(8) :: rtanmax
  !! maximum tangency radius for centreline of beam (m)

  real(8) :: taubeam
  !! neutral beam e-decay lengths to plasma centre

  real(8) :: tbeamin
  !! permitted neutral beam e-decay lengths to plasma centre

  contains

  subroutine init_current_drive_variables
    !! Initialise module variables
    implicit none

    beamwd = 0.58D0  
    bigq = 0.0D0  
    bootipf = 0.0D0  
    bscfmax = 0.9D0  
    bscf_iter89 = 0.0D0  
    bscf_nevins = 0.0D0  
    bscf_sauter = 0.0D0  
    bscf_wilson = 0.0D0  
    cboot = 1.0D0  
    cnbeam = 0.0D0  
    diacf_hender = 0.0D0  
    diacf_scene = 0.0D0  
    diaipf = 0.0D0  
    echpwr = 0.0D0  
    echwpow = 0.0D0  
    effcd = 0.0D0  
    harnum = 1.0D0
    enbeam = 1.0D3  
    etacd = 0.0D0  
    etacdfix = 0.0D0  
    etaech = 0.3D0  
    etalh = 0.3D0  
    etanbi = 0.3D0  
    fpion = 0.5D0  
    pnbitot = 0.0D0  
    pscf_scene = 0.0D0  
    nbshinemw = 0.0D0  
    feffcd = 1.0D0  
    forbitloss = 0.0D0  
    frbeam = 1.05D0  
    ftritbm = 1.0D-6  
    gamcd = 0.0D0  
    gamma_ecrh = 0.35D0  
    rho_ecrh = 0.1D0  
    iefrf = 5  
    iefrffix = 0   
    irfcd = 1  
    nbshinef = 0.0D0  
    nbshield = 0.5D0  
    pheat = 0.0D0  
    pheatfix = 0.0D0  
    pinjalw = 150.0D0  
    pinjemw = 0.0D0  
    pinjimw = 0.0D0  
    pinjmw = 0.0D0  
    pinjfixmw = 0.0D0  
    plasipf = 0.0D0  
    plhybd = 0.0D0  
    pnbeam = 0.0D0  
    porbitlossmw = 0.0D0  
    psipf = 0.0D0  
    pwplh = 0.0D0  
    pwpnb = 0.0D0  
    rtanbeam = 0.0D0  
    rtanmax = 0.0D0  
    taubeam = 0.0D0  
    tbeamin = 3.0D0
  end subroutine init_current_drive_variables
end module current_drive_variables