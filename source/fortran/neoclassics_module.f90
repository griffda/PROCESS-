!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module neoclassics_module

    !! Module containing neoclassical computations
    !! author: J Lion, IPP Greifswald
    !! N/A
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    private
    integer, parameter :: no_roots = 30 ! Number of Gauss laguerre roots

    public :: init_neoclassics

    type :: gauss_laguerre
        real(dp), dimension(no_roots) :: roots
        !  Gauss Laguerre Roots
        real(dp), dimension(no_roots) :: weights
        !  Gauss Laguerre Weights
    end type gauss_laguerre

    type, public :: profile_values
        character, dimension(4) :: species = (/"e","D","T","a"/)
        !  Species that are considered
        real(dp), dimension(4) :: densities
        !  Densities of the species that are considered
        real(dp), dimension(4) :: temperatures
        !  Densities of the species that are considered
        real(dp), dimension(4) :: dr_densities
        !  Densities of the species that are considered
        real(dp), dimension(4) :: dr_temperatures
        !  Densities of the species that are considered
    end type profile_values

    type, public :: neoclassics
        type(gauss_laguerre) :: gauss_laguerre
        !  Gauss Laguerre object
        real(dp), dimension(4,no_roots) :: nu = 0
        !  90-degree deflection frequency 
        real(dp), dimension(4,no_roots) :: vd = 0
        !  Drift velocity
        real(dp) :: Er = 0
        !  Radial electrical field
        real(dp), dimension(no_roots) :: lnlambda = 0
        !  Coulomb Logarithm
        real(dp), dimension(4,no_roots) :: D11_mono = 0
        !  Radial monoenergetic transport coefficient
        real(dp), dimension(4) :: D111 = 0
        !  Radial integrated transport coefficient (n=1)
        real(dp), dimension(4) :: D112 = 0
        !  Radial integrated transport coefficient (n=2)
        real(dp), dimension(4) :: D113 = 0
        !  Radial integrated transport coefficient (n=3)
        real(dp), dimension(4) :: q_flux = 0
        !  energy transport flux
        real(dp), dimension(4) :: Gamma_flux = 0
        !  energy flux from particle transport
        real(dp), dimension(no_roots) :: D31_mono = 0
        !  Toroidal monoenergetic transport coefficient
        real(dp) :: eps_eff = 1d-5
        !  Epsilon effective

        character, dimension(4) :: species = (/"e","D","T","a"/)
        !  Species that are considered

        type(profile_values) :: profiles
        !  Profile values

    contains
        procedure :: calc_nu => neoclassics_calc_nu
        procedure :: calc_D11_mono => neoclassics_calc_D11_mono
        procedure :: calc_vd => neoclassics_calc_vd
        procedure :: calc_D111 => neoclassics_calc_D111
        procedure :: calc_D112 => neoclassics_calc_D112
        procedure :: calc_D113 => neoclassics_calc_D113
        procedure :: calc_gamma_flux => neoclassics_calc_gamma_flux
        procedure :: calc_q_flux => neoclassics_calc_q_flux

    end type neoclassics
 



    type, public :: profiles
        character, dimension(4) :: species = (/"e","D","T","a"/)
        !  Species that are considered
        real(dp), dimension(:,:),allocatable :: densities
        !  Densities of the species that are considered
        real(dp), dimension(:,:),allocatable :: temperatures
        !  Densities of the species that are considered
    end type profiles


    ! Overwrite implicit fortran constructors
    interface profiles ! Ignore the warning here
       procedure :: init_profiles_from_PROCESS
    end interface profiles

    interface profile_values ! Ignore the warning here
        procedure :: init_profile_values_from_PROCESS
    end interface profile_values

    interface create_neoclassics ! Ignore the warning here
       procedure :: init_neoclassics
    end interface create_neoclassics





contains

    function neoclassics_calc_Gamma_flux(self)
        ! Calculates the Energy flux by particle transport
        class(neoclassics), intent(in) :: self
        
        
        real(dp),dimension(4) :: neoclassics_calc_Gamma_flux,densities, temps, dr_temps, dr_densities, z


        densities = self%profiles%densities
        temps = self%profiles%temperatures
        dr_densities = self%profiles%dr_densities
        dr_temps = self%profiles%dr_temperatures

        z = (/-1.0,1.0,1.0,2.0/)

        neoclassics_calc_Gamma_flux = - densities * self%D111 * ((dr_densities/densities - z * self%Er/temps)+ &
                        (self%D112/self%D113-3.0/2.0) * dr_temps/temps )
        
    end function neoclassics_calc_Gamma_flux

    function neoclassics_calc_q_flux(self) result(q_flux)
        ! Calculates the Energy flux by neocl. energy transport

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: q_flux, densities, temps, dr_temps, dr_densities, z


        densities = self%profiles%densities
        temps = self%profiles%temperatures
        dr_densities = self%profiles%dr_densities
        dr_temps = self%profiles%dr_temperatures

        z = (/-1.0,1.0,1.0,2.0/)

        q_flux = - densities * temps * self%D112 * ((dr_densities/densities - z * self%Er/temps) + &
                        (self%D113/self%D112-3.0/2.0) * dr_temps/temps )


    end function neoclassics_calc_q_flux

    function neoclassics_calc_D11_mono(self) result(D11_mono)
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4,no_roots) :: D11_mono

     
        D11_mono = 4.0d0/(9.0d0*pi) * (2.0d0 * self%eps_eff)**(3.0d0/2.0d0) &
                    * self%vd**2/self%nu

    end function neoclassics_calc_D11_mono

    function neoclassics_calc_vd(self) result(vd)
        ! Calculates the drift velocities

        use const_and_precisions, only: e_
        use physics_variables, only: rmajor, bt

        class(neoclassics), intent(in) :: self
        real(dp), dimension(no_roots) :: vde,vdT,vdD,vda, K
        real(dp), dimension(4,no_roots) :: vd



        K = self%gauss_laguerre%roots

        vde = K * self%profiles%temperatures(1)/(e_ * rmajor * bt)
        vdD = K * self%profiles%temperatures(2)/(e_ * rmajor * bt)
        vdT = K * self%profiles%temperatures(3)/(e_ * rmajor * bt)
        vda = K * self%profiles%temperatures(4)/(2.0*e_ * rmajor * bt)

        vd(1,:) = vde
        vd(2,:) = vdD
        vd(3,:) = vdT
        vd(4,:) = vda

    end function neoclassics_calc_vd

    function neoclassics_calc_D111(self) result(D111)
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: D111

        real(dp),dimension(no_roots) :: xi,wi

        xi = self%gauss_laguerre%roots
        wi = self%gauss_laguerre%weights
     
        D111(1) = sum(2.0d0/sqrt(pi) * self%D11_mono(1,:) * xi**(1.0-0.5) * wi)
        D111(2) = sum(2.0d0/sqrt(pi) * self%D11_mono(2,:) * xi**(1.0-0.5) * wi)
        D111(3) = sum(2.0d0/sqrt(pi) * self%D11_mono(3,:) * xi**(1.0-0.5) * wi)
        D111(4) = sum(2.0d0/sqrt(pi) * self%D11_mono(4,:) * xi**(1.0-0.5) * wi)

    end function neoclassics_calc_D111

    function neoclassics_calc_D112(self) result(D112)
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: D112

        real(dp),dimension(no_roots) :: xi,wi

        xi = self%gauss_laguerre%roots
        wi = self%gauss_laguerre%weights
     
        D112(1) = sum(2.0d0/sqrt(pi) * self%D11_mono(1,:) * xi**(2.0-0.5) * wi)
        D112(2) = sum(2.0d0/sqrt(pi) * self%D11_mono(2,:) * xi**(2.0-0.5) * wi)
        D112(3) = sum(2.0d0/sqrt(pi) * self%D11_mono(3,:) * xi**(2.0-0.5) * wi)
        D112(4) = sum(2.0d0/sqrt(pi) * self%D11_mono(4,:) * xi**(2.0-0.5) * wi)

    end function neoclassics_calc_D112

    function neoclassics_calc_D113(self) result(D113)
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: D113

        real(dp),dimension(no_roots) :: xi,wi

        xi = self%gauss_laguerre%roots
        wi = self%gauss_laguerre%weights
     
        D113(1) = sum(2.0d0/sqrt(pi) * self%D11_mono(1,:) * xi**(3.0-0.5) * wi)
        D113(2) = sum(2.0d0/sqrt(pi) * self%D11_mono(2,:) * xi**(3.0-0.5) * wi)
        D113(3) = sum(2.0d0/sqrt(pi) * self%D11_mono(3,:) * xi**(3.0-0.5) * wi)
        D113(4) = sum(2.0d0/sqrt(pi) * self%D11_mono(4,:) * xi**(3.0-0.5) * wi)

    end function neoclassics_calc_D113

    function neoclassics_calc_nu(self)
        use const_and_precisions, only: pi, me_, mp_, eps0_,e_

        real(dp),dimension(4,no_roots) :: neoclassics_calc_nu
        class(neoclassics), intent(in) :: self
        real(dp) :: t,erfn,phixmgx,expxk,xk, lnlambda,x,v
        real(dp),dimension(4) :: temp, mass,density,z


        integer :: jj,ii,kk


        temp = self%profiles%temperatures
        density = self%profiles%densities

        !          e      D      T         a
        mass = (/me_,mp_*2.0d0,mp_*3.0d0,mp_*4.0d0/)
        z = (/-1.0d0,1.0d0,1.0d0,2.0d0/) * e_

        print *,"Densities: ",density(1),density(2),density(3),density(4)
        
        ! transform the temperature back in eV
        ! Formula from L. Spitzer.Physics of fully ionized gases.  Interscience, New York, 1962
        lnlambda = 32.2d0 - 1.15d0*log10(density(1)) + 2.3d0*log10(temp(1)/e_)

        print *, "My lnlambda is:",lnlambda
        neoclassics_calc_nu(:,:) = 0.0

        do jj = 1, 4
           do ii = 1, no_roots
              x = self%gauss_laguerre%roots(ii) 
              do kk = 1,4
                 xk = (mass(kk)/mass(jj))*(temp(jj)/temp(kk))*x
                 expxk = exp(-xk)
                 t = 1.0d0/(1.0d0+0.3275911d0*sqrt(xk))
                 erfn = 1.0d0-t*(.254829592d0 + t*(-.284496736d0 + t*(1.421413741d0       &
                         + t*(-1.453152027d0 +t*1.061405429d0))))*expxk
                 phixmgx = (1.0-0.5/xk)*erfn + expxk/sqrt(pi*xk)
                 v = sqrt(2.*x*temp(jj)/mass(jj))
                 neoclassics_calc_nu(jj,ii) = neoclassics_calc_nu(jj,ii) + density(kk)*(z(jj)*z(kk))**2 &
                              *lnlambda *phixmgx/(4.0*pi*eps0_**2*mass(jj)**2*v**3)
              enddo
           enddo
        enddo

    end function neoclassics_calc_nu


    type(neoclassics) function init_neoclassics(r_eff)
        real(dp), intent(in) :: r_eff
        type(neoclassics) :: myneo
        real(dp), dimension(4,no_roots) :: mynu

        ! This should be called as the standard constructor
        myneo = neoclassics(gauss_laguerre = gauss_laguerre_30(),&
                            profiles = init_profile_values_from_PROCESS(r_eff))

        mynu = neoclassics_calc_nu(myneo)

        print *, "My nu: ",mynu(1,1)

        myneo%nu = myneo%calc_nu()
        myneo%vd = myneo%calc_vd()
        
        myneo%D11_mono = myneo%calc_D11_mono()
        
        !print *, "Check: ",myneo%D11_mono
        myneo%D111 = myneo%calc_D111()
        
        myneo%D112 = myneo%calc_D112()
        myneo%D113 = myneo%calc_D113()

        myneo%Gamma_flux = myneo%calc_Gamma_flux()
        myneo%q_flux = myneo%calc_q_flux()
        
        
        init_neoclassics = myneo
    end function init_neoclassics



    type(profiles) function init_profiles_from_PROCESS(no_r)
        use physics_variables, only: ne0,te0,alphan,&
                                     alphat,ti0,ni0,fdeut, dnalp

        integer, intent(in) :: no_r

        integer :: i
        real(dp),dimension(4,no_r) :: dens,temp
        real(dp),dimension(no_r) :: rlist, dense, densD,densT,densa, &
                                    tempD,tempT,tempa,tempe

        rlist = (/(real(i,dp)/no_r, i=0,no_r-1, 1)/)

        tempe = te0 * (1-rlist**2)**alphat

        tempT = ti0 * (1-rlist**2)**alphat
        tempD = ti0 * (1-rlist**2)**alphat
        tempa = ti0 * (1-rlist**2)**alphat

        dense = ne0 * (1-rlist**2)**alphan

        densT = (1-fdeut) * ni0 * (1-rlist**2)**alphan
        densD = fdeut *ni0 * (1-rlist**2)**alphan
        densa = dnalp*(1+alphan) * (1-rlist**2)**alphan

        dens(1,:) = dense
        dens(2,:) = densD
        dens(3,:) = densT
        dens(4,:) = densa

        temp(1,:) = tempe
        temp(2,:) = tempD
        temp(3,:) = tempT
        temp(4,:) = tempa

        init_profiles_from_PROCESS%densities = dens
        init_profiles_from_PROCESS%temperatures = temp

    end function init_profiles_from_PROCESS

    type(profile_values) function init_profile_values_from_PROCESS(r)
        use physics_variables, only: ne0,te0,alphan,&
                                     alphat,ti0,ni0,fdeut, dnalp
        use const_and_precisions, only: keV_

        real(dp), intent(in) :: r

        real(dp),dimension(4) :: dens,temp, dr_dens, dr_temp
        real(dp) :: dense, densD,densT,densa, &
                    tempD,tempT,tempa,tempe, &
                    dr_tempe, dr_tempT, dr_tempD, dr_tempa,&
                    dr_dense, dr_densT, dr_densD, dr_densa

        

        tempe = te0 * (1-r**2)**alphat * keV_ ! To SI units bc.. convenience I guess?
        tempT = ti0 * (1-r**2)**alphat * keV_
        tempD = ti0 * (1-r**2)**alphat * keV_
        tempa = ti0 * (1-r**2)**alphat * keV_

        dense = ne0 * (1-r**2)**alphan
        densT = (1-fdeut) * ni0 * (1-r**2)**alphan
        densD = fdeut *ni0 * (1-r**2)**alphan
        densa = dnalp*(1+alphan) * (1-r**2)**alphan

        dr_tempe = -2 * te0 * r * (1-r**2)**(alphat-1) * alphat * keV_
        dr_tempT = -2 * ti0 * r * (1-r**2)**(alphat-1) * alphat * keV_
        dr_tempD = -2 * ti0 * r * (1-r**2)**(alphat-1) * alphat * keV_
        dr_tempa = -2 * ti0 * r * (1-r**2)**(alphat-1) * alphat * keV_

        dr_dense = -2 * r * ne0 *             (1-r**2)**(alphan-1) * alphan
        dr_densT = -2 * r * (1-fdeut) * ni0 * (1-r**2)**(alphan-1) * alphan
        dr_densD = -2 * r * fdeut *ni0 *      (1-r**2)**(alphan-1) * alphan
        dr_densa = -2 * r * dnalp*(1+alphan)* (1-r**2)**(alphan-1) * alphan

        dens(1) = dense
        dens(2) = densD
        dens(3) = densT
        dens(4) = densa

        temp(1) = tempe
        temp(2) = tempD
        temp(3) = tempT
        temp(4) = tempa

        dr_dens(1) = dr_dense
        dr_dens(2) = dr_densD
        dr_dens(3) = dr_densT
        dr_dens(4) = dr_densa

        dr_temp(1) = dr_tempe
        dr_temp(2) = dr_tempD
        dr_temp(3) = dr_tempT
        dr_temp(4) = dr_tempa

        init_profile_values_from_PROCESS%densities = dens
        init_profile_values_from_PROCESS%temperatures = temp
        init_profile_values_from_PROCESS%dr_densities = dr_dens
        init_profile_values_from_PROCESS%dr_temperatures = dr_temp

    end function init_profile_values_from_PROCESS



    type(gauss_laguerre) function gauss_laguerre_30()
        gauss_laguerre_30%roots = (/4.740718054080526184d-02,&
                                    2.499239167531593919d-01,&
                                    6.148334543927683749d-01,&
                                    1.143195825666101451d+00,&
                                    1.836454554622572344d+00,&
                                    2.696521874557216147d+00,&
                                    3.725814507779509288d+00,&
                                    4.927293765849881879d+00,&
                                    6.304515590965073635d+00,&
                                    7.861693293370260349d+00,&
                                    9.603775985479263255d+00,&
                                    1.153654659795613924d+01,&
                                    1.366674469306423489d+01,&
                                    1.600222118898106771d+01,&
                                    1.855213484014315029d+01,&
                                    2.132720432178312819d+01,&
                                    2.434003576453269346d+01,&
                                    2.760555479678096091d+01,&
                                    3.114158670111123683d+01,&
                                    3.496965200824907072d+01,&
                                    3.911608494906788991d+01,&
                                    4.361365290848483056d+01,&
                                    4.850398616380419980d+01,&
                                    5.384138540650750571d+01,&
                                    5.969912185923549686d+01,&
                                    6.618061779443848991d+01,&
                                    7.344123859555988076d+01,&
                                    8.173681050672767867d+01,&
                                    9.155646652253683726d+01,&
                                    1.041575244310588886d+02/)


        gauss_laguerre_30%weights = (/1.160440860204388913d-01,&
                                  2.208511247506771413d-01,&
                                  2.413998275878537214d-01,&
                                  1.946367684464170855d-01,&
                                  1.237284159668764899d-01,&
                                  6.367878036898660943d-02,&
                                  2.686047527337972682d-02,&
                                  9.338070881603925677d-03,&
                                  2.680696891336819664d-03,&
                                  6.351291219408556439d-04,&
                                  1.239074599068830081d-04,&
                                  1.982878843895233056d-05,&
                                  2.589350929131392509d-06,&
                                  2.740942840536013206d-07,&
                                  2.332831165025738197d-08,&
                                  1.580745574778327984d-09,&
                                  8.427479123056716393d-11,&
                                  3.485161234907855443d-12,&
                                  1.099018059753451500d-13,&
                                  2.588312664959080167d-15,&
                                  4.437838059840028968d-17,&
                                  5.365918308212045344d-19,&
                                  4.393946892291604451d-21,&
                                  2.311409794388543236d-23,&
                                  7.274588498292248063d-26,&
                                  1.239149701448267877d-28,&
                                  9.832375083105887477d-32,&
                                  2.842323553402700938d-35,&
                                  1.878608031749515392d-39,&
                                  8.745980440465011553d-45/)
                            

    end function gauss_laguerre_30




end module neoclassics_module