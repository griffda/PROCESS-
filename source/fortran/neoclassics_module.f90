!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module neoclassics_module

    !! Module containing neoclassical computations
    !! author: J Lion, IPP Greifswald
    !! Formulas used are described in:
    !! Beidler (2013), https://doi.org/10.1088/0029-5515/51/7/076001
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef dp
    use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

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
        !  Densities of the species that are considered [/m3]
        real(dp), dimension(4) :: temperatures
        !  Temperature of the species that are considered [J]
        real(dp), dimension(4) :: dr_densities
        !  Radial derivative of the density of the species [/m3]
        real(dp), dimension(4) :: dr_temperatures
        !  Radial derivative of the temperature of the species [J]
    end type profile_values

    type, public :: neoclassics
        type(gauss_laguerre) :: gauss_laguerre
        !  Gauss Laguerre object
        real(dp), dimension(4,no_roots) :: nu = 0
        !  90-degree deflection frequency on GL roots
        real(dp), dimension(4,no_roots) :: nu_star = 0
        !  Dimensionless 90-degree deflection frequency on GL roots
        real(dp), dimension(4,no_roots) :: vd = 0
        !  Drift velocity on GL roots
        real(dp), dimension(4,no_roots) :: KT = 0
        !  Thermal energy on GL roots
        real(dp) :: Er = 0.0
        !  Radial electrical field [V/m]
        real(dp) :: iota = 1.0d0
        !  Iota (1/safety factor)
        real(dp), dimension(4,no_roots) :: D11_mono = 0
        !  Radial monoenergetic transport coefficient on GL roots (species dependent)
        real(dp), dimension(4,no_roots) :: D11_plateau = 0
        !  Radial monoenergetic transport coefficient on GL roots (species dependent)
        real(dp), dimension(:), allocatable :: nu_star_mono_input
        !  Radial monoenergetic transport coefficient as given by the stellarator input json
        !  on GL roots (species dependent)
        real(dp), dimension(:), allocatable :: D11_star_mono_input
        !  Radial monoenergetic transport coefficient as given by the stellarator input json
        !  as function of nu_star, normalized by the plateau value.
        real(dp), dimension(:), allocatable :: D13_star_mono_input
        !  Toroidal monoenergetic transport coefficient as given by the stellarator
        !  input json file as function of nu_star, normalized by the banana value.
        real(dp), dimension(4) :: D111 = 0
        !  Radial integrated transport coefficient (n=1) (species dependent)
        real(dp), dimension(4) :: D112 = 0
        !  Radial integrated transport coefficient (n=2) (species dependent)
        real(dp), dimension(4) :: D113 = 0
        !  Radial integrated transport coefficient (n=3) (species dependent)
        real(dp), dimension(4) :: q_flux = 0
        !  energy transport flux (J/m2)
        real(dp), dimension(4) :: Gamma_flux = 0
        !  energy flux from particle transport
        real(dp), dimension(no_roots) :: D31_mono = 0
        !  Toroidal monoenergetic transport coefficient
        real(dp) :: eps_eff = 1d-5
        !  Epsilon effective (used in neoclassics_calc_D11_mono)

        character, dimension(4) :: species = (/"e","D","T","a"/)
        !  Species that are considered (not used right now but keep it for now)

        type(profile_values) :: profiles
        !  Profile values

    contains
        procedure :: calc_KT => neoclassics_calc_KT
        procedure :: calc_nu => neoclassics_calc_nu
        procedure :: calc_nu_star => neoclassics_calc_nu_star
        procedure :: calc_D11_mono => neoclassics_calc_D11_mono
        procedure :: calc_vd => neoclassics_calc_vd
        procedure :: calc_D111 => neoclassics_calc_D111
        procedure :: calc_D112 => neoclassics_calc_D112
        procedure :: calc_D113 => neoclassics_calc_D113
        procedure :: calc_gamma_flux => neoclassics_calc_gamma_flux
        procedure :: calc_q_flux => neoclassics_calc_q_flux
        procedure :: calc_D11_plateau => neoclassics_calc_D11_plateau
        procedure :: interpolate_D11_mono => neoclassics_interpolate_D11_mono

    end type neoclassics
 



contains


    type(neoclassics) function init_neoclassics(r_eff,eps_eff,iota)
        !! Constructor of the neoclassics object from the effective radius,
        !! epsilon effective and iota only.
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real(dp), intent(in) :: r_eff,eps_eff,iota
        type(neoclassics) :: myneo
        real(dp), dimension(4,no_roots) :: mynu

        !! This should be called as the standard constructor
        myneo = neoclassics(gauss_laguerre = gauss_laguerre_30(), &
                            profiles = init_profile_values_from_PROCESS(r_eff), &
                            eps_eff = eps_eff, iota = iota)

        mynu = neoclassics_calc_nu(myneo)

        myneo%KT = myneo%calc_KT()
        myneo%nu = myneo%calc_nu()
        myneo%nu_star = myneo%calc_nu_star()
        myneo%vd = myneo%calc_vd()

        myneo%D11_plateau = myneo%calc_D11_plateau()

        myneo%D11_mono = myneo%calc_D11_mono() !for using epseff
        !alternatively use:  = myneo%interpolate_D11_mono() !

        myneo%D111 = myneo%calc_D111()

        myneo%D112 = myneo%calc_D112()
        myneo%D113 = myneo%calc_D113()

        myneo%Gamma_flux = myneo%calc_Gamma_flux()
        myneo%q_flux = myneo%calc_q_flux()

        ! Return:
        init_neoclassics = myneo
    end function init_neoclassics

    function neoclassics_calc_KT(self) result(KK)
        !! Calculates the energy on the given grid
        !! which is given by the gauss laguerre roots.
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: e_,c_,me_,mp_,keV_

        class(neoclassics), intent(in) :: self
        real(dp), dimension(no_roots) ::  K
        real(dp), dimension(4,no_roots) :: KK

        K = self%gauss_laguerre%roots/keV_

        KK(1,:) = K * self%profiles%temperatures(1) ! electrons
        KK(2,:) = K * self%profiles%temperatures(2) ! deuterium
        KK(3,:) = K * self%profiles%temperatures(3) ! tritium
        KK(4,:) = K * self%profiles%temperatures(4) ! helium

    end function neoclassics_calc_KT

    function neoclassics_calc_Gamma_flux(self)
        !! Calculates the Energy flux by neoclassicsal particle transport
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        class(neoclassics), intent(in) :: self
        
        real(dp),dimension(4) :: neoclassics_calc_Gamma_flux,densities, temps, dr_temps, dr_densities, z

        densities = self%profiles%densities
        temps = self%profiles%temperatures
        dr_densities = self%profiles%dr_densities
        dr_temps = self%profiles%dr_temperatures

        z = (/-1.0,1.0,1.0,2.0/)

        neoclassics_calc_Gamma_flux = - densities * self%D111 * ((dr_densities/densities - z * self%Er/temps)+ &
                        (self%D112/self%D111-3.0/2.0) * dr_temps/temps )
        
    end function neoclassics_calc_Gamma_flux

    function neoclassics_calc_q_flux(self)
        !! Calculates the Energy flux by neoclassicsal energy transport
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: q_flux, densities, temps, dr_temps, dr_densities, z, neoclassics_calc_q_flux


        densities = self%profiles%densities
        temps = self%profiles%temperatures
        dr_densities = self%profiles%dr_densities
        dr_temps = self%profiles%dr_temperatures

        z = (/-1.0,1.0,1.0,2.0/)

        q_flux = - densities * temps * self%D112 * ((dr_densities/densities - z * self%Er/temps) + &
                        (self%D113/self%D112-3.0/2.0) * dr_temps/temps )

        neoclassics_calc_q_flux = q_flux
    end function neoclassics_calc_q_flux

    function neoclassics_calc_D11_mono(self) result(D11_mono)
        !! Calculates the monoenergetic radial transport coefficients
        !! using epsilon effective.
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4,no_roots) :: D11_mono

        D11_mono = 4.0d0/(9.0d0*pi) * (2.0d0 * self%eps_eff)**(3.0d0/2.0d0) &
                    * self%vd**2/self%nu

    end function neoclassics_calc_D11_mono

    function neoclassics_calc_D11_plateau(self) result(D11_plateau)
        !! Calculates the plateau transport coefficients (D11_star sometimes)
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: pi, me_, mp_, c_
        use physics_variables, only: rmajor

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4,no_roots) :: D11_plateau, v
        real(dp),dimension(4) :: mass

        mass = (/me_,mp_*2.0d0,mp_*3.0d0,mp_*4.0d0/)

        v(1,:) = c_ * sqrt(1.0d0-(self%KT(1,:)/(mass(1) * c_**2)+1)**(-1))
        v(2,:) = c_ * sqrt(1.0d0-(self%KT(2,:)/(mass(2) * c_**2)+1)**(-1))
        v(3,:) = c_ * sqrt(1.0d0-(self%KT(3,:)/(mass(3) * c_**2)+1)**(-1))
        v(4,:) = c_ * sqrt(1.0d0-(self%KT(4,:)/(mass(4) * c_**2)+1)**(-1))

        D11_plateau = pi/4.0 * self%vd**2 * rmajor/ self%iota / v

    end function neoclassics_calc_D11_plateau

    function neoclassics_interpolate_D11_mono(self) result(D11_mono)
        !! Interpolates the D11 coefficients on the Gauss laguerre grid
        !! (This method is unused as of now, but is needed when taking D11 explicitely as input)
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use const_and_precisions, only: pi
        use maths_library, only: find_y_nonuniform_x
        ! use grad_func, only: interp1_ef

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4,no_roots) :: D11_mono
        integer :: ii,jj

        do ii = 1,4
            do jj = 1,no_roots
                D11_mono(ii,jj) = find_y_nonuniform_x(self%nu_star(ii,jj),self%nu_star_mono_input, &
                                                      self%D11_star_mono_input,size(self%nu_star_mono_input)) * &
                                  self%D11_plateau(ii,jj)
            end do
        end do

    end function neoclassics_interpolate_D11_mono

    function neoclassics_calc_vd(self)
        !! Calculates the drift velocities
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use const_and_precisions, only: e_
        use physics_variables, only: rmajor, bt

        class(neoclassics), intent(in) :: self
        real(dp), dimension(no_roots) :: vde,vdT,vdD,vda, K
        real(dp), dimension(4,no_roots) :: vd,neoclassics_calc_vd

        K = self%gauss_laguerre%roots

        vde = K * self%profiles%temperatures(1)/(e_ * rmajor * bt)
        vdD = K * self%profiles%temperatures(2)/(e_ * rmajor * bt)
        vdT = K * self%profiles%temperatures(3)/(e_ * rmajor * bt)
        vda = K * self%profiles%temperatures(4)/(2.0*e_ * rmajor * bt)

        vd(1,:) = vde
        vd(2,:) = vdD
        vd(3,:) = vdT
        vd(4,:) = vda

        neoclassics_calc_vd = vd
    end function neoclassics_calc_vd

    function neoclassics_calc_nu_star(self) result(nu_star)
        !! Calculates the normalized collision frequency
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use const_and_precisions, only: e_,c_,me_,mp_
        use physics_variables, only: rmajor

        class(neoclassics), intent(in) :: self
        real(dp), dimension(no_roots) ::  K
        real(dp), dimension(4,no_roots) :: v,nu_star,KK
        real(dp), dimension(4) :: mass

        K = self%gauss_laguerre%roots

        KK(1,:) = K * self%profiles%temperatures(1)
        KK(2,:) = K * self%profiles%temperatures(2)
        KK(3,:) = K * self%profiles%temperatures(3)
        KK(4,:) = K * self%profiles%temperatures(4)

        mass = (/me_,mp_*2.0d0,mp_*3.0d0,mp_*4.0d0/)

        v(1,:) = c_ * sqrt(1.0d0-(KK(1,:)/(mass(1) * c_**2)+1)**(-1))
        v(2,:) = c_ * sqrt(1.0d0-(KK(2,:)/(mass(2) * c_**2)+1)**(-1))
        v(3,:) = c_ * sqrt(1.0d0-(KK(3,:)/(mass(3) * c_**2)+1)**(-1))
        v(4,:) = c_ * sqrt(1.0d0-(KK(4,:)/(mass(4) * c_**2)+1)**(-1))

        nu_star = rmajor * self%nu/(self%iota*v)

    end function neoclassics_calc_nu_star

    function neoclassics_calc_D111(self)
        !! Calculates the integrated radial transport coefficients (index 1)
        !! It uses Gauss laguerre integration
        !! https://en.wikipedia.org/wiki/Gauss%E2%80%93Laguerre_quadrature
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: D111, neoclassics_calc_D111

        real(dp),dimension(no_roots) :: xi,wi

        xi = self%gauss_laguerre%roots
        wi = self%gauss_laguerre%weights
     
        D111(1) = sum(2.0d0/sqrt(pi) * self%D11_mono(1,:) * xi**(1.0-0.5) * wi)
        D111(2) = sum(2.0d0/sqrt(pi) * self%D11_mono(2,:) * xi**(1.0-0.5) * wi)
        D111(3) = sum(2.0d0/sqrt(pi) * self%D11_mono(3,:) * xi**(1.0-0.5) * wi)
        D111(4) = sum(2.0d0/sqrt(pi) * self%D11_mono(4,:) * xi**(1.0-0.5) * wi)

        neoclassics_calc_D111 = D111

    end function neoclassics_calc_D111

    function neoclassics_calc_D112(self)
        !! Calculates the integrated radial transport coefficients (index 2)
        !! It uses Gauss laguerre integration
        !! https://en.wikipedia.org/wiki/Gauss%E2%80%93Laguerre_quadrature
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: D112, neoclassics_calc_D112

        real(dp),dimension(no_roots) :: xi,wi

        xi = self%gauss_laguerre%roots
        wi = self%gauss_laguerre%weights
     
        D112(1) = sum(2.0d0/sqrt(pi) * self%D11_mono(1,:) * xi**(2.0-0.5) * wi)
        D112(2) = sum(2.0d0/sqrt(pi) * self%D11_mono(2,:) * xi**(2.0-0.5) * wi)
        D112(3) = sum(2.0d0/sqrt(pi) * self%D11_mono(3,:) * xi**(2.0-0.5) * wi)
        D112(4) = sum(2.0d0/sqrt(pi) * self%D11_mono(4,:) * xi**(2.0-0.5) * wi)

        neoclassics_calc_D112 = D112

    end function neoclassics_calc_D112

    function neoclassics_calc_D113(self)
        !! Calculates the integrated radial transport coefficients (index 3)
        !! It uses Gauss laguerre integration
        !! https://en.wikipedia.org/wiki/Gauss%E2%80%93Laguerre_quadrature
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: pi

        class(neoclassics), intent(in) :: self
        real(dp),dimension(4) :: D113, neoclassics_calc_D113

        real(dp),dimension(no_roots) :: xi,wi

        xi = self%gauss_laguerre%roots
        wi = self%gauss_laguerre%weights
     
        D113(1) = sum(2.0d0/sqrt(pi) * self%D11_mono(1,:) * xi**(3.0-0.5) * wi)
        D113(2) = sum(2.0d0/sqrt(pi) * self%D11_mono(2,:) * xi**(3.0-0.5) * wi)
        D113(3) = sum(2.0d0/sqrt(pi) * self%D11_mono(3,:) * xi**(3.0-0.5) * wi)
        D113(4) = sum(2.0d0/sqrt(pi) * self%D11_mono(4,:) * xi**(3.0-0.5) * wi)

        neoclassics_calc_D113 = D113
    end function neoclassics_calc_D113

    function neoclassics_calc_nu(self)
        !! Calculates the collision frequency
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use const_and_precisions, only: pi, me_, mp_, eps0_,e_

        real(dp),dimension(4,no_roots) :: neoclassics_calc_nu
        class(neoclassics), intent(in) :: self
        real(dp) :: t,erfn,phixmgx,expxk,xk, lnlambda,x,v
        real(dp),dimension(4) :: temp, mass,density,z

        integer :: jj,ii,kk

        temp = self%profiles%temperatures
        density = self%profiles%densities

        !          e      D      T         a (He)
        mass = (/me_,mp_*2.0d0,mp_*3.0d0,mp_*4.0d0/)
        z = (/-1.0d0,1.0d0,1.0d0,2.0d0/) * e_

        
        ! transform the temperature back in eV
        ! Formula from L. Spitzer.Physics of fully ionized gases.  Interscience, New York, 1962
        lnlambda = 32.2d0 - 1.15d0*log10(density(1)) + 2.3d0*log10(temp(1)/e_)

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

    type(profile_values) function init_profile_values_from_PROCESS(rho)
        !! Initializes the profile_values object from PROCESS' parabolic profiles
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        use physics_variables, only: ne0,te0,alphan,&
                                     alphat,ti0,ni0,fdeut, dnalp, rminor
        use const_and_precisions, only: keV_

        real(dp), intent(in) :: rho

        real(dp),dimension(4) :: dens,temp, dr_dens, dr_temp
        real(dp) :: dense, densD,densT,densa, &
                    tempD,tempT,tempa,tempe, &
                    dr_tempe, dr_tempT, dr_tempD, dr_tempa,&
                    dr_dense, dr_densT, dr_densD, dr_densa, r

        
        r = rho * rminor

        tempe = te0 * (1-rho**2)**alphat * keV_ ! To SI units bc.. convenience I guess?
        tempT = ti0 * (1-rho**2)**alphat * keV_
        tempD = ti0 * (1-rho**2)**alphat * keV_
        tempa = ti0 * (1-rho**2)**alphat * keV_

        dense = ne0 * (1-rho**2)**alphan
        densT = (1-fdeut) * ni0 * (1-rho**2)**alphan
        densD = fdeut *ni0 * (1-rho**2)**alphan
        densa = dnalp*(1+alphan) * (1-rho**2)**alphan

        ! Derivatives in real space
        dr_tempe = -2 * 1.0d0/rminor * te0 * rho * (1-rho**2)**(alphat-1) * alphat * keV_
        dr_tempT = -2 * 1.0d0/rminor * ti0 * rho * (1-rho**2)**(alphat-1) * alphat * keV_
        dr_tempD = -2 * 1.0d0/rminor * ti0 * rho * (1-rho**2)**(alphat-1) * alphat * keV_
        dr_tempa = -2 * 1.0d0/rminor * ti0 * rho * (1-rho**2)**(alphat-1) * alphat * keV_

        dr_dense = -2 * 1.0d0/rminor * rho * ne0 *             (1-rho**2)**(alphan-1) * alphan
        dr_densT = -2 * 1.0d0/rminor * rho * (1-fdeut) * ni0 * (1-rho**2)**(alphan-1) * alphan
        dr_densD = -2 * 1.0d0/rminor * rho * fdeut *ni0 *      (1-rho**2)**(alphan-1) * alphan
        dr_densa = -2 * 1.0d0/rminor * rho * dnalp*(1+alphan)* (1-rho**2)**(alphan-1) * alphan

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
        !! Sets the gauss Laguerre roots and weights for 30 
        !! discretization points. Used for integration in this module.
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
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