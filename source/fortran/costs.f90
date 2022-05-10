! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_module
   !! Module containing fusion power plant costing algorithms
   !! author: P J Knight, CCFE, Culham Science Centre
   !! N/A
   !! This module contains the PROCESS fusion power plant costing model,
   !! split into separate cost accounts.
   !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef dp
   use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   implicit none

   !  Various cost account values (M$)
   real(dp) :: c228, c229, c23, c25, c26, cindrt, ccont

   !  Account 226 - Heat transport system
   real(dp) :: c226, c2261, c2262, c2263

   !  Account 227 - Fuel handling
   real(dp) :: c227, c2271, c2272, c2273, c2274

   !  Account 24 - electrical plant equipment
   real(dp) :: c24, c241, c242, c243, c244, c245


   real(dp) :: c21, c211, c212, c213, c214, c2141, c2142, c215, c216, c217, c2171

   real(dp) :: c2172, c2173, c2174, c22, c2211, c2212, c22121, c22122, c22123

   real(dp) :: c22124, c22125, c22126, c22127, c2213, c22131, c22132, c2214, c2215

   real(dp) :: c2221, c22211, c22212, c22213, c22214, c22215, c2222, c22221, c22222

   real(dp) :: c22223, c22224, c2223, c223, c2231, c2232, c2233, c2234, c224, c2241

   real(dp) :: c2242, c2243, c2244, c2245, c2246, c225, c2251, c22511, c22512, c22513

   real(dp) :: c22514, c22515, c2252, c22521, c22522, c22523, c22524, c22525, c22526

   real(dp) :: c22527, c2253, chx, cpp, cppa, c22128

contains

   subroutine init_costs_module
      !! Initialise module variables
      implicit none

      c228 = 0.0D0
      c229 = 0.0D0
      c23 = 0.0D0
      c25 = 0.0D0
      c26 = 0.0D0
      cindrt = 0.0D0
      ccont = 0.0D0
      c226 = 0.0D0
      c2261 = 0.0D0
      c2262 = 0.0D0
      c2263 = 0.0D0
      c227 = 0.0D0
      c2271 = 0.0D0
      c2272 = 0.0D0
      c2273 = 0.0D0
      c2274 = 0.0D0
      c24 = 0.0D0
      c241 = 0.0D0
      c242 = 0.0D0
      c243 = 0.0D0
      c244 = 0.0D0
      c245 = 0.0D0
      c21 = 0.0D0
      c211 = 0.0D0
      c212 = 0.0D0
      c213 = 0.0D0
      c214 = 0.0D0
      c2141 = 0.0D0
      c2142 = 0.0D0
      c215 = 0.0D0
      c216 = 0.0D0
      c217 = 0.0D0
      c2171 = 0.0D0
      c2172 = 0.0D0
      c2173 = 0.0D0
      c2174 = 0.0D0
      c22 = 0.0D0
      c2211 = 0.0D0
      c2212 = 0.0D0
      c22121 = 0.0D0
      c22122 = 0.0D0
      c22123 = 0.0D0
      c22124 = 0.0D0
      c22125 = 0.0D0
      c22126 = 0.0D0
      c22127 = 0.0D0
      c2213 = 0.0D0
      c22131 = 0.0D0
      c22132 = 0.0D0
      c2214 = 0.0D0
      c2215 = 0.0D0
      c2221 = 0.0D0
      c22211 = 0.0D0
      c22212 = 0.0D0
      c22213 = 0.0D0
      c22214 = 0.0D0
      c22215 = 0.0D0
      c2222 = 0.0D0
      c22221 = 0.0D0
      c22222 = 0.0D0
      c22223 = 0.0D0
      c22224 = 0.0D0
      c2223 = 0.0D0
      c223 = 0.0D0
      c2231 = 0.0D0
      c2232 = 0.0D0
      c2233 = 0.0D0
      c2234 = 0.0D0
      c224 = 0.0D0
      c2241 = 0.0D0
      c2242 = 0.0D0
      c2243 = 0.0D0
      c2244 = 0.0D0
      c2245 = 0.0D0
      c2246 = 0.0D0
      c225 = 0.0D0
      c2251 = 0.0D0
      c22511 = 0.0D0
      c22512 = 0.0D0
      c22513 = 0.0D0
      c22514 = 0.0D0
      c22515 = 0.0D0
      c2252 = 0.0D0
      c22521 = 0.0D0
      c22522 = 0.0D0
      c22523 = 0.0D0
      c22524 = 0.0D0
      c22525 = 0.0D0
      c22526 = 0.0D0
      c22527 = 0.0D0
      c2253 = 0.0D0
      chx = 0.0D0
      cpp = 0.0D0
      cppa = 0.0D0
      c22128 = 0.0D0
   end subroutine init_costs_module


   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module costs_module
