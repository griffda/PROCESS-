module times_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the plasma pulse timings
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: pulsetimings = 1.0D0
  !! Switch for pulse timings (if lpulse=1):
  !!
  !! - =0, tohs = Ip(MA)/0.1 tramp, tqnch = input
  !! - =1, tohs = iteration var or input. tramp/tqnch max of input or tohs

  real(dp), bind(C) :: tburn = 1000.0D0
  !! burn time (s) (calculated if `lpulse=1`)

  real(dp) :: tburn0 = 0.0D0
  !! burn time (s) - used for internal consistency

  real(dp), bind(C) :: tcycle = 0.0D0
  !! full cycle time (s)

  real(dp), bind(C) :: tdown = 0.0D0
  !! down time (s)

  real(dp), bind(C) :: tdwell = 1800.0D0
  !! time between pulses in a pulsed reactor (s) (`iteration variable 17`)

  real(dp), bind(C) :: theat = 10.0D0
  !! heating time, after current ramp up (s)

  real(dp), dimension(6) :: tim = 0.0D0
  !! array of time points during plasma pulse (s)

  character(len=11), dimension(6) :: timelabel = (/ 'Start', &
    'BOP  ', &
    'EOR  ', &
    'BOF  ', &
    'EOF  ', &
    'EOP  ' /)
  !! array of time labels during plasma pulse (s)

  character(len=11), dimension(5) :: intervallabel = (/ 'tramp', &
    'tohs ', &
    'theat', &
    'tburn', &
    'tqnch' /)
  !! time intervals - as strings (s)

  real(dp), bind(C) :: tohs = 30.0D0
  !! plasma current ramp-up time for current initiation (s) (calculated if `lpulse=0`)
  !! (`iteration variable 65`)

  real(dp) :: tohsin = 0.0D0
  !! Switch for plasma current ramp-up time (if lpulse=0):
  !!
  !! - = 0, tohs = tramp = tqnch = Ip(MA)/0.5
  !! - <>0, tohs = tohsin; tramp, tqnch are input

  real(dp) :: tpulse = 0.0D0
  !! pulse length = tohs + theat + tburn + tqnch

  real(dp) :: tqnch = 15.0D0
  !! shut down time for PF coils (s); if pulsed, = tohs

  real(dp) :: tramp = 15.0D0
  !! initial PF coil charge time (s); if pulsed, = tohs

end module times_variables