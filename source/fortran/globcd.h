!     globcd.h
!------------------------------------------------------------------------
      REAL(p_) small
      parameter (small=1.e-3_p_)
      REAL(p_) tmass
      parameter (tmass=511._p_)
!
      REAL(p_) pi
      common/cnstcd/pi
!
      REAL(p_) tolval
      common /sftwcd/tolval
!
      integer modelv
      integer igv
      common /mdlpcd/modelv,igv
!
      REAL(p_) yval, enz, enzsq, sgnnz
      integer nharm
      common /wavccd/yval,enz,enzsq,sgnnz,nharm
!
      REAL(p_) hloc, href, hav, hsqav, chrtav
      REAL(p_) cxi2, cxi4, fc, ft
      common /geoqcd/hloc,href,hav,hsqav,chrtav,cxi2,cxi4,fc,ft
!
      REAL(p_) zeff, zrat, tau, etcutoff, gammin, gammax
      common /ztmpcd/zeff,zrat,tau,etcutoff,gammin,gammax
!
      REAL(p_)  eta
      common /collcd/eta
