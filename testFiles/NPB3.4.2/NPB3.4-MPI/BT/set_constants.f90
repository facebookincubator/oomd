!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine  set_constants

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      implicit none
      
      ce(1,1)  = 2.0d0
      ce(1,2)  = 0.0d0
      ce(1,3)  = 0.0d0
      ce(1,4)  = 4.0d0
      ce(1,5)  = 5.0d0
      ce(1,6)  = 3.0d0
      ce(1,7)  = 0.5d0
      ce(1,8)  = 0.02d0
      ce(1,9)  = 0.01d0
      ce(1,10) = 0.03d0
      ce(1,11) = 0.5d0
      ce(1,12) = 0.4d0
      ce(1,13) = 0.3d0
      
      ce(2,1)  = 1.0d0
      ce(2,2)  = 0.0d0
      ce(2,3)  = 0.0d0
      ce(2,4)  = 0.0d0
      ce(2,5)  = 1.0d0
      ce(2,6)  = 2.0d0
      ce(2,7)  = 3.0d0
      ce(2,8)  = 0.01d0
      ce(2,9)  = 0.03d0
      ce(2,10) = 0.02d0
      ce(2,11) = 0.4d0
      ce(2,12) = 0.3d0
      ce(2,13) = 0.5d0

      ce(3,1)  = 2.0d0
      ce(3,2)  = 2.0d0
      ce(3,3)  = 0.0d0
      ce(3,4)  = 0.0d0
      ce(3,5)  = 0.0d0
      ce(3,6)  = 2.0d0
      ce(3,7)  = 3.0d0
      ce(3,8)  = 0.04d0
      ce(3,9)  = 0.03d0
      ce(3,10) = 0.05d0
      ce(3,11) = 0.3d0
      ce(3,12) = 0.5d0
      ce(3,13) = 0.4d0

      ce(4,1)  = 2.0d0
      ce(4,2)  = 2.0d0
      ce(4,3)  = 0.0d0
      ce(4,4)  = 0.0d0
      ce(4,5)  = 0.0d0
      ce(4,6)  = 2.0d0
      ce(4,7)  = 3.0d0
      ce(4,8)  = 0.03d0
      ce(4,9)  = 0.05d0
      ce(4,10) = 0.04d0
      ce(4,11) = 0.2d0
      ce(4,12) = 0.1d0
      ce(4,13) = 0.3d0

      ce(5,1)  = 5.0d0
      ce(5,2)  = 4.0d0
      ce(5,3)  = 3.0d0
      ce(5,4)  = 2.0d0
      ce(5,5)  = 0.1d0
      ce(5,6)  = 0.4d0
      ce(5,7)  = 0.3d0
      ce(5,8)  = 0.05d0
      ce(5,9)  = 0.04d0
      ce(5,10) = 0.03d0
      ce(5,11) = 0.1d0
      ce(5,12) = 0.3d0
      ce(5,13) = 0.2d0

      c1 = 1.4d0
      c2 = 0.4d0
      c3 = 0.1d0
      c4 = 1.0d0
      c5 = 1.4d0

      bt = dsqrt(0.5d0)

      dnxm1 = 1.0d0 / dble(grid_points(1)-1)
      dnym1 = 1.0d0 / dble(grid_points(2)-1)
      dnzm1 = 1.0d0 / dble(grid_points(3)-1)

      c1c2 = c1 * c2
      c1c5 = c1 * c5
      c3c4 = c3 * c4
      c1345 = c1c5 * c3c4

      conz1 = (1.0d0-c1c5)

      tx1 = 1.0d0 / (dnxm1 * dnxm1)
      tx2 = 1.0d0 / (2.0d0 * dnxm1)
      tx3 = 1.0d0 / dnxm1

      ty1 = 1.0d0 / (dnym1 * dnym1)
      ty2 = 1.0d0 / (2.0d0 * dnym1)
      ty3 = 1.0d0 / dnym1
      
      tz1 = 1.0d0 / (dnzm1 * dnzm1)
      tz2 = 1.0d0 / (2.0d0 * dnzm1)
      tz3 = 1.0d0 / dnzm1

      dx1 = 0.75d0
      dx2 = 0.75d0
      dx3 = 0.75d0
      dx4 = 0.75d0
      dx5 = 0.75d0

      dy1 = 0.75d0
      dy2 = 0.75d0
      dy3 = 0.75d0
      dy4 = 0.75d0
      dy5 = 0.75d0

      dz1 = 1.0d0
      dz2 = 1.0d0
      dz3 = 1.0d0
      dz4 = 1.0d0
      dz5 = 1.0d0

      dxmax = dmax1(dx3, dx4)
      dymax = dmax1(dy2, dy4)
      dzmax = dmax1(dz2, dz3)

      dssp = 0.25d0 * dmax1(dx1, dmax1(dy1, dz1) )

      c4dssp = 4.0d0 * dssp
      c5dssp = 5.0d0 * dssp

      dttx1 = dt*tx1
      dttx2 = dt*tx2
      dtty1 = dt*ty1
      dtty2 = dt*ty2
      dttz1 = dt*tz1
      dttz2 = dt*tz2

      c2dttx1 = 2.0d0*dttx1
      c2dtty1 = 2.0d0*dtty1
      c2dttz1 = 2.0d0*dttz1

      dtdssp = dt*dssp

      comz1  = dtdssp
      comz4  = 4.0d0*dtdssp
      comz5  = 5.0d0*dtdssp
      comz6  = 6.0d0*dtdssp

      c3c4tx3 = c3c4*tx3
      c3c4ty3 = c3c4*ty3
      c3c4tz3 = c3c4*tz3

      dx1tx1 = dx1*tx1
      dx2tx1 = dx2*tx1
      dx3tx1 = dx3*tx1
      dx4tx1 = dx4*tx1
      dx5tx1 = dx5*tx1
      
      dy1ty1 = dy1*ty1
      dy2ty1 = dy2*ty1
      dy3ty1 = dy3*ty1
      dy4ty1 = dy4*ty1
      dy5ty1 = dy5*ty1
      
      dz1tz1 = dz1*tz1
      dz2tz1 = dz2*tz1
      dz3tz1 = dz3*tz1
      dz4tz1 = dz4*tz1
      dz5tz1 = dz5*tz1

      c2iv  = 2.5d0
      con43 = 4.0d0/3.0d0
      con16 = 1.0d0/6.0d0
      
      xxcon1 = c3c4tx3*con43*tx3
      xxcon2 = c3c4tx3*tx3
      xxcon3 = c3c4tx3*conz1*tx3
      xxcon4 = c3c4tx3*con16*tx3
      xxcon5 = c3c4tx3*c1c5*tx3

      yycon1 = c3c4ty3*con43*ty3
      yycon2 = c3c4ty3*ty3
      yycon3 = c3c4ty3*conz1*ty3
      yycon4 = c3c4ty3*con16*ty3
      yycon5 = c3c4ty3*c1c5*ty3

      zzcon1 = c3c4tz3*con43*tz3
      zzcon2 = c3c4tz3*tz3
      zzcon3 = c3c4tz3*conz1*tz3
      zzcon4 = c3c4tz3*con16*tz3
      zzcon5 = c3c4tz3*c1c5*tz3

      return
      end
