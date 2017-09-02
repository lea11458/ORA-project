      ! program that calculates the trajectory of a rocket 
	  PROGRAM rocket2 
      IMPLICIT NONE 
	  REAL :: g, th, v_y, v_0, y, v_x, pi, d_to_r, x
	  INTEGER :: t
	  
	  ! quite useless as g is already defined, but added for practice 
	  PRINT*, 'What is the acceleration due to gravity?'
	  READ*, g 
	  
	  !values of the acceleration due to gravity
	  g=9.81
	  ! value of the velocity at the starting point 
	  v_0=88
	  ! value of the angle 
	  th=45
	  ! an approximation of the value of pi
	  pi= 3.1415926
	  ! the conversion from degree to radian (Fortran works in radians and the formula here works in degrees)
	  d_to_r= pi/180
	  
	  PRINT*, '         TIME     V_Y              V_X              Y                X'
	  the_loop: DO t=0, 100, 1
	     ! calculate v_x (horizontal velocity)
		 v_x= v_0 * COS(th * d_to_r) * t
	     ! calculate the v_y (vertical velocity)
	     v_y= v_0 * SIN(th * d_to_r) - g * t
		 ! calculate the y (height)
		 y= v_0 * SIN(th * d_to_r) * t -0.5 * g * t **2
		 ! calculate the x (distance)
		 x= v_0 * COS(th * d_to_r) * t
		 ! print out each row in the table
		 PRINT*, t, v_y, v_x, y, x
       END DO the_loop  
	  
	  STOP
      END PROGRAM rocket2