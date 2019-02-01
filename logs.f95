!Program Name: logs.f95
!Author      : Jordan Evans (0964044)
!Due Date    : Feb 1, 2019 

! getLOGdata()
! a subroutine to retrieve data from the user to be used in volume more_calculations
! pre:  scaling_diameter, diameter_inside_bark, total_length, kerf -> all undefined
! post: scaling_diameter     >  0
!       diameter_inside_bark >= 0
!       total_length         >  0
!       kerf                 == 1 or 0
subroutine getLOGdata(scaling_diameter, diameter_inside_bark, total_length, kerf)      
  real, intent(out) :: scaling_diameter, diameter_inside_bark, total_length, kerf

  ! Get scaling diameter of log. Do not allow values <= 0.
  DO WHILE (1 == 1)
    print*,""
    print*,"Enter diameter inside bark at log's small end (scaling diameter) (inches): "
    read(*,*) scaling_diameter
    IF (scaling_diameter <= 0.0) then
      print*,"Scaling diameter must be > 0."
    ELSE
      exit
    END IF
  END DO
  
  ! Get diameter inside bark of log. Do not allow values < 0.
  DO WHILE (1 == 1)
    print*,""
    print*,"Enter diameter inside bark at log's large end or 0 if the default taper option (1/2"") is to be used (inches): "
    read(*,*) diameter_inside_bark
    IF (diameter_inside_bark < 0.0) then
      print*,"Diameter inside bark must be >= 0."
    ELSE
      exit
    END IF
  END DO
  
  ! Get log's total length. Do not allow values <= 0.
  DO WHILE (1 == 1)
    print*,""
    print*,"Enter log's total length (feet): "
    read(*,*) total_length
    IF (total_length <= 0.0) then
      print*,"Total log length must be > 0."
    ELSE
      exit
    END IF
  END DO
  
  ! Get kerf value. Do not allow values other than 0 or 1.
  DO WHILE (1 == 1)
    print*,""
    print*,"Enter 1 if 1/4"" saw kerf is assumed or 0 if 1/8"" saw kerf is assumed: "
    read(*,*) kerf
    IF (kerf /= 0.0 .and. kerf /= 1.0) then
      print*,"KERF value must be either 1 or 0."
    ELSE
      exit
    END IF
  END DO
end subroutine getLOGdata


! printResults()
! a subroutine to print the equation variables and results
! pre:  sd, dib, tl, kerf, bv, v -> all defined values of type "real"
! post: no changes made to variables.
subroutine printResults(sd, dib, tl, kerf, bv, v)
  real, intent(in) :: sd, dib, tl, kerf, bv, v

  print*,""
  print*,"      ~~~~~~~~  RESULTS  ~~~~~~~~      "
  write(*, '("    Scaling Diameter (in): ",f10.2)') sd
  write(*, '("Diameter Inside Bark (in): ",f10.2)') dib
  write(*, '("  Total Log Length (feet): ",f10.2)') tl
  write(*, '("                     KERF: ",f10.2)') kerf
  print*,""
  write(*, '("             Board Volume: ",f10.2)') bv
  write(*, '("             Volume (m^3): ",f10.2)') v
end subroutine printResults


! calcLOGjclark()
! a subroutine to calculate the board foot volume of sawlogs by the international rule.
! pre:  diameter_small   >  0
!       diameter_large   >= 0
!       total_log_length >  0
!       KERF             == 1 or 0
!       VOLUME           -> undefined
! post: diamteter_small, diameter_large, total_log_length, KERF -> all unchanged.
!       VOLUME -> the calculated board foot volume of the log
subroutine calcLOGjclark(diameter_small, diameter_large, total_log_length, KERF, VOLUME)
  real, intent(out) :: VOLUME
  real, intent(in)  :: diameter_small, diameter_large, total_log_length, KERF

  real :: taper_rate, segment_length, segment_scaling_diameter, DEX, UADD, log_segment_amt, DC

  ! Define variables to necessary defaults
  VOLUME          = 0 
  taper_rate      = 0.5
  segment_length  = 0.0
  segment_scaling_diameter = 0.0
  DEX             = 0.0
  UADD            = 0.0
  log_segment_amt = 0.0
  DC              = 0.0

  ! If diameter_large == 0, use default taper rate of 0.5. If diameter_large > 0, calculate taper rate.
  IF(diameter_large < 0.0) RETURN
  IF(diameter_large > 0.0) taper_rate = 4.0 * (diameter_large - diameter_small) / total_log_length

  ! Determine how many full 4 foot segments are in the log, and the total length of all the segments.
  DO i = 1,20
    IF (total_log_length - 4 * i < 0.0) then
      log_segment_amt = i - 1
      segment_length = 4 * log_segment_amt
      exit
    END IF
  END DO

  ! Calculates the scaling diameter at the end of the 4 foot segments according to the taper rate
  segment_scaling_diameter = diameter_small + (taper_rate / 4.0) * (total_log_length - segment_length)

  ! Determines how many full feet of length are in the left over segment of the log and calculates the volume of it
  DO i = 1,4
    IF (segment_length - total_log_length + i > 0.0) then
      DEX  = diameter_small + (taper_rate / 4.0) * (total_log_length - segment_length - (i - 1))
      UADD = 0.055 * (i - 1) * DEX * DEX - 0.1775 * (i - 1) * DEX
      exit
    END IF
  END DO

  ! Calculates volume in the portion of the log containing whole 4 foot segments
  DO i = 1,INT(log_segment_amt)
    DC = segment_scaling_diameter + taper_rate * (i - 1)
    VOLUME = VOLUME + 0.22 * DC * DC - 0.71 * DC
  END DO

  ! Adds extra log ending volume to the log segments volume
  VOLUME = VOLUME + UADD

  ! If KERF > 0, international 1/8 inch volume will be converted to international 1/4 inch volume
  IF(KERF > 0) VOLUME = 0.905 * VOLUME

  RETURN
end subroutine calcLOGjclark


! calcLOGvolume()
! a subroutine to calculate metric volume of a log.
! pre:  diameter_small   >  0
!       diameter_large   >= 0
!       total_log_length >  0
!       VOLUME           -> undefined
! post: diamteter_small, diameter_large, total_log_length -> all unchanged.
!       VOLUME -> the calculated metric volume of the log (m^3)
subroutine calcLOGvolume(diameter_small, diameter_large, total_log_length, VOLUME)
  real, intent(out) :: VOLUME
  real, intent(in)  :: diameter_small, diameter_large, total_log_length

  real :: PI, calc_diameter_large, area_small, area_large, metres_radius_small, metres_radius_large, metres_log_length

  ! Determine if default taper is being used. If so, calculate. If not, assign as usual 
  calc_diameter_large = diameter_large
  IF(diameter_large == 0) calc_diameter_large = diameter_small + ((total_log_length/4) * .5)

  PI                  = 3.14159265

  ! Convert imperial diameters to metric radii
  metres_radius_small = (diameter_small / 39.37) / 2.0
  metres_radius_large = (calc_diameter_large / 39.37) / 2.0

  ! Convert imperial feet to metric metres
  metres_log_length   = (total_log_length / 3.2808) 

  ! Calculate area of small and large ends of the log (pi * r^2)
  area_small = PI * (metres_radius_small * metres_radius_small)
  area_large = PI * (metres_radius_large * metres_radius_large)

  ! Calculate metric volume.
  VOLUME = ((area_small + area_large) / 2) * metres_log_length

  RETURN   
end subroutine calcLOGvolume


PROGRAM test
integer :: more_calculations
real :: scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume, metric_volume

more_calculations = 1

DO WHILE (more_calculations == 1)

  ! Get user's input for log variabes.
  call getLOGdata(scaling_diameter, diameter_inside_bark, total_length, kerf)

  ! Calculate board volume and assign result to 'board_volume'
  call calcLOGjclark(scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume)

  ! Calculate metric volume and assign result to 'metric volume'
  call calcLOGvolume(scaling_diameter, diameter_inside_bark, total_length, metric_volume)
  
  ! Print initial variables as well as calculated results (board_volume, metric_volume)
  call printResults(scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume, metric_volume)

  ! Loop to determine if the user wants to calculate volume for another set of log variables
  DO WHILE (1 == 1)
    print*,""
    print*,"Would you like to perform another calculation? (1=yes, 0=no): "
    read(*,*) more_calculations
    IF (more_calculations /= 0.0 .and. more_calculations /= 1.0) then
      print*,"Must enter either 1 (yes) or 0 (no)."
    ELSE
      exit
    END IF
  END DO
END DO

END PROGRAM test