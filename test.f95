      ! TODO
      !  - round output to 2 decimal places
      !  - prettify

      ! Subroutine to gather log size data to use in calculations
      subroutine getLOGdata(scaling_diameter, diameter_inside_bark, total_length, kerf)      
        real, intent(out) :: scaling_diameter, diameter_inside_bark, total_length, kerf

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

      ! A testing funciton used to print the input variables
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


      subroutine calcLOGjclark(diameter_small, diameter_large, total_log_length, KERF, VOLUME)
        real, intent(out) :: VOLUME
        real, intent(in)  :: diameter_small, diameter_large, total_log_length, KERF

        real :: taper_rate, segment_length, segment_scaling_diameter, DEX, UADD, log_segment_amt, DC

        VOLUME     = 0 
        taper_rate = 0.5
        segment_length = 0.0
        segment_scaling_diameter = 0.0
        DEX        = 0.0
        UADD       = 0.0
        log_segment_amt = 0.0
        DC         = 0.0

        IF(diameter_large < 0.0) RETURN
        IF(diameter_large > 0.0) taper_rate = 4.0 * (diameter_large - diameter_small) / total_log_length

        DO i = 1,20
          IF (total_log_length - 4 * i < 0.0) then
            log_segment_amt = i - 1
            segment_length = 4 * log_segment_amt
            exit
          END IF
        END DO

        segment_scaling_diameter = diameter_small + (taper_rate / 4.0) * (total_log_length - segment_length)

        DO i = 1,4
          IF (segment_length - total_log_length + i > 0.0) then
            DEX  = diameter_small + (taper_rate / 4.0) * (total_log_length - segment_length - (i - 1))
            UADD = 0.055 * (i - 1) * DEX * DEX - 0.1775 * (i - 1) * DEX
            exit
          END IF
        END DO

        DO i = 1,INT(log_segment_amt)
          DC = segment_scaling_diameter + taper_rate * (i - 1)
          VOLUME = VOLUME + 0.22 * DC * DC - 0.71 * DC
        END DO

        VOLUME = VOLUME + UADD

        IF(KERF > 0) VOLUME = 0.905 * VOLUME

        RETURN
      end subroutine calcLOGjclark

      subroutine calcLOGvolume(diameter_small, diameter_large, total_log_length,VOLUME)
        real, intent(out) :: VOLUME
        real, intent(in)  :: diameter_small, diameter_large, total_log_length

        real :: PI, calc_diameter_large, area_small, area_large, metres_radius_small, metres_radius_large, metres_log_length

        calc_diameter_large = diameter_large
        IF(diameter_large == 0) calc_diameter_large = diameter_small + ((total_log_length/4) * .5)


        PI                  = 3.14159265
        metres_radius_small = (diameter_small / 39.37) / 2.0
        metres_radius_large = (calc_diameter_large / 39.37) / 2.0
        metres_log_length   = (total_log_length / 3.2808) 

        area_small = PI * (metres_radius_small * metres_radius_small)
        area_large = PI * (metres_radius_large * metres_radius_large)

        VOLUME = ((area_small + area_large) / 2) * metres_log_length

        RETURN   
      end subroutine calcLOGvolume

      PROGRAM test
      
      integer :: more_calculations
      real :: scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume, metric_volume
      
      more_calculations = 1

      DO WHILE (more_calculations == 1)
        call getLOGdata(scaling_diameter, diameter_inside_bark, total_length, kerf)

        call calcLOGjclark(scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume)

        call calcLOGvolume(scaling_diameter, diameter_inside_bark, total_length, metric_volume)
        
        call printResults(scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume, metric_volume)

        
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