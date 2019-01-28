      ! Subroutine to gather log size data to use in calculations
      subroutine getLOGdata(scaling_diameter, diameter_inside_bark, total_length, kerf)      
        real, intent(out) :: scaling_diameter, diameter_inside_bark, total_length, kerf

        print*,"Enter diameter inside bark at log's small end (scaling diameter) (inches): "
        read(*,*) scaling_diameter

        print*,"Enter diameter inside bark at log's large end or 0 if the default taper option (1/2"") is to be used (inches): "
        read(*,*) diameter_inside_bark

        print*,"Enter log's total length (feet): "
        read(*,*) total_length
        
        print*,"Enter 1 if 1/4"" saw kerf is assumed or 0 if 1/8"" saw kerf is assumed:  "
        read(*,*) kerf
      end subroutine getLOGdata

      ! A testing funciton used to print the input variables
      subroutine TEST_PRINT(sd, dib, tl, kerf)
        real, intent(in) :: sd, dib, tl, kerf

        print*,"############## TEST PRINT ##############"
        print*,"# Scaling Diameter: ",   sd," #"
        print*,"#              DIB: ",  dib," #"
        print*,"#     Total Length: ",   tl," #"
        print*,"#         Saw KERF: ", kerf," #"
        print*,"########################################"
      end subroutine TEST_PRINT


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

        print*,"Taper Rate=",taper_rate

        DO i = 1,20
          IF (total_log_length - 4 * i < 0.0) then
            log_segment_amt = i - 1
            segment_length = 4 * log_segment_amt
            exit
          END IF
        END DO

        print*,"segment_length: ",segment_length

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

      subroutine calcLOGvolume(diameter_small, diameter_large, total_log_length, KERF, VOLUME)
        real, intent(out) :: VOLUME
        real, intent(in)  :: diameter_small, diameter_large, total_log_length, KERF

        real :: PI, area_small, area_large, metres_radius_small, metres_radius_large, metres_log_length

        PI                  = 3.14159265
        metres_radius_small = (diameter_small / 39.37) / 2.0
        metres_radius_large = (diameter_large / 39.37) / 2.0
        metres_log_length   = (total_log_length / 3.2808) 

        area_small = PI * metres_radius_small * metres_radius_small
        area_large = PI * metres_radius_large * metres_radius_large

        VOLUME = ((area_small + area_large) / 2) * metres_log_length

        RETURN   
      end subroutine calcLOGvolume

      PROGRAM test
      
      real :: scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume, metric_volume
      
      call getLOGdata(scaling_diameter, diameter_inside_bark, total_length, kerf)
      
      call TEST_PRINT(scaling_diameter, diameter_inside_bark, total_length, kerf)

      call calcLOGjclark(scaling_diameter, diameter_inside_bark, total_length, kerf, board_volume)

      call calcLOGvolume(scaling_diameter, diameter_inside_bark, total_length, kerf, metric_volume)
      
      print*,"BOARD  VOLUME: ",board_volume
      print*,"METRIC VOLUME: ",metric_volume
      END PROGRAM test