program main
  use ftoc
  implicit none

  character(len=32) :: args
  real              :: temp
  integer           :: ierr

  call banner()

  call getinput()
  
  call blankline(.true.)
  
  call display(temp)  

  contains

    ! Display banner message
    subroutine banner()
      write (*, '(A)', advance='no') "Temperature converter ... "
    end subroutine

    ! Get the command line input
    subroutine getinput()
      call get_command_argument(1, args, STATUS=ierr)

      if (ierr .ne. 0) then
        call usage(.false.)
        stop
      else 
        read(args, *, iostat=ierr) temp
        if (ierr .ne. 0) then
          call usage(.true.)
          stop
        end if
      end if
    end subroutine

    ! Usage menu
    subroutine usage(nan)
      logical, intent(in) :: nan

      if (nan) then
        ! call blankline(.false.)
        call blankline(.false.)
        write (*, '(A)') "*** Not a number ***"
      end if

      call blankline(.false.)
      write (*, '(A)') "Usage: ftoc <number>"
      write (*, '(A)') "EG: ftoc 10.32"
      write (*, '(A)') "EG: ftoc 10."
    end subroutine

    ! Print a blank line or continue a message with a blank line
    subroutine blankline(proceed)
      logical, intent(in) :: proceed
      
      if (proceed) then
        write(*, '(A)') "Calculating"
        write(*, '(A)') " "
      else
        write(*, '(A)') " "
      end if
    end subroutine

    ! Perform the math and display the results
    subroutine display(t)
      real, intent(in)  :: t
      real              :: c,f,k

      c = f_to_c(temp)
      write (*, '(F10.2,A)') c, "C (F to C)"
      f = c_to_f(temp)
      write (*, '(F10.2,A)') f, "F (C to F)"
      k = c_to_k(temp)
      write (*, '(F10.2,A)') k, "K (C to K)"
      k = f_to_k(temp)
      write (*, '(F10.2,A)') k, "K (F to K)"
      c = k_to_c(temp)
      write (*, '(F10.2,A)') c, "C (K to C)"
      f = k_to_f(temp)
      write (*, '(F10.2,A)') f, "F (K to F)"
    end subroutine
end program main