module ftoc
  implicit none
  private

  ! Export to public the functions
  public :: f_to_c, c_to_f, c_to_k, k_to_c, f_to_k, k_to_f
contains
  ! Fahrenheit to Celsius
  function f_to_c(f) result(c)
    real, intent(in)   :: f
    real                  :: c

    c = 5. / 9. * (f - 32)
  end function

  ! Celsius to Fahrenheit
  function c_to_f(c) result(f)
    real, intent(in)  :: c
    real              :: f

    f = (c * (9. / 5.)) + 32.
  end function

  ! Celsius to Kelvin
  function c_to_k(c) result(k)
    real, intent(in)  :: c
    real              :: k

    k = c + 273.15
  end function

  ! Kelvin to Celsius
  function k_to_c(k) result (c)
    real, intent(in)  :: k
    real              :: c

    c = k - 273.15
  end function

  ! Fahrenheit to Kelvin
  function f_to_k(f) result(k)
    real, intent(in)  :: f
    real              :: k
    real              :: c

    c = f_to_c(f)
    k = c_to_k(c)
  end function

  ! Kelvin to Fahrenheit
  function k_to_f(k) result(f)
    real, intent(in)  :: k
    real              :: f
    real              :: c

    c = k_to_c(k)
    f = c_to_f(c)
  end function
end module ftoc
