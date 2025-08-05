program test_ftoc
    use VerySimpleTestFramework
    use ftoc

    implicit none
    
    call run_tests()

contains
    subroutine run_tests()
        real :: t, tol
        t = 0.
        tol = 1. * 10.**-2

        call suite("Temp Converter")
        call test("C to F 0 Tolerance 0.01")
        call assert_equals(c_to_f(t), 32., tol)

        call test("F to C 0  Tolerance 0.01")
        call assert_equals(f_to_c(t), -17.78, tol)

        call test("C to K 0  Tolerance 0.01")
        call assert_equals(c_to_k(t), 273.15, tol)

        call test("F to K 0  Tolerance 0.01")
        call assert_equals(f_to_k(t), 255.37, tol)

        call test("K to C 0  Tolerance 0.01")
        call assert_equals(k_to_c(t), -273.15, tol)

        call test("K to F 0  Tolerance 0.01")
        call assert_equals(k_to_f(t), -459.67, tol)

        call results()
    end subroutine
end program test_ftoc
