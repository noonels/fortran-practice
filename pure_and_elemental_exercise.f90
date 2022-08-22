program  pure_and_elemental_exercise
    implicit none

    integer, parameter :: PRIMES(*) = [2, 3, 5, 7, 11]
    integer, parameter :: REAL_ANSWER = 1872
    integer :: answer

    ! Replace '42' with an expression using elemental functions
    ! that calculates the equivalent of the following:
    ! answer = 0
    ! do i = 1, size(PRIMES)
    !   answer = answer + (primes(i) * 3)**2
    ! end do
    answer = sum(square(mult_by_three(PRIMES)))

    if (answer == REAL_ANSWER) then
        print *, "You got it! :)"
    else
        print *, answer
        print *, "Not passing yet :("
    end if
contains
    ! put your functions down here
    elemental integer function mult_by_three(x)
        integer, intent(in) :: x
        mult_by_three = x * 3
    end function

    elemental integer function square(x)
        integer, intent(in) :: x
        square = x ** 2
    end function
end program
