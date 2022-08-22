program recursive_sum_exercise
    implicit none

    integer, parameter :: THE_NUMBERS(*) = [2, 3, 5, 7, 11]
    integer :: answer

    answer = sum_(THE_NUMBERS)

    if (answer == 28) then
        print *, "You got it! :)"
    else
        print *, answer
        print *, "Not quite right yet. :("
    end if
contains
    recursive function sum_(numbers) result(total)
        integer, intent(in) :: numbers(:)
        integer :: total
        if (size(numbers) == 0) then
            total = 0
        else
            total = numbers(1) + sum_(numbers(2:))
        end if

    end function
end program
