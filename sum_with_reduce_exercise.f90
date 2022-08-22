module reduce_m
  implicit none

  abstract interface
    pure function combiner(x, y) result(z)
      integer, intent(in) :: x
      integer, intent(in) :: y
      integer :: z
    end function
  end interface
contains
  pure recursive function reduce(vals, accumulator, init) result(combined)
    integer, intent(in) :: vals(:)
    procedure(combiner) :: accumulator
    integer, intent(in) :: init
    integer :: combined

    if (size(vals) == 1) then
      combined = accumulator(init, vals(1))
    else
      combined = reduce(vals(2:), accumulator, accumulator(init, vals(1)))
    end if
  end function
end module

program main
  use reduce_m, only: reduce

  implicit none

  integer :: answer

  answer = sum_([1, 2, 3, 4])

  if (answer == 10) then
    print *, "You got it! :)"
  else
    print *, answer
    print *, "Not quite right yet. :("
  end if
contains
  pure function sum_(numbers) result(total)
    integer, intent(in) :: numbers(:)
    integer ::total

    total = reduce(numbers, add, 0)
  end function
  
  pure function add(x, y) result(z)
    integer, intent(in) :: x
    integer, intent(in) :: y
    integer :: z
    z = x + y
  end function
end program
