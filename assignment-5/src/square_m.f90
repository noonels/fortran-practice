module square_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_m, only: shape_t
    use strff, only: hanging_indent, to_string, NEWLINE

    implicit none
    private
    public :: square_t

    type, extends(shape_t) :: square_t
        private
        real :: width
    contains
        private
        procedure, public :: to_string => square_to_string
        procedure, public :: calculate_area => calculate_area
    end type

    interface square_t
        module procedure constructor
    end interface
contains
    pure function constructor(width) result(square)
        real, intent(in) :: width
        type(square_t) :: square

        square%width = width
    end function

    pure function square_to_string(self) result(string)
        class(square_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
                "square_t(" // NEWLINE &
                        // "width = " // to_string(self%width), &
                4) // NEWLINE &
                // ")"
    end function

    pure function calculate_area(self) result(area)
        class(square_t), intent(in) :: self
        real :: area
        area = self%width * self%width
    end function
end module
