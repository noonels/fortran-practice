module rectangle_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_m, only: shape_t
    use strff, only: hanging_indent, to_string, NEWLINE

    implicit none
    private
    public :: rectangle_t

    type, extends(shape_t) :: rectangle_t
        private
        real :: width
        real :: height
    contains
        private
        procedure, public :: to_string => rectangle_to_string
        procedure, public :: calculate_area => calculate_area
    end type

    interface rectangle_t
        module procedure constructor
    end interface
contains
    pure function constructor(height, width) result(rectangle)
        real, intent(in) :: height
        real, intent(in) :: width
        type(rectangle_t) :: rectangle

        rectangle%height = height
        rectangle%width = width
    end function

    pure function rectangle_to_string(self) result(string)
        class(rectangle_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
            "rectangle_t(" // NEWLINE &
                    // "height = " // to_string(self%height) // "," // NEWLINE &
                    // "width = " // to_string(self%width), &
            4) // NEWLINE &
            // ")"
    end function

    pure function calculate_area(self) result(area)
        class(rectangle_t), intent(in) :: self
        real :: area
        area = self%height * self%width
    end function
end module
