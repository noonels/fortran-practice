module circle_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_m, only: shape_t
    use strff, only: hanging_indent, to_string, NEWLINE

    implicit none
    private
    public :: circle_t

    type, extends(shape_t) :: circle_t
        private
        real :: radius
    contains
        private
        procedure, public :: to_string => circle_to_string
        procedure, public :: scale
    end type

    interface circle_t
        module procedure constructor
    end interface
contains
    pure function constructor(radius) result(circle)
        real, intent(in) :: radius
        type(circle_t) :: circle

        circle%radius = radius
    end function

    pure function circle_to_string(self) result(string)
        class(circle_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
            "circle_t(" // NEWLINE &
            // "radius = " // to_string(self%radius), &
            4) // NEWLINE &
            // ")"
    end function

    function scale(self, factor) result(scaled)
        class(circle_t), intent(in) :: self
        real, intent(in) :: factor
        class(shape_t), allocatable :: scaled

        allocate(scaled, source = circle_t(radius = self%radius * factor))
    end function
end module
