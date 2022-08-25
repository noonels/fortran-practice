module triangle_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_m, only: shape_t
    use strff, only: hanging_indent, to_string, NEWLINE

    implicit none
    private
    public :: triangle_t

    type, extends(shape_t) :: triangle_t
        private
        real :: base
        real :: height
    contains
        private
        procedure, public :: to_string => triangle_to_string
        procedure, public :: scale
    end type

    interface triangle_t
        module procedure constructor
    end interface
contains
    pure function constructor(base, height) result(triangle)
        real, intent(in) :: base
        real, intent(in) :: height
        type(triangle_t) :: triangle

        triangle%base = base
        triangle%height = height
    end function

    pure function triangle_to_string(self) result(string)
        class(triangle_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
            "triangle_t(" // NEWLINE &
            // "base = " // to_string(self%base) // "," // NEWLINE &
            // "height = " // to_string(self%height), &
            4) // NEWLINE &
            // ")"
    end function

    function scale(self, factor) result(scaled)
        class(triangle_t), intent(in) :: self
        real, intent(in) :: factor
        class(shape_t), allocatable :: scaled

        allocate(scaled, source = triangle_t(base = self%base, height = self%height * factor))
    end function
end module
