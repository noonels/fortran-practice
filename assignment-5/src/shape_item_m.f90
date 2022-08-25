module shape_item_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_m, only: shape_t
    use strff, only: hanging_indent, NEWLINE

    implicit none
    private
    public :: shape_item_t

    type :: shape_item_t
        private
        class(shape_t), allocatable :: shape
    contains
        private
        procedure, public :: to_string
        procedure, public :: calculate_area
    end type

    interface shape_item_t
        module procedure constructor
    end interface
contains
    function constructor(shape) result(shape_item)
        class(shape_t), intent(in) :: shape
        type(shape_item_t) :: shape_item

        shape_item%shape = shape
    end function

    elemental function to_string(self) result(string)
        class(shape_item_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
            "shape_item_t(" // NEWLINE &
            // "shape = " // self%shape%to_string(), &
            4) // NEWLINE &
            // ")"
    end function

    elemental function calculate_area(self) result(area)
        class(shape_item_t), intent(in) :: self
        real :: area
        area = self%shape%calculate_area()
    end function
end module
