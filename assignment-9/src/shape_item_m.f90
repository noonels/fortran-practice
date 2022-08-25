module shape_item_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_m, only: shape_t
    use shape_filterer_m, only: shape_filterer_t
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
        procedure :: scale
        generic, public :: operator(*) => scale
        procedure, public :: area
        procedure, public :: satisfies
    end type

    interface shape_item_t
        module procedure constructor
    end interface
contains
    function constructor(shape) result(shape_item)
        class(shape_t), intent(in) :: shape
        type(shape_item_t) :: shape_item

        allocate(shape_item%shape, source = shape)
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

    impure elemental function scale(self, factor) result(scaled)
        class(shape_item_t), intent(in) :: self
        real, intent(in) :: factor
        type(shape_item_t) :: scaled

        allocate(scaled%shape, source = self%shape * factor)
    end function

    elemental function area(self)
        class(shape_item_t), intent(in) :: self
        real :: area

        area = self%shape%area()
    end function

    elemental function satisfies(self, filterer)
        class(shape_item_t), intent(in) :: self
        class(shape_filterer_t), intent(in) :: filterer
        logical :: satisfies

        satisfies = filterer%matches(self%shape)
    end function
end module
