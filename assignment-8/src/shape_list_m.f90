module shape_list_m
    use iso_varying_string, only: varying_string, operator(//)
    use shape_item_m, only: shape_item_t
    use strff, only: hanging_indent, indent, join, NEWLINE

    implicit none
    private
    public :: shape_list_t

    type :: shape_list_t
        private
        type(shape_item_t), allocatable :: shapes(:)
    contains
        private
        procedure, public :: to_string
        procedure :: concat
        generic, public :: operator(//) => concat
        procedure :: scale
        generic, public :: operator(*) => scale
    end type

    interface shape_list_t
        module procedure constructor
    end interface
contains
    function constructor(shapes) result(shape_list)
        type(shape_item_t), intent(in) :: shapes(:)
        type(shape_list_t) :: shape_list

        allocate(shape_list%shapes, source = shapes)
    end function

    pure function to_string(self) result(string)
        class(shape_list_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
            "shape_list_t(" // NEWLINE &
            // "shapes = [" // NEWLINE &
            // indent(join(self%shapes%to_string(), "," // NEWLINE), 4) // NEWLINE &
            // "]", &
            4) // NEWLINE &
            // ")"
    end function

    function concat(lhs, rhs) result(combined)
        class(shape_list_t), intent(in) :: lhs
        type(shape_list_t), intent(in) :: rhs
        type(shape_list_t) :: combined

        allocate(combined%shapes, source = [lhs%shapes, rhs%shapes])
    end function

    function scale(self, factor) result(scaled)
        class(shape_list_t), intent(in) :: self
        real, intent(in) :: factor
        type(shape_list_t) :: scaled

        allocate(scaled%shapes, source = self%shapes * factor)

    end function
end module
