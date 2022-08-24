module leaf_m
    use iso_varying_string, only: varying_string
    use strff, only: to_string
    use tree_m, only: tree_t

    implicit none
    private
    public :: leaf_t

    type, extends(tree_t) :: leaf_t
        private
        integer :: value_
    contains
        private
        procedure, public :: to_string => leaf_to_string
        procedure, public :: total => leaf_total
    end type

    interface leaf_t
        module procedure constructor
    end interface
contains
    pure function constructor(value_) result(leaf)
        integer, intent(in) :: value_
        type(leaf_t) :: leaf

        leaf%value_ = value_
    end function

    pure function leaf_to_string(self) result(string)
        class(leaf_t), intent(in) :: self
        type(varying_string) :: string

        string = to_string(self%value_)
    end function

    pure function leaf_total(self)
        class(leaf_t), intent(in) :: self
        integer :: leaf_total
        leaf_total = self%value_
    end function
end module
