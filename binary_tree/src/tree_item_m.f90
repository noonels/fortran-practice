module tree_item_m
    use iso_varying_string, only: varying_string
    use tree_m, only: tree_t

    implicit none
    private
    public :: tree_item_t

    type :: tree_item_t
        private
        class(tree_t), allocatable :: tree
    contains
        private
        procedure, public :: to_string
        procedure, public :: total
    end type

    interface tree_item_t
        module procedure constructor
    end interface
contains
    function constructor(tree) result(tree_item)
        class(tree_t), intent(in) :: tree
        type(tree_item_t) :: tree_item

        allocate(tree_item%tree, source = tree)
    end function

    pure recursive function to_string(self) result(string)
        class(tree_item_t), intent(in) :: self
        type(varying_string) :: string

        string = self%tree%to_string()
    end function

    elemental recursive function total(self)
        class(tree_item_t), intent(in) :: self
        integer :: total

        total = self%tree%total()
    end function
end module
