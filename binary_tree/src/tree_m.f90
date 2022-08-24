module tree_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: tree_t

    type, abstract :: tree_t
    contains
        private
        procedure(to_string_i), public, deferred :: to_string
    end type

    abstract interface
        pure function to_string_i(self) result(string)
            import :: tree_t, varying_string

            implicit none

            class(tree_t), intent(in) :: self
            type(varying_string) :: string
        end function
    end interface
end module
