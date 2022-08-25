module fallible_integer_m
    use erloff, only: error_list_t

    implicit none
    private
    public :: fallible_integer_t

    type :: fallible_integer_t
        private
        integer :: value__
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: value_
        procedure, public :: errors
    end type

    interface fallible_integer_t
        module procedure from_value
        module procedure from_errors
    end interface
contains
    function from_value(value_) result(fallible_integer)
        integer, intent(in) :: value_
        type(fallible_integer_t) :: fallible_integer

        fallible_integer%value__ = value_
    end function

    function from_errors(errors) result(fallible_integer)
        type(error_list_t), intent(in) :: errors
        type(fallible_integer_t) :: fallible_integer

        fallible_integer%errors_ = errors
    end function

    function failed(self)
        class(fallible_integer_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function value_(self)
        class(fallible_integer_t), intent(in) :: self
        integer :: value_

        value_ = self%value__
    end function

    function errors(self)
        class(fallible_integer_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
