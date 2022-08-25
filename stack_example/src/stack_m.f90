module stack_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, NOT_FOUND
    use fallible_integer_m, only: fallible_integer_t

    implicit none
    private
    public :: fallible_stack_t, stack_t

    type :: stack_t
        private
        integer, dimension(4096) :: stack
        integer :: depth_ = 0
    contains
        private
        procedure, public :: empty
        procedure, public :: top
        procedure, public :: pop
        procedure, public :: push
        procedure, public :: depth
    end type

    type :: fallible_stack_t
        private
        type(stack_t) :: stack_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: stack
        procedure, public :: errors
    end type


    interface stack_t
        module procedure stack_constructor
    end interface

    interface fallible_stack_t
        module procedure from_stack
        module procedure from_errors
    end interface


    character(len=*), parameter :: MODULE_NAME = "stack_m"
contains
    function stack_constructor() result(empty_stack)
        type(stack_t) :: empty_stack

    end function

    function empty(self)
        class(stack_t), intent(in) :: self
        logical :: empty

        empty = self%depth_ == 0
    end function

    function top(self)
        class(stack_t), intent(in) :: self
        type(fallible_integer_t) :: top

        if (self%empty()) then
            top = fallible_integer_t(error_list_t(fatal_t( &
                NOT_FOUND, &
                module_t(MODULE_NAME), &
                procedure_t("top"), &
                "Asked for top of an empty stack.")))
        else
            top = fallible_integer_t(self%stack(self%depth_))
        end if
    end function

    function pop(self) result(popped)
        class(stack_t), intent(inout) :: self
        type(fallible_stack_t) :: popped
        integer, dimension(:), allocatable :: tmp

        if (self%empty()) then
            popped = fallible_stack_t(error_list_t(fatal_t( &
                module_t(MODULE_NAME), &
                procedure_t("pop"), &
                "Attempted to pop an empty stack.")))
        else
            self%depth_ = self%depth_ - 1
            popped = fallible_stack_t(self)
        end if
    end function

    function push(self, top) result(pushed)
        class(stack_t), intent(inout) :: self
        integer, intent(in) :: top
        type(stack_t) :: pushed
        integer, dimension(:), allocatable :: tmp

        self%depth_ = self%depth_ + 1
        self%stack(self%depth_) = top
        pushed = self
    end function

    function depth(self)
        class(stack_t), intent(in) :: self
        integer :: depth

        depth = self%depth_
    end function

    function from_stack(stack) result(fallible_stack)
        type(stack_t), intent(in) :: stack
        type(fallible_stack_t) :: fallible_stack

        fallible_stack%stack_ = stack
    end function

    function from_errors(errors) result(fallible_stack)
        type(error_list_t), intent(in) :: errors
        type(fallible_stack_t) :: fallible_stack

        fallible_stack%errors_ = errors
    end function

    function failed(self)
        class(fallible_stack_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function stack(self)
        class(fallible_stack_t), intent(in) :: self
        type(stack_t) :: stack

        stack = self%stack_
    end function

    function errors(self)
        class(fallible_stack_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
