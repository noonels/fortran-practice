module stack_input_m
    use stack_m, only: stack_t
    use vegetables, only: input_t

    implicit none
    private
    public :: stack_input_t

    type, extends(input_t) :: stack_input_t
        private
        type(stack_t) :: stack_
    contains
        private
        procedure, public :: stack
    end type

    interface stack_input_t
        module procedure constructor
    end interface
contains
    function constructor(stack) result(stack_input)
        type(stack_t), intent(in) :: stack
        type(stack_input_t) :: stack_input

        stack_input%stack_ = stack
    end function

    function stack(self)
        class(stack_input_t), intent(in) :: self
        type(stack_t) :: stack

        stack = self%stack_
    end function
end module
