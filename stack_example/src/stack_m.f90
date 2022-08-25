module stack_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, NOT_FOUND
    use fallible_integer_m, only: fallible_integer_t

    implicit none
    private
    public :: fallible_stack_t, stack_t

    type :: stack_t
        private
        class(stack_entry_t), allocatable :: head
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

    type, abstract :: stack_entry_t
    contains
        private
        procedure(depth_i), public, deferred :: depth
    end type

    abstract interface
        function depth_i(self) result(depth)
            import :: stack_entry_t

            implicit none

            class(stack_entry_t), intent(in) :: self
            integer :: depth
        end function
    end interface

    type, extends(stack_entry_t) :: stack_empty_t
    contains
        private
        procedure, public :: depth => stack_empty_depth
    end type

    type, extends(stack_entry_t) :: stack_item_t
        private
        integer :: item_
        class(stack_entry_t), allocatable :: next_
    contains
        private
        procedure, public :: depth => stack_item_depth
        procedure, public :: item
        procedure, public :: next
    end type

    interface stack_t
        module procedure stack_constructor
    end interface

    interface fallible_stack_t
        module procedure from_stack
        module procedure from_errors
    end interface

    interface stack_item_t
        module procedure stack_item_constructor
    end interface

    character(len=*), parameter :: MODULE_NAME = "stack_m"
contains
    function stack_constructor() result(empty_stack)
        type(stack_t) :: empty_stack

        allocate(empty_stack%head, source = stack_empty_t())
    end function

    function empty(self)
        class(stack_t), intent(in) :: self
        logical :: empty

        empty = self%depth() == 0
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
            select type (head => self%head)
            type is (stack_item_t)
                top = fallible_integer_t(head%item())
            end select
        end if
    end function

    function pop(self) result(popped)
        class(stack_t), intent(in) :: self
        type(fallible_stack_t) :: popped

        if (self%empty()) then
            popped = fallible_stack_t(error_list_t(fatal_t( &
                    module_t(MODULE_NAME), &
                    procedure_t("pop"), &
                    "Attempted to pop an empty stack.")))
        else
            block
                type(stack_t) :: new_stack

                select type (head => self%head)
                type is (stack_item_t)
                    allocate(new_stack%head, source = head%next())
                    popped = fallible_stack_t(new_stack)
                end select
            end block
        end if
    end function

    function push(self, top) result(pushed)
        class(stack_t), intent(in) :: self
        integer, intent(in) :: top
        type(stack_t) :: pushed

        if (self%empty()) then
            allocate(pushed%head, source = stack_item_t(top, stack_empty_t()))
        else
            allocate(pushed%head, source = stack_item_t(top, self%head))
        end if
    end function

    function depth(self)
        class(stack_t), intent(in) :: self
        integer :: depth

        if (allocated(self%head)) then
            depth = self%head%depth()
        else
            depth = 0
        end if
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

    function stack_empty_depth(self) result(depth)
        class(stack_empty_t), intent(in) :: self
        integer :: depth

        associate(unused => self)
        end associate

        depth = 0
    end function

    function stack_item_constructor(item, next) result(stack_item)
        integer, intent(in) :: item
        class(stack_entry_t), intent(in) :: next
        type(stack_item_t) :: stack_item

        stack_item%item_ = item
        allocate(stack_item%next_, source = next)
    end function

    recursive function stack_item_depth(self) result(depth)
        class(stack_item_t), intent(in) :: self
        integer :: depth

        depth = 1 + self%next_%depth()
    end function

    function item(self)
        class(stack_item_t), intent(in) :: self
        integer :: item

        item = self%item_
    end function

    function next(self)
        class(stack_item_t), intent(in) :: self
        class(stack_entry_t), allocatable :: next

        allocate(next, source = self%next_)
    end function
end module
