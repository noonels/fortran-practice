module integer_list_m
    implicit none
    private
    public :: integer_list_t, visitor_t

    type :: list_node_t
        private
        integer :: val
        type(list_node_t), pointer :: next => null()
    end type

    type :: integer_list_t
        private
        type(list_node_t), pointer :: head => null()
    contains
        private
        procedure, public :: prepend
        procedure, public :: foreach
    end type

    type, abstract :: visitor_t
    contains
        private
        procedure(visit_i), deferred, public :: visit
    end type

    abstract interface
        subroutine visit_i(self, item)
            import visitor_t
            implicit none
            class(visitor_t), intent(inout) :: self
            integer, intent(inout) :: item
        end subroutine
    end interface
contains
    subroutine prepend(self, item)
        class(integer_list_t), intent(inout) :: self
        integer, intent(in) :: item

        type(list_node_t), pointer :: new

        if (associated(self%head)) then
            allocate (new)
            new%val = item
            new%next => self%head
            self%head => new
        else
            allocate (self%head)
            self%head%val = item
        end if
    end subroutine

    subroutine foreach(self, visitor)
        class(integer_list_t), intent(inout) :: self
        class(visitor_t), intent(inout) :: visitor

        type(list_node_t), pointer :: cursor

        cursor => self%head
        do while (associated(cursor))
            call visitor%visit(cursor%val)
            cursor => cursor%next
        end do
    end subroutine
end module

module list_operator_m
    use integer_list_m, only: visitor_t

    implicit none
    private
    public :: printer_t, squarer_t, summer_t

    type, extends(visitor_t) :: printer_t
    contains
        private
        procedure, public :: visit => printer_visit
    end type

    type, extends(visitor_t) :: squarer_t
    contains
        private
        procedure, public :: visit => squarer_visit
    end type

    type, extends(visitor_t) :: summer_t
        integer :: sum = 0
    contains
        private
        procedure, public :: visit => summer_visit
        procedure, public :: get_sum => get_sum
    end type

contains
    subroutine printer_visit(self, item)
        class(printer_t), intent(inout) :: self
        integer, intent(inout) :: item

        print *, item
    end subroutine

    subroutine squarer_visit(self, item)
        class(squarer_t), intent(inout) :: self
        integer, intent(inout) :: item

        item = item*item
    end subroutine

    subroutine summer_visit(self, item)
        class(summer_t), intent(inout) :: self
        integer, intent(inout) :: item

        self%sum = self%sum + item
    end subroutine

    integer function get_sum(self)
        class(summer_t), intent(inout) :: self
        get_sum = self%sum
    end function
end module

program main
    use integer_list_m, only: integer_list_t
    use list_operator_m, only: printer_t, squarer_t, summer_t

    implicit none

    type(integer_list_t) :: list
    type(printer_t) :: printer
    type(squarer_t) :: squarer
    type(summer_t) :: summer

    call list%prepend(2)
    call list%prepend(3)
    call list%prepend(5)

    call list%foreach(printer)

    call list%foreach(squarer)

    call list%foreach(printer)

    call list%foreach(summer)
    print *, summer%get_sum()
end program
