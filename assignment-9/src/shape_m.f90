module shape_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: shape_t

    type, abstract :: shape_t
    contains
        private
        procedure(to_string_i), deferred, public :: to_string
        procedure(scale_i), deferred, public :: scale
        generic, public :: operator(*) => scale
        procedure(area_i), deferred, public :: area
        procedure(has_curves_i), deferred, public :: has_curves
    end type

    abstract interface
        pure function to_string_i(self) result(string)
            import :: shape_t, varying_string

            implicit none

            class(shape_t), intent(in) :: self
            type(varying_string) :: string
        end function

        function scale_i(self, factor) result(scaled)
            import :: shape_t

            implicit none

            class(shape_t), intent(in) :: self
            real, intent(in) :: factor
            class(shape_t), allocatable :: scaled
        end function

        pure function area_i(self) result(area)
            import :: shape_t

            implicit none

            class(shape_t), intent(in) :: self
            real :: area
        end function

        pure function has_curves_i(self) result(has_curves)
            import :: shape_t

            implicit none

            class(shape_t), intent(in) :: self
            logical :: has_curves
        end function
    end interface
end module
