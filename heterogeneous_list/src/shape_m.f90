module shape_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: shape_t

    type, abstract :: shape_t
    contains
        private
        procedure(to_string_i), deferred, public :: to_string
        procedure(calculate_area_i), deferred, public :: calculate_area
    end type

    abstract interface
        pure function to_string_i(self) result(string)
            import :: shape_t, varying_string

            implicit none

            class(shape_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function calculate_area_i(self) result(area)
            import :: shape_t

            implicit none

            class(shape_t), intent(in) :: self
            real :: area
        end function
    end interface
end module
