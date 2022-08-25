module shape_filterer_m
    use shape_m, only: shape_t

    implicit none
    private
    public :: shape_filterer_t

    type, abstract :: shape_filterer_t
    contains
        private
        procedure(matches_i), deferred, public :: matches
    end type

    abstract interface
        pure function matches_i(self, shape) result(matches)
            import :: shape_t, shape_filterer_t

            implicit none

            class(shape_filterer_t), intent(in) :: self
            class(shape_t), intent(in) :: shape
            logical :: matches
        end function
    end interface
end module
