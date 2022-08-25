module filter_out_area_greater_than_m
    use shape_m, only: shape_t
    use shape_filterer_m, only: shape_filterer_t

    implicit none
    private
    public :: filter_out_area_greater_than_t

    type, extends(shape_filterer_t) :: filter_out_area_greater_than_t
        private
        real :: area_limit
    contains
        private
        procedure, public :: matches
    end type

    interface filter_out_area_greater_than_t
        module procedure constructor
    end interface
contains
    pure function constructor(area_limit) result(filter)
        real, intent(in) :: area_limit
        type(filter_out_area_greater_than_t) :: filter

        filter%area_limit = area_limit
    end function

    pure function matches(self, shape)
        class(filter_out_area_greater_than_t), intent(in) :: self
        class(shape_t), intent(in) :: shape
        logical :: matches

        matches = shape%area() < self%area_limit
    end function
end module
