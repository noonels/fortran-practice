module filter_out_curves_m
    use shape_m, only: shape_t
    use shape_filterer_m, only: shape_filterer_t

    implicit none
    private
    public :: filter_out_curves_t

    type, extends(shape_filterer_t) :: filter_out_curves_t
    contains
        private
        procedure, public :: matches
    end type
contains
    pure function matches(self, shape)
        class(filter_out_curves_t), intent(in) :: self
        class(shape_t), intent(in) :: shape
        logical :: matches

        matches = .not. shape%has_curves()
    end function
end module
