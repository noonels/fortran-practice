program main
    use circle_m, only: circle_t
    use filter_out_area_greater_than_m, only: filter_out_area_greater_than_t
    use filter_out_curves_m, only: filter_out_curves_t
    use iso_varying_string, only: put_line
    use shape_list_m, only: shape_list_t
    use shape_item_m, only: shape_item_t
    use square_m, only: square_t
    use strff, only: to_string
    use triangle_m, only: triangle_t

    implicit none

    type(shape_list_t) :: list
    type(shape_list_t) :: duplicated_list
    type(shape_list_t) :: filtered

    list = shape_list_t( &
        shapes = [ &
            shape_item_t( &
                shape = square_t( &
                    width = 2.0 &
                ) &
            ), &
            shape_item_t( &
                shape = circle_t( &
                    radius = 3.0 &
                ) &
            ), &
            shape_item_t( &
                shape = triangle_t( &
                    base = 4.0, &
                    height = 5.0 &
                ) &
            ) &
        ] &
    )

    call put_line(list%to_string())
    call put_line(to_string(list%total_area()))

    duplicated_list = (list*2.0) // (list*0.3)

    call put_line(duplicated_list%to_string())

    filtered = duplicated_list%filtered_by(filter_out_area_greater_than_t(2.0))

    call put_line(filtered%to_string())
end program
