program main
    use circle_m, only: circle_t
    use iso_varying_string, only: put_line
    use shape_list_m, only: shape_list_t
    use shape_item_m, only: shape_item_t
    use square_m, only: square_t
    use triangle_m, only: triangle_t

    implicit none

    type(shape_list_t) :: list

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
end program
