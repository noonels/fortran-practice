program main
    use circle_m, only: circle_t
    use rectangle_m, only: rectangle_t
    use iso_varying_string, only: put_line
    use shape_list_m, only: shape_list_t
    use shape_item_m, only: shape_item_t
    use strff, only: to_string
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
            ), &
            shape_item_t( &
                shape = rectangle_t( &
                    height = 2.0, &
                    width = 3.0 &
                ) &
            ) &
        ] &
    )

    call put_line(list%to_string())
    call put_line(to_string(list%total_area()))

end program
