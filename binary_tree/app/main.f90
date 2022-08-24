program main
    use iso_varying_string, only: put_line
    use leaf_m, only: leaf_t
    use node_m, only: node_t

    implicit none

    type(node_t) :: tree

    tree = node_t( &
            node_t( &
                    node_t( &
                            node_t( &
                                    leaf_t(1), &
                                    leaf_t(23)), &
                            leaf_t(4)), &
                    leaf_t(567)), &
            node_t( &
                    leaf_t(89), &
                    node_t( &
                            leaf_t(1234), &
                            leaf_t(56))))

    call put_line(tree%to_string())
end program
