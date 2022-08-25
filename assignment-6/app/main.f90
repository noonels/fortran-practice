program main
    use iso_varying_string, only: put_line
    use leaf_m, only: leaf_t
    use node_m, only: node_t
    use tree_item_m, only: tree_item_t
    use strff, only: to_string

    implicit none

    type(node_t) :: tree

    tree = node_t( &
            [ tree_item_t(node_t( &
                    [ tree_item_t(node_t( &
                            [ tree_item_t(leaf_t(1)) &
                            , tree_item_t(leaf_t(23)) &
                            , tree_item_t(leaf_t(4)) &
                            ])) &
                    , tree_item_t(leaf_t(567)) &
                    ])) &
            , tree_item_t(node_t( &
                    [ tree_item_t(node_t( &
                            [ tree_item_t(leaf_t(89)) &
                            ])) &
                    , tree_item_t(node_t( &
                            [ tree_item_t(leaf_t(1234)) &
                            , tree_item_t(leaf_t(56)) &
                            ])) &
                    ])) &
            ])

    call put_line(tree%to_string())
    call put_line(to_string(tree%total()))
end program
