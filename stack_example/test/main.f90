! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use stack_test, only: &
                stack_new_stack => test_new_stack, &
                stack_non_empty_stack => test_non_empty_stack
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = stack_new_stack()
        individual_tests(2) = stack_non_empty_stack()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
