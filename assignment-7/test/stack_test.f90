module stack_test
    use erloff, only: error_list_t
    use fallible_integer_m, only: fallible_integer_t
    use stack_m, only: fallible_stack_t, stack_t
    use stack_input_m, only: stack_input_t
    use vegetables, only: &
            input_t, &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            fail, &
            given, &
            it_

    implicit none
    private
    public :: test_new_stack, test_non_empty_stack

    integer, parameter :: STACK_ITEMS(*) = [2, 3, 5, 7, 11]
contains
    function test_new_stack() result(tests)
        type(test_item_t) :: tests

        tests = &
            given("a new stack", stack_input_t(stack_t()), &
                [ it_("it is empty", check_empty) &
                , it_("it returns an error when queried for its top item", check_empty_top) &
                , it_("it returns an error when popped", check_empty_pop) &
                , it_("it acquires depth by retaining a pushed item as its top", check_empty_push) &
                ])
    end function

    function test_non_empty_stack() result(tests)
        type(test_item_t) :: tests

        integer :: i
        type(stack_t) :: stack

        do i = 1, size(STACK_ITEMS)
            stack = stack%push(stack_items(i))
        end do

        tests = &
            given("a non-empty stack", stack_input_t(stack), &
                [ it_("it becomes deeper by retaining a pushed item as its top", check_non_empty_push) &
                , it_("on popping reveals tops in reverse order of pushing", check_popping) &
                ])
    end function

    function check_empty(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            result_ = assert_that(stack%empty())
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function

    function check_empty_top(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_integer_t) :: fallible_integer
        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            fallible_integer = stack%top()
            errors = fallible_integer%errors()
            result_ = assert_that(fallible_integer%failed(), errors%to_string())
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function

    function check_empty_pop(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_stack_t) :: fallible_stack
        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            fallible_stack = stack%pop()
            errors = fallible_stack%errors()
            result_ = assert_that(fallible_stack%failed(), errors%to_string())
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function

    function check_empty_push(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, parameter :: PUSHED_ITEM = 42
        type(error_list_t) :: errors
        type(fallible_integer_t) :: fallible_integer
        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            stack = stack%push(PUSHED_ITEM)
            fallible_integer = stack%top()
            errors = fallible_integer%errors()
            result_ = &
                    assert_that(.not.fallible_integer%failed(), "stack%top() didn't fail", errors%to_string()) &
                    .and. assert_equals(1, stack%depth(), "stack%depth()") &
                    .and. assert_equals(PUSHED_ITEM, fallible_integer%value_(), "stack%top()%value()")
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function

    function check_non_empty_push(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        integer, parameter :: PUSHED_ITEM = 42
        type(error_list_t) :: errors
        type(fallible_integer_t) :: fallible_integer
        integer :: initial_depth
        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            initial_depth = stack%depth()
            stack = stack%push(PUSHED_ITEM)
            fallible_integer = stack%top()
            errors = fallible_integer%errors()
            result_ = &
                    assert_that(.not.fallible_integer%failed(), "stack%top() didn't fail", errors%to_string()) &
                    .and. assert_equals(initial_depth+1, stack%depth(), "stack%depth()") &
                    .and. assert_equals(PUSHED_ITEM, fallible_integer%value_(), "stack%top()%value()")
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function

    function check_popping(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_integer_t) :: fallible_integer
        type(fallible_stack_t) :: fallible_stack
        integer :: i
        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            do i = size(STACK_ITEMS), 1, -1
                fallible_integer = stack%top()
                errors = fallible_integer%errors()
                result_ = &
                        result_ &
                        .and. assert_that(.not.fallible_integer%failed(), "stack%top() didn't fail", errors%to_string()) &
                        .and. assert_equals(STACK_ITEMS(i), fallible_integer%value_(), "stack%top()%value()")
                fallible_stack = stack%pop()
                errors = fallible_stack%errors()
                result_ = result_ .and. assert_that(.not.fallible_stack%failed(), "stack%pop() didn't fail", errors%to_string())
                if (result_%passed()) then
                    stack = fallible_stack%stack()
                else
                    return
                end if
            end do
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function
end module
