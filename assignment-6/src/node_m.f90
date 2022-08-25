module node_m
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), len, trim, var_str
    use strff, only: join, split_at, NEWLINE
    use tree_m, only: tree_t
    use tree_item_m, only: tree_item_t

    implicit none
    private
    public :: node_t

    type, extends(tree_t) :: node_t
        private
        type(tree_item_t), allocatable :: children(:)
    contains
        private
        procedure, public :: to_string
        procedure, public :: total
    end type

    interface node_t
        module procedure constructor
    end interface
contains
    function constructor(children) result(node)
        type(tree_item_t), intent(in) :: children(:)
        type(node_t) :: node

        allocate(node%children, source = children)
    end function

    pure recursive function to_string(self) result(string)
        class(node_t), intent(in) :: self
        type(varying_string) :: string

        type(varying_string) :: blocked
        type(varying_string), allocatable :: child_strings(:)
        type(varying_string) :: dashed_line
        integer :: height_
        integer :: i
        type(varying_string), allocatable :: padded_strings(:)
        type(varying_string) :: pipes
        integer, allocatable :: widths(:)

        allocate(child_strings, source = [(self%children(i)%to_string(), i = 1, size(self%children))])
        allocate(widths, source = max_width(child_strings)+1)
        height_ = maxval(height(child_strings))
        dashed_line = make_dashes(widths)
        pipes = join(make_pipe(widths), "")
        allocate(padded_strings, source = pad_to(child_strings, widths, height_))
        blocked = join( &
                [ dashed_line&
                , pipes &
                , concat_lines(padded_strings) &
                ], &
                NEWLINE)
        string = strip_trailing_space(blocked)
    end function

    elemental recursive function total(self)
        class(node_t), intent(in) :: self
        integer :: total

        total = sum(self%children%total())
    end function

    elemental function max_width(tree_string) result(width)
        type(varying_string), intent(in) :: tree_string
        integer :: width

        width = maxval(len(split_at(tree_string, NEWLINE)))
    end function

    elemental function height(tree_string)
        type(varying_string), intent(in) :: tree_string
        integer :: height

        height = size(split_at(tree_string, NEWLINE))
    end function

    elemental function pad_to(string, width, num_lines) result(padded)
        type(varying_string), intent(in) :: string
        integer, intent(in) :: width, num_lines
        type(varying_string) :: padded

        integer :: i

        associate(lines => split_at(string, NEWLINE))
            associate(padded_lines => pad_line(lines, width))
                padded = join([padded_lines, [(var_str(repeat(" ", width)), i = 1, num_lines - size(lines))]], NEWLINE)
            end associate
        end associate
    contains
        elemental function pad_line(line, width) result(padded_line)
            type(varying_string), intent(in) :: line
            integer, intent(in) :: width
            type(varying_string) :: padded_line

            padded_line = line // repeat(" ", width - len(line))
        end function
    end function

    pure function make_dashes(widths) result(dashes)
        integer, intent(in) :: widths(:)
        type(varying_string) :: dashes

        if (size(widths) == 1) then
            dashes = make_pipe(widths(1))
        else
            associate( &
                    leading_spaces => widths(1)/2 - 1, &
                    trailing_spaces => widths(size(widths)) - widths(size(widths))/2, &
                    total_width => sum(widths))
                associate(dash_width => total_width - leading_spaces - trailing_spaces)
                    dashes = repeat(" ", leading_spaces) // repeat("-", dash_width) // repeat(" ", trailing_spaces)
                end associate
            end associate
        end if
    end function

    elemental function make_pipe(width) result(pipe)
        integer, intent(in) :: width
        type(varying_string) :: pipe

        pipe = center_in(var_str("|"), width)
    end function

    pure function center_in(string, width) result(centered)
        type(varying_string), intent(in) :: string
        integer, intent(in) :: width
        type(varying_string) :: centered

        associate(leading_spaces => width/2 - len(string)/2 - 1)
            associate(trailing_spaces => width - leading_spaces - len(string))
                centered = repeat(" ", leading_spaces) // string // repeat(" ", trailing_spaces)
            end associate
        end associate
    end function

    pure function concat_lines(strings) result(joined)
        type(varying_string), intent(in) :: strings(:)
        type(varying_string) :: joined

        integer :: i, j

        associate(lines => [(split_at(strings(i), NEWLINE), i = 1, size(strings))])
            associate(num_lines => size(lines) / size(strings))
                joined = join( &
                        [(join( &
                                [(lines(i), i = j, num_lines*size(strings), num_lines)] &
                                , ""), j = 1, num_lines)], &
                        NEWLINE)
            end associate
        end associate
    end function

    pure function strip_trailing_space(string) result(stripped)
        type(varying_string), intent(in) :: string
        type(varying_string) :: stripped

        stripped = join(trim(split_at(string, NEWLINE)), NEWLINE)
    end function
end module
