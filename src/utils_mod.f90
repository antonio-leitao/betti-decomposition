module utils_mod
  implicit none
  private
  public :: quick_sort
  public :: parse_arguments

contains
  recursive subroutine quick_sort(arr)
    integer, intent(inout) :: arr(:)
    integer :: pivot, i, j, temp
    
    if (size(arr) > 1) then
      pivot = arr(size(arr)/2)
      i = 1
      j = size(arr)
      do
        do while (arr(i) < pivot)
          i = i + 1
        end do
        do while (arr(j) > pivot)
          j = j - 1
        end do
        if (i >= j) exit
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
        i = i + 1
        j = j - 1
      end do
      call quick_sort(arr(:j))
      call quick_sort(arr(j+1:))
    end if
  end subroutine quick_sort
  subroutine parse_arguments(filename, output, time_flag)
    character(len=*), intent(out) :: filename, output
    logical, intent(out) :: time_flag
    
    character(len=100) :: arg
    logical :: have_filename, have_output
    integer :: i, num_args
    
    ! Initialize variables
    filename = ""
    output = ""
    have_filename = .false.
    have_output = .false.
    time_flag = .false.
    
    ! Get number of command-line arguments
    num_args = command_argument_count()
    
    ! Parse command-line arguments
    i = 1
    do while (i <= num_args)
      call get_command_argument(i, arg)
      
      select case (arg)
        case ("--out", "-o")
          if (i + 1 <= num_args) then
            i = i + 1
            call get_command_argument(i, output)
            have_output = .true.
          else
            print *, "Error: Missing argument for --out/-o"
            call print_usage()
            stop
          end if
        
        case ("--time", "-t")
          time_flag = .true.
        
        case default
          if (.not. have_filename) then
            filename = arg
            have_filename = .true.
          else
            print *, "Error: Unexpected argument: ", trim(arg)
            call print_usage()
            stop
          end if
      end select
      
      i = i + 1
    end do
  end subroutine parse_arguments

  subroutine print_usage()
    print *, "Usage: name [filename] --out|-o [output] --time|-t"
    print *, "  filename : Optional input filename"
    print *, "  --out|-o : Optional output filename"
    print *, "  --time|-t: Optional time flag"
  end subroutine print_usage
end module utils_mod
