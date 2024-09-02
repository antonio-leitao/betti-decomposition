module utils_mod
   implicit none
   private
   public :: bubble_sort
   public :: parse_arguments

contains
   subroutine bubble_sort(arr)
      implicit none
      integer, intent(inout) :: arr(:)
      integer :: i, j, temp, n
      n = size(arr)
      ! Bubble sort
      do i = 1, n - 1
         do j = 1, n - i
            if (arr(j) > arr(j + 1)) then
               temp = arr(j)
               arr(j) = arr(j + 1)
               arr(j + 1) = temp
            end if
         end do
      end do
   end subroutine bubble_sort

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
      print *, "Usage: betti [filename] --out|-o [filename] --time|-t"
      print *, "  filename : Simplicial complex filename, prompts if not specified."
      print *, "  --out|-o : Output filename, prints to console if not specified."
      print *, "  --time|-t: Output execution time isntead of betti numbers."
   end subroutine print_usage
end module utils_mod
