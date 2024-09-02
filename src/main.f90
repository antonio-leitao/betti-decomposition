program main
   use simplicial_complex_mod
   use utils_mod, only: parse_arguments
   implicit none
   type(simplicial_complex) :: sc
   character(len=100) :: filename, output
   integer :: num_args
   integer, allocatable :: betti(:)
   real(kind=8) :: start_time, end_time, elapsed_time
   logical :: time_flag

   call parse_arguments(filename, output, time_flag)

   ! Check if a filename was provided as a command-line argument
   num_args = command_argument_count()

   !if user didnt provide filename, ask for it
   if (len_trim(filename) < 1) then
      print *, "Enter the filename containing simplices:"
      read *, filename
   end if

   ! CORE COMPUTATIONS
   sc = read_simplex_file(filename)
   allocate (betti(sc%max_dim))

   call cpu_time(start_time)
   call sc%betti_numbers(betti)
   call cpu_time(end_time)
   elapsed_time = end_time - start_time
   if (time_flag) then
      !print to console if user didnt provide
      if (len_trim(output) > 0) then
         print *, "  Printing time to file: ", trim(output)
      else
         print *, human_readable_time(elapsed_time)
      end if
   else
      !print to console if user didnt provide
      if (len_trim(output) > 0) then
         call print_betti_numbers(betti, trim(output))
      else
         call print_betti_numbers(betti)
      end if
   end if

contains

   function read_simplex_file(filename) result(sc)
      implicit none
      character(len=*), intent(in) :: filename
      character(len=2000) :: line !has to be big for big simplices
      type(simplicial_complex) :: sc
      integer :: n_vertices, max_dim
      integer, dimension(:), allocatable :: simplex_count, simplex
      integer :: ios

      open (unit=10, file=filename, status='old', action='read')

      !!!! FIRST PASS GET THE NUMBER OF DIMENSIONS !!!
      max_dim = 0
      do
         read (10, '(A)', iostat=ios) line
         if (ios /= 0) exit  ! Exit loop at the end of the file
         ! Count the number of integers in the line
         n_vertices = count_integers_in_line(line)
         if (n_vertices > max_dim) then
            max_dim = n_vertices
         end if
      end do

      !!!! SECOND PASS GET THE N_SIMPLEXES PER DIMENSION !!!

      rewind (10)
      allocate (simplex_count(max_dim))
      simplex_count = 0
      do
         read (10, '(A)', iostat=ios) line
         if (ios /= 0) exit  ! Exit loop at the end of the file
         ! Count the number of integers in the line
         n_vertices = count_integers_in_line(line)
         !increase the count of simplex_count
         simplex_count(n_vertices) = simplex_count(n_vertices) + 1
      end do

      !!!! THIRD PASS ALLOCATE THEM !!!
      rewind (10)
      call sc%initialize(max_dim, simplex_count) ! Initialize the simplicial complex
      deallocate (simplex_count)

      do
         read (10, '(A)', iostat=ios) line
         if (ios /= 0) exit  ! Exit loop at the end of the file
         ! Count the number of integers in the line
         n_vertices = count_integers_in_line(line)
         !Allocate the array to store integers
         allocate (simplex(n_vertices))
         ! Read integers from the line
         read (line, *) simplex
         ! Print the integers
         ! print *, 'Adding simplex: ', simplex
         call sc%add_simplex(simplex)
         ! Deallocate the array
         deallocate (simplex)
      end do
      close (10)

   end function read_simplex_file

   integer function count_integers_in_line(line)
      implicit none
      character(len=*), intent(in) :: line
      integer :: i
      count_integers_in_line = 0
      do i = 1, len_trim(line)
         if (line(i:i) /= ' ') then
            if (i == 1 .or. line(i - 1:i - 1) == ' ') count_integers_in_line = count_integers_in_line + 1
         end if
      end do
   end function count_integers_in_line

   function human_readable_time(seconds) result(time_string)
      real(kind=8), intent(in) :: seconds  ! Using double precision for higher accuracy
      character(len=50) :: time_string

      ! Write the seconds with high precision (at least 8 significant figures)
      write (time_string, '(F0.8)') seconds

      ! Remove any leading spaces that might result from the formatting
      time_string = adjustl(time_string)
   end function human_readable_time

   subroutine print_betti_numbers(betti, filename)
      integer, intent(in) :: betti(:)  ! Betti numbers array passed as an argument
      character(len=*), optional, intent(in) :: filename
      integer :: i, max_dim, max_width, unit
      character(len=50) :: fmt_string

      max_dim = size(betti)

      ! only God knows how this works now, because I've forgotten
      max_width = floor(log10(real(max_dim - 1))) + 1
      write (fmt_string, '(A,I0,A)') '(A,I0,T', max_width + 3, ',A,1X,I0)'

      if (present(filename)) then
         open (newunit=unit, file=filename, status='replace') !<-maybe add some check for err
         do i = 1, max_dim
            write (unit, fmt_string) 'b', i - 1, '=', betti(i)
         end do
         close (unit)
      else
         ! Print Betti numbers to the console
         do i = 1, max_dim
            write (*, fmt_string) 'b', i - 1, '=', betti(i)
         end do
      end if
   end subroutine print_betti_numbers

end program main
