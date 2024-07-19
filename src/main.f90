program main
  use simplicial_complex_mod
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  type(simplicial_complex) :: sc
  character(len=100) :: filename
  integer :: num_args
  
  ! Check if a filename was provided as a command-line argument
  num_args = command_argument_count()
  
  if (num_args == 1) then
    call get_command_argument(1, filename)
  else if (num_args == 0) then
    ! No argument provided, prompt user for filename
    print *, "Enter the filename containing simplices:"
    read *, filename
  else
    ! Incorrect number of arguments
    write(error_unit,*) "Usage: ./betti [filename]"
    stop 1
  end if
  
  print *, "Reading from file:", trim(filename)
  sc = read_simplex_file(filename)
  call sc%betti_numbers()
  
  ! Print the simplicial complex
  ! print *, "Printing the simplicial complex:"
  ! call sc%print_complex()

contains

  function read_simplex_file(filename) result(sc)
      implicit none
      character(len=*), intent(in) :: filename
      character(len=2000) :: line !has to be big for big simplices
      type(simplicial_complex) :: sc
      integer :: i, n_vertices, max_dim
      integer, dimension(:), allocatable :: simplex_count, simplex
      integer :: ios
  
      open(unit=10, file=filename, status='old', action='read')
  
      !!!! FIRST PASS GET THE NUMBER OF DIMENSIONS !!!
      max_dim = 0
      do
          read(10, '(A)', iostat=ios) line
          if (ios /= 0) exit  ! Exit loop at the end of the file
          ! Count the number of integers in the line
          n_vertices = count_integers_in_line(line)
          if (n_vertices > max_dim) then
            max_dim = n_vertices
          end if
      end do
      print *, "MAX DIM: ", max_dim 
  
      !!!! SECOND PASS GET THE N_SIMPLEXES PER DIMENSION !!!
  
      rewind(10)
      allocate(simplex_count(max_dim))
      simplex_count=0
      do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit  ! Exit loop at the end of the file
        ! Count the number of integers in the line
        n_vertices = count_integers_in_line(line)
  !increase the count of simplex_count
        simplex_count(n_vertices)=simplex_count(n_vertices) + 1
      end do
      do i=1, size(simplex_count)
        print *, i, ": ", simplex_count(i)
      end do
      

      !!!! THIRD PASS ALLOCATE THEM !!!
      rewind(10)
      call sc%initialize(max_dim,simplex_count) ! Initialize the simplicial complex
      deallocate(simplex_count)
      
      do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit  ! Exit loop at the end of the file
        ! Count the number of integers in the line
        n_vertices = count_integers_in_line(line)
        !Allocate the array to store integers
        allocate(simplex(n_vertices))
        ! Read integers from the line
        read(line, *) simplex
        ! Print the integers
        ! print *, 'Adding simplex: ', simplex
        call sc%add_simplex(simplex)
        ! Deallocate the array
        deallocate(simplex)
      end do
      close(10)
  
  end function read_simplex_file

  
  integer function count_integers_in_line(line)
      implicit none
      character(len=*), intent(in) :: line
      integer :: i
      count_integers_in_line = 0
      do i = 1, len_trim(line)
          if (line(i:i) /= ' ') then
              if (i == 1 .or. line(i-1:i-1) == ' ') count_integers_in_line = count_integers_in_line + 1
          end if
      end do
  end function count_integers_in_line

end program main
