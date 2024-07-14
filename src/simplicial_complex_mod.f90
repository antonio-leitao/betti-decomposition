module simplicial_complex_mod
  use sorted_list_mod
  use utils_mod, only: quick_sort
  implicit none
  private

  type, public :: simplicial_complex
    private
    type(sorted_list), allocatable :: dimensions(:)
    integer :: max_dim
  contains
    procedure :: initialize
    procedure :: add_simplex
    procedure :: print_complex
  end type simplicial_complex

contains

  ! CHECKED
  subroutine initialize(this, max_dim, simplex_counts)
    class(simplicial_complex), intent(inout) :: this
    integer, intent(in) :: max_dim
    integer, intent(in) :: simplex_counts(:)
    integer :: i

    this%max_dim = max_dim
    allocate(this%dimensions(0:max_dim))
    
    do i = 1, max_dim
      call this%dimensions(i)%initialize(i, simplex_counts(i))  ! Initialize the sorted vector with capacity
    end do
  end subroutine initialize

  subroutine add_simplex(this, simplex)
    class(simplicial_complex), intent(inout) :: this
    integer, intent(in) :: simplex(:)
    integer :: dim, status
    integer, allocatable :: ordered_simplex(:)

    dim = size(simplex)
    !! TODO CONTROL FLOW!
    if (dim > this%max_dim) then
      print *, "Error: Simplex dimension exceeds maximum dimension of complex"
      return
    end if

    ! Order the simplex
    allocate(ordered_simplex, source=simplex)
    call quick_sort(ordered_simplex)

    ! Add to the appropriate dimension
    status = this%dimensions(dim)%try_insert(ordered_simplex)
    !! TODO CONTROL FLOW!
    select case (status)
        case (1)
            print *, "LIST FULL: Failed to add simplex: ", ordered_simplex, "to dimension: ", dim
        case (2)
            print *, "ALREADY PRESENT: Failed to add simplex: ", ordered_simplex, "to dimension: ", dim
    end select
  end subroutine add_simplex

  ! Helper functions

  subroutine print_complex(this)
    class(simplicial_complex), intent(in) :: this
    integer :: i

    do i = 0, this%max_dim
      print *, "Dimension", i, ":"
      call this%dimensions(i)%print_list()
      print *, ""
    end do
  end subroutine print_complex
end module simplicial_complex_mod
