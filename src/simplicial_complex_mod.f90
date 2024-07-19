module simplicial_complex_mod
  use sorted_list_mod
  use sparse_matrix_mod, only: sparse_row, gaussian_elimination_f2
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
    procedure :: boundary_matrix_rank
    procedure :: betti_numbers
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

    subroutine boundary_matrix_rank(this, k, rank)
      class(simplicial_complex), intent(in) :: this
      integer, intent(in) :: k
      integer, intent(out) :: rank
      integer :: m, n, i, j, l, face_index
      type(sparse_row), allocatable :: matrix(:)
      integer, allocatable :: temp_list(:)
      logical :: found
    ! Check for valid input
      if (k < 0 .or. k > this%max_dim) then
        print *, "Error: Invalid dimension k =", k
        rank = -1  ! Indicate error
        return
      end if

      ! Convention for 0-dimensional boundary map
      if (k == 1) then
        rank = 0
        return
      end if

      m = this%dimensions(k)%num_elements
      n = this%dimensions(k-1)%num_elements
      
      allocate(matrix(m))
      allocate(temp_list(k-1))

      do i = 1, m
        allocate(matrix(i)%indices(k))
        !! BOUNDARY OPERATOR LOOP
        do j = 1, k
          l = 0
          ! Create a temporary list excluding j-th element
          do n = 1, k
            if (n /= j) then
              l = l + 1
              temp_list(l) = this%dimensions(k)%data(n,i)
            end if
          end do
          ! Binary search for the face index
          face_index = this%dimensions(k-1)%binary_search(temp_list, found)
          if (.not. found) then
            print *, "Error: Face not found for simplex", this%dimensions(k)%data(:,i)
            rank = -1  ! Indicate error
            return
          end if
          ! Store the face index
          matrix(i)%indices(k-j+1) = face_index
        end do
      ! print *, "boundary: ", matrix(i)%indices
      end do
      ! Cleanup
      deallocate(temp_list)
      call gaussian_elimination_f2(matrix, m, n, rank)
      deallocate(matrix)
      print *, "Dimension: ",k, "Shape: ",m,n,"Rank: ",rank 
    end subroutine boundary_matrix_rank

subroutine betti_numbers(this)
    class(simplicial_complex), intent(inout) :: this
    integer :: i, K, rank_k, rank_k1
    integer, allocatable :: betti(:), ranks(:)
    
    allocate(betti(this%max_dim))
    allocate(ranks(this%max_dim))
    
    ! Calculate ranks in descending order
    ranks(1) = 0  ! By convention, rank of boundary map for 0-simplices is 0
    do i = this%max_dim, 1, -1
        call this%boundary_matrix_rank(i, ranks(i))
    end do
    
    ! Calculate Betti numbers
    do i = 1, this%max_dim
        K = this%dimensions(i)%num_elements
        betti(i) = K - (ranks(i) + ranks(i+1))
    end do

    ! Print Betti numbers
    do i = 1, this%max_dim
        print *, "b", i-1, " = ", betti(i)
    end do

    deallocate(betti)
    deallocate(ranks)
end subroutine betti_numbers
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
