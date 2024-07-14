module sorted_list_mod
  implicit none
  private

  type, public :: sorted_list
    private
    integer :: num_elements
    integer :: list_size
    integer :: element_size
    integer, allocatable :: data(:,:)
  contains
    procedure :: binary_search
    procedure :: try_insert
    procedure :: initialize
    procedure :: print_list
  end type sorted_list

contains

  subroutine initialize(this, element_size, initial_capacity)
    class(sorted_list), intent(inout) :: this
    integer, intent(in) :: element_size
    integer, intent(in) :: initial_capacity

    this%num_elements = 0
    this%list_size = initial_capacity
    this%element_size = element_size
    allocate(this%data(element_size, initial_capacity))
  end subroutine initialize

  function binary_search(this, element, found) result(index)
    class(sorted_list), intent(in) :: this
    integer, intent(in) :: element(this%element_size)
    logical, intent(out) :: found
    integer :: index
    integer :: low, high, mid

    low = 1
    high = this%num_elements
    found = .false.

    do while (low <= high)
      mid = (low + high) / 2
      if (all(this%data(:, mid) == element)) then
        found = .true.
        index = mid
        return
      else if (any(this%data(:, mid) < element)) then
        low = mid + 1
      else
        high = mid - 1
      end if
    end do

    index = low
  end function binary_search

  function try_insert(this, element) result(status)
    class(sorted_list), intent(inout) :: this
    integer, intent(in) :: element(this%element_size)
    integer :: status
    logical :: found
    integer :: index, i

    index = this%binary_search(element, found)

    if (found) then
      status = 2  ! Element already exists
      return
    end if

    if (this%num_elements == this%list_size) then
      status = 1  ! Error: List is full
      return
    end if

    ! Shift elements to make space for the new element
    do i = this%num_elements, index, -1
      this%data(:, i+1) = this%data(:, i)
    end do

    ! Insert the new element
    this%data(:, index) = element
    this%num_elements = this%num_elements + 1
    status = 0  ! Successfully inserted
  end function try_insert

  ! REDO THIS
  subroutine print_list(this)
    class(sorted_list), intent(in) :: this
    integer :: i, j

    print *, "List contents:"
    do i = 1, this%num_elements
      write(*, '(A,I0,A)', advance='no') "Element ", i, ": ("
      do j = 1, this%element_size
        write(*, '(I0)', advance='no') this%data(j, i)
        if (j < this%element_size) write(*, '(A)', advance='no') ", "
      end do
      print *, ")"
    end do
  end subroutine print_list

end module sorted_list_mod
