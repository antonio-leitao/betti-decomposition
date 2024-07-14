module utils_mod
  implicit none
  private
  public :: quick_sort

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
end module utils_mod
