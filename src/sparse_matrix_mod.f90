module sparse_matrix_mod
   implicit none
   private
   public :: sparse_row, gaussian_elimination_f2

   type sparse_row
      integer, allocatable :: indices(:)
   end type sparse_row

contains

   subroutine gaussian_elimination_f2(matrix, m, n, rank)
      type(sparse_row), intent(inout) :: matrix(:)
      integer, intent(in) :: m, n
      integer, intent(out) :: rank

      integer :: h, k, i, pivot_row
      logical :: found_pivot

      h = 1  ! Current pivot row
      k = 1  ! Current pivot column
      rank = 0

      do while (h <= m .and. k <= n)
         ! Find pivot
         found_pivot = .false.
         do i = h, m
            if (any(matrix(i)%indices == k)) then
               pivot_row = i
               found_pivot = .true.
               exit
            end if
         end do

         if (.not. found_pivot) then
            ! No pivot in this column, move to next
            k = k + 1
            cycle
         end if

         ! Swap pivot row with current row if necessary
         if (pivot_row /= h) then
            call swap_rows(matrix(h), matrix(pivot_row))
         end if

         ! XOR operations for rows below pivot_row
         ! tried parallelizeing this but to no avail, rust speedup is massive here
         do i = h + 1, m
            if (any(matrix(i)%indices == k)) then
               call xor_rows(matrix(i), matrix(h))
            end if
         end do

         rank = rank + 1
         h = h + 1
         k = k + 1
      end do
   end subroutine gaussian_elimination_f2

   subroutine swap_rows(row1, row2)
      type(sparse_row), intent(inout) :: row1, row2
      type(sparse_row) :: temp

      temp = row1
      row1 = row2
      row2 = temp
   end subroutine swap_rows

   subroutine xor_rows(row1, row2)
      type(sparse_row), intent(inout) :: row1
      type(sparse_row), intent(in) :: row2

      integer, allocatable :: result(:)
      integer :: i, j, k, n1, n2

      n1 = size(row1%indices)
      n2 = size(row2%indices)
      allocate (result(n1 + n2))

      i = 1
      j = 1
      k = 1

      do while (i <= n1 .and. j <= n2)
         if (row1%indices(i) < row2%indices(j)) then
            result(k) = row1%indices(i)
            i = i + 1
         else if (row1%indices(i) > row2%indices(j)) then
            result(k) = row2%indices(j)
            j = j + 1
         else  ! Equal indices, they cancel out in XOR
            i = i + 1
            j = j + 1
            cycle
         end if
         k = k + 1
      end do

      ! Add remaining elements
      do while (i <= n1)
         result(k) = row1%indices(i)
         i = i + 1
         k = k + 1
      end do

      do while (j <= n2)
         result(k) = row2%indices(j)
         j = j + 1
         k = k + 1
      end do

      ! Resize result to actual size
      deallocate (row1%indices)
      allocate (row1%indices(k - 1))
      row1%indices = result(:k - 1)
   end subroutine xor_rows

end module sparse_matrix_mod
