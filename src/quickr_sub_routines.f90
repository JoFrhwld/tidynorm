subroutine dct_q(y, kk, jj, forward, coefs, y__len_) bind(c)
  use iso_c_binding, only: c_double, c_int, c_ptrdiff_t
  implicit none

  ! manifest start
  ! sizes
  integer(c_ptrdiff_t), intent(in), value :: y__len_

  ! args
  real(c_double), intent(in) :: y(y__len_)
  integer(c_int), intent(in) :: kk
  integer(c_int), intent(in) :: jj
  integer(c_int), intent(in) :: forward ! logical
  real(c_double), intent(out) :: coefs(kk)

  ! locals
  integer(c_int), allocatable :: j_vec(:)
  real(c_double) :: normer
  real(c_double) :: bank(kk, jj)
  integer(c_int) :: tmp1_
  real(c_double) :: pi
  integer(c_int) :: i
  integer(c_int) :: j
  integer(c_int) :: k
  real(c_double) :: N
  integer(c_int) :: tmp2_
  integer(c_int), allocatable :: k_vec(:)
  ! manifest end


  pi = (4.0_c_double * ((4.0_c_double * atan(0.2_c_double)) - atan(0.0041841004184100415_c_double)))
  k_vec = [ (tmp1_, tmp1_ = 0_c_int, (kk - 1_c_int), sign(1, (kk - 1_c_int)-0_c_int)) ]
  j_vec = [ (tmp2_, tmp2_ = 0_c_int, (jj - 1_c_int), sign(1, (jj - 1_c_int)-0_c_int)) ]
  N = 0.0_c_double
  bank = 0.0_c_double
  do j = 1, size(j_vec)
    N = (N + 1.0_c_double)
    do k = 1, size(k_vec)
      bank(k, j) = cos((((pi * (k - 1.0_c_double)) * ((2.0_c_double * (j - 1.0_c_double)) + 1.0_c_double)) / (2.0_c_double * jj)))
    end do
  end do
  coefs = 0
  do i = 1, size(coefs)
    coefs(i) = sum((bank(i, :) * y))
  end do
  normer = merge((1.0_c_double / N), 2.0_c_double, (forward/=0))
  coefs = (coefs * normer)
  coefs(1_c_int) = (coefs(1_c_int) / sqrt(2.0_c_double))
end subroutine
