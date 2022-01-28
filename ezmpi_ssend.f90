module ezmpi_ssend_m

  use mpi_f08, only: mpi_ssend, mpi_real, mpi_logical, mpi_comm_world, &
       mpi_integer

  implicit none

  interface ezmpi_ssend
     module procedure ezmpi_ssend_0d_real, ezmpi_ssend_1d_real, &
          ezmpi_ssend_2d_real, ezmpi_ssend_0d_logical, ezmpi_ssend_0d_integer, &
          ezmpi_ssend_2d_integer
  end interface ezmpi_ssend

  private
  public ezmpi_ssend

contains

  subroutine ezmpi_ssend_0d_real(buf, dest, tag)

    real, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

    call mpi_ssend(buf, 1, mpi_real, dest, tag, mpi_comm_world)

  end subroutine ezmpi_ssend_0d_real

  !****************************************************

  subroutine ezmpi_ssend_1d_real(buf, dest, tag)

    real, intent(in):: buf(:)
    integer, intent(in):: dest, tag

    !-------------------

    if (size(buf) /= 0) call mpi_ssend(buf(1), size(buf), mpi_real, dest, tag, &
         mpi_comm_world)

  end subroutine ezmpi_ssend_1d_real

  !****************************************************

  subroutine ezmpi_ssend_2d_real(buf, dest, tag)

    real, intent(in):: buf(:, :)
    integer, intent(in):: dest, tag

    !-------------------

    if (size(buf) /= 0) call mpi_ssend(buf(1, 1), size(buf), mpi_real, dest, &
         tag, mpi_comm_world)

  end subroutine ezmpi_ssend_2d_real

  !****************************************************

  subroutine ezmpi_ssend_0d_logical(buf, dest, tag)

    logical, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

    call mpi_ssend(buf, 1, mpi_logical, dest, tag, mpi_comm_world)

  end subroutine ezmpi_ssend_0d_logical

  !****************************************************

  subroutine ezmpi_ssend_0d_integer(buf, dest, tag)

    integer, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

    call mpi_ssend(buf, 1, mpi_integer, dest, tag, mpi_comm_world)

  end subroutine ezmpi_ssend_0d_integer

  !****************************************************

  subroutine ezmpi_ssend_2d_integer(buf, dest, tag)

    integer, intent(in):: buf(:, :)
    integer, intent(in):: dest, tag

    !-------------------

    if (size(buf) /= 0) call mpi_ssend(buf(1, 1), size(buf), mpi_integer, &
         dest, tag, mpi_comm_world)

  end subroutine ezmpi_ssend_2d_integer

end module ezmpi_ssend_m
