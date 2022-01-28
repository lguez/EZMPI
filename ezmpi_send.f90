module ezmpi_send_m

  use mpi_f08, only: mpi_send, mpi_real, mpi_logical, mpi_comm_world, &
       mpi_integer

  implicit none

  interface ezmpi_send
     module procedure ezmpi_send_0d_real, ezmpi_send_1d_real, &
          ezmpi_send_2d_real, ezmpi_send_0d_logical, ezmpi_send_0d_integer, &
          ezmpi_send_2d_integer
  end interface ezmpi_send

  private
  public ezmpi_send

contains

  subroutine ezmpi_send_0d_real(buf, dest, tag)

    real, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

    call mpi_send(buf, 1, mpi_real, dest, tag, mpi_comm_world)

  end subroutine ezmpi_send_0d_real

  !****************************************************

  subroutine ezmpi_send_1d_real(buf, dest, tag)

    real, intent(in):: buf(:)
    integer, intent(in):: dest, tag

    !-------------------

    if (size(buf) /= 0) call mpi_send(buf(1), size(buf), mpi_real, dest, tag, &
         mpi_comm_world)

  end subroutine ezmpi_send_1d_real

  !****************************************************

  subroutine ezmpi_send_2d_real(buf, dest, tag)

    real, intent(in):: buf(:, :)
    integer, intent(in):: dest, tag

    !-------------------

    if (size(buf) /= 0) call mpi_send(buf(1, 1), size(buf), mpi_real, dest, &
         tag, mpi_comm_world)

  end subroutine ezmpi_send_2d_real

  !****************************************************

  subroutine ezmpi_send_0d_logical(buf, dest, tag)

    logical, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

    call mpi_send(buf, 1, mpi_logical, dest, tag, mpi_comm_world)

  end subroutine ezmpi_send_0d_logical

  !****************************************************

  subroutine ezmpi_send_0d_integer(buf, dest, tag)

    integer, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

    call mpi_send(buf, 1, mpi_integer, dest, tag, mpi_comm_world)

  end subroutine ezmpi_send_0d_integer

  !****************************************************

  subroutine ezmpi_send_2d_integer(buf, dest, tag)

    integer, intent(in):: buf(:, :)
    integer, intent(in):: dest, tag

    !-------------------

    if (size(buf) /= 0) call mpi_send(buf(1, 1), size(buf), mpi_integer, &
         dest, tag, mpi_comm_world)

  end subroutine ezmpi_send_2d_integer

end module ezmpi_send_m
