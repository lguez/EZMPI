module ezmpi_send_m

#ifdef HAVE_MPI
  use mpi_f08, only: mpi_ssend, mpi_real, mpi_logical, mpi_comm_world, &
       mpi_integer
#endif

  implicit none

  interface ezmpi_send
     module procedure ezmpi_send_0d_real, ezmpi_send_1d_real, &
          ezmpi_send_2d_real, ezmpi_send_0d_logical, ezmpi_send_0d_integer
  end interface ezmpi_send

  private
  public ezmpi_send

contains

  subroutine ezmpi_send_0d_real(buf, dest, tag)

    real, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

#ifdef HAVE_MPI
    call mpi_ssend(buf, 1, mpi_real, dest, tag, mpi_comm_world)
#endif

  end subroutine ezmpi_send_0d_real

  !****************************************************

  subroutine ezmpi_send_1d_real(buf, dest, tag)

    real, intent(in):: buf(:)
    integer, intent(in):: dest, tag

    !-------------------

#ifdef HAVE_MPI
    call mpi_ssend(buf(1), size(buf), mpi_real, dest, tag, mpi_comm_world)
#endif

  end subroutine ezmpi_send_1d_real

  !****************************************************

  subroutine ezmpi_send_2d_real(buf, dest, tag)

    real, intent(in):: buf(:, :)
    integer, intent(in):: dest, tag

    !-------------------

#ifdef HAVE_MPI
    call mpi_ssend(buf(1, 1), size(buf), mpi_real, dest, tag, mpi_comm_world)
#endif

  end subroutine ezmpi_send_2d_real

  !****************************************************

  subroutine ezmpi_send_0d_logical(buf, dest, tag)

    logical, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

#ifdef HAVE_MPI
    call mpi_ssend(buf, 1, mpi_logical, dest, tag, mpi_comm_world)
#endif

  end subroutine ezmpi_send_0d_logical

  !****************************************************

  subroutine ezmpi_send_0d_integer(buf, dest, tag)

    integer, intent(in):: buf
    integer, intent(in):: dest, tag

    !-------------------

#ifdef HAVE_MPI
    call mpi_ssend(buf, 1, mpi_integer, dest, tag, mpi_comm_world)
#endif

  end subroutine ezmpi_send_0d_integer

end module ezmpi_send_m
