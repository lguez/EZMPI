module ezmpi_recv_m

#ifdef HAVE_MPI
  use mpi_f08, only: mpi_recv, mpi_real, mpi_comm_world, MPI_STATUS_IGNORE, &
       mpi_logical, mpi_integer

  implicit none

  interface ezmpi_recv
     module procedure ezmpi_recv_0d_real, ezmpi_recv_1d_real, &
          ezmpi_recv_2d_real, ezmpi_recv_0d_logical, ezmpi_recv_0d_integer, &
          ezmpi_recv_2d_integer
  end interface ezmpi_recv

  private
  public ezmpi_recv

contains

  subroutine ezmpi_recv_0d_real(buf, source, tag)

    real, intent(out):: buf
    integer, intent(in):: source, tag

    !-------------------------------------------------------

    call mpi_recv(buf, 1, mpi_real, source, tag, mpi_comm_world, &
         MPI_STATUS_IGNORE)

  end subroutine ezmpi_recv_0d_real

  !****************************************************

  subroutine ezmpi_recv_1d_real(buf, source, tag)

    real, intent(out):: buf(:)
    integer, intent(in):: source, tag

    !-------------------------------------------------------

    call mpi_recv(buf(1), size(buf), mpi_real, source, tag, mpi_comm_world, &
         MPI_STATUS_IGNORE)    

  end subroutine ezmpi_recv_1d_real

  !****************************************************

  subroutine ezmpi_recv_2d_real(buf, source, tag)

    real, intent(out):: buf(:, :)
    integer, intent(in):: source, tag

    !-------------------------------------------------------

    call mpi_recv(buf(1, 1), size(buf), mpi_real, source, tag, mpi_comm_world, &
         MPI_STATUS_IGNORE)    

  end subroutine ezmpi_recv_2d_real

  !****************************************************

  subroutine ezmpi_recv_0d_logical(buf, source, tag)

    logical, intent(out):: buf
    integer, intent(in):: source, tag

    !-------------------------------------------------------

    call mpi_recv(buf, 1, mpi_logical, source, tag, mpi_comm_world, &
         MPI_STATUS_IGNORE)

  end subroutine ezmpi_recv_0d_logical

  !****************************************************

  subroutine ezmpi_recv_0d_integer(buf, source, tag)

    integer, intent(out):: buf
    integer, intent(in):: source, tag

    !-------------------------------------------------------

    call mpi_recv(buf, 1, mpi_integer, source, tag, mpi_comm_world, &
         MPI_STATUS_IGNORE)

  end subroutine ezmpi_recv_0d_integer

  !****************************************************

  subroutine ezmpi_recv_2d_integer(buf, source, tag)

    integer, intent(out):: buf(:, :)
    integer, intent(in):: source, tag

    !-------------------------------------------------------

    call mpi_recv(buf(1, 1), size(buf), mpi_integer, source, tag, &
         mpi_comm_world, MPI_STATUS_IGNORE)    

  end subroutine ezmpi_recv_2d_integer

#endif

end module ezmpi_recv_m
