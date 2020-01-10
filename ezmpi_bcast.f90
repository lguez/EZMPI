module ezmpi_bcast_m

  implicit none

  interface ezmpi_bcast
     module procedure ezmpi_bcast_1d_real, ezmpi_bcast_0d_integer
  end interface ezmpi_bcast

  private
  public ezmpi_bcast

contains

  subroutine ezmpi_bcast_1d_real(buffer, root)

    use mpi_f08, only: mpi_bcast, mpi_real, mpi_comm_world

    real, intent(in):: buffer(:)
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, size(buffer), mpi_real, root, mpi_comm_world)

  end subroutine ezmpi_bcast_1d_real

  !****************************************************

  subroutine ezmpi_bcast_0d_integer(buffer, root)

    use mpi_f08, only: mpi_bcast, mpi_integer, mpi_comm_world

    integer, intent(in):: buffer
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, 1, mpi_integer, root, mpi_comm_world)

  end subroutine ezmpi_bcast_0d_integer

end module ezmpi_bcast_m
