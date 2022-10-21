module ezmpi_bcast_m

  use mpi_f08, only: mpi_bcast, mpi_real, mpi_integer, mpi_comm_world, &
       mpi_logical
  
  implicit none
    
  interface ezmpi_bcast
     module procedure ezmpi_bcast_0d_real, ezmpi_bcast_1d_real, &
          ezmpi_bcast_0d_integer, ezmpi_bcast_1d_integer, ezmpi_bcast_0d_logical
  end interface ezmpi_bcast

  private
  public ezmpi_bcast

contains

  subroutine ezmpi_bcast_0d_real(buffer, root)

    real, intent(inout):: buffer
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, 1, mpi_real, root, mpi_comm_world)

  end subroutine ezmpi_bcast_0d_real

  !****************************************************

  subroutine ezmpi_bcast_1d_real(buffer, root)

    real, intent(inout):: buffer(:)
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, size(buffer), mpi_real, root, mpi_comm_world)

  end subroutine ezmpi_bcast_1d_real

  !****************************************************

  subroutine ezmpi_bcast_0d_integer(buffer, root)

    integer, intent(inout):: buffer
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, 1, mpi_integer, root, mpi_comm_world)

  end subroutine ezmpi_bcast_0d_integer

  !****************************************************

  subroutine ezmpi_bcast_1d_integer(buffer, root)

    integer, intent(inout):: buffer(:)
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, size(buffer), mpi_integer, root, mpi_comm_world)

  end subroutine ezmpi_bcast_1d_integer

  !****************************************************

  subroutine ezmpi_bcast_0d_logical(buffer, root)

    logical, intent(inout):: buffer
    integer, intent(in):: root

    !---------------------------------------------------------------------

    call mpi_bcast(buffer, 1, mpi_logical, root, mpi_comm_world)

  end subroutine ezmpi_bcast_0d_logical

end module ezmpi_bcast_m
