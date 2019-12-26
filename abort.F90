module abort_m

  implicit none

contains

  subroutine abort

#ifdef HAVE_MPI
    use mpi_f08, only: mpi_comm_world, mpi_abort

    !--------------------------------------------------------------

    CALL mpi_abort(mpi_comm_world, 1)
#else
    stop 1
#endif       

  end subroutine abort
  
end module abort_m
