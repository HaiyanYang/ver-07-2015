module abstPly_elem_module
!
!  Purpose:
!    define an 'abstract element' object to interface with the 
!    base brick and wedge elements
!    with the associated procedures to empty, set, integrate and extract
!    its components
!
!
!  Record of revision:
!    Date      Programmer            Description of change
!    ========  ====================  ========================================
!    17/04/15  B. Y. Chen            Original code
!
use parameter_module, only : NST => NST_STANDARD, DP, ELTYPELENGTH, &
                      & MSGLENGTH, STAT_SUCCESS, STAT_FAILURE

use wedgePly_elem_module, only : wedgePly_elem
use brickPly_elem_module, only : brickPly_elem

  implicit none
  private

  type, public :: abstPly_elem
    ! encapsulate components of this type
    private
    ! list of type components:
    ! eltype : element type
    ! wedge  : allocated if eltype = 'wedge'
    ! brick  : allocated if eltype = 'brick'
    character(len=ELTYPELENGTH)      :: eltype = ''
    type(wedgePly_elem), allocatable :: wedge      ! wedge sub elements
    type(brickPly_elem), allocatable :: brick      ! brick sub elements
  end type

  interface set
      module procedure set_abstPly_elem
  end interface

  interface integrate
      module procedure integrate_abstPly_elem
  end interface

  interface extract
      module procedure extract_abstPly_elem
  end interface




  public :: set, integrate, extract




  contains
  



  pure subroutine set_abstPly_elem (elem, eltype, connec, ply_angle, &
  & istat, emsg)
  ! Purpose:
  ! this is an interface used to set the contained base element according to
  ! eltype:
  ! - if eltype is brick, the input dummy args are passed to set_brickPly_elem
  ! to define the elem's brick component; elem's wedge component is dealloc.
  ! - if eltype is wedge, the input dummy args are passed to set_wedgePly_elem
  ! to define the elem's wedge component; elem's brick component is dealloc.
  ! ** note:
  ! - size of connec is checked here against eltype to ensure compatibility
  ! the values of connec and other dummy args are not checked here; they are
  ! left for the called eltype's set procedure for checking
  ! - local copies of elem's components are used for set operation;
  ! they are copied to actual elem's components only before successful return
  use wedgePly_elem_module, only : set
  use brickPly_elem_module, only : set

    type(abstPly_elem),          intent(inout) :: elem
    character(len=*),            intent(in)    :: eltype
    integer,                     intent(in)    :: connec(:)
    real(DP),                    intent(in)    :: ply_angle
    integer,                     intent(out)   :: istat
    character(len=MSGLENGTH),    intent(out)   :: emsg


    ! initialize intent out and local variables (non derived type)
    istat = STAT_SUCCESS
    emsg  = ''

    select case (trim(adjustl(eltype)))

      case ('wedge')
          ! check no. of nodes, exit program if incorrect
          if ( size(connec) /= 6 ) then
            istat = STAT_FAILURE
            emsg  = 'size of connec is not 6 for wedge base element, &
            & set, abstPly_elem_module'
            return
          end if
          
          ! set elem type
          elem%eltype = 'wedge'
          
          ! allocate the appropriate base element
          if (.not. allocated(elem%wedge)) allocate(elem%wedge)
          ! deallocate the other base element
          if (allocated(elem%brick)) deallocate(elem%brick)

          ! call the set procedure of the base element
          call set (elem%wedge, connec, ply_angle, istat, emsg)
          ! check istat, if istat is failure, clean up and exit program
          if (istat == STAT_FAILURE) then
            deallocate(elem%wedge)
            return
          end if

      case ('brick')
          ! check no. of nodes, exit program if incorrect
          if ( size(connec) /= 8 ) then
            istat = STAT_FAILURE
            emsg  = 'size of connec is not 8 for brick base element, &
            & set, abstPly_elem_module'
            return
          end if
          
          ! set elem type
          elem%eltype = 'brick'

          ! allocate the appropriate base element
          if (.not. allocated(elem%brick)) allocate(elem%brick)
          ! deallocate the other base element
          if (allocated(elem%wedge)) deallocate(elem%wedge)

          ! call the set procedure of the base element
          call set (elem%brick, connec, ply_angle, istat, emsg)
          ! check istat, if istat is failure, clean up and exit program
          if (istat == STAT_FAILURE) then
            deallocate(elem%brick)
            return
          end if

      case default
          ! this should not be reached, flag an error and return
          istat = STAT_FAILURE
          emsg  = 'unsupported eltype in set, abstPly element module'
          return

    end select

  end subroutine set_abstPly_elem



  pure subroutine extract_abstPly_elem (elem, eltype, fstat, connec, &
  & ply_angle, ig_points, stress, strain, df)
  ! extra modules needed to declare the type of some dummy args
  use lamina_material_module, only : lamina_ig_point
  use wedgePly_elem_module,   only : extract
  use brickPly_elem_module,   only : extract

    type(abstPly_elem),                        intent(in)  :: elem
    character(len=ELTYPELENGTH),        optional, intent(out) :: eltype
    integer,                            optional, intent(out) :: fstat
    integer,               allocatable, optional, intent(out) :: connec(:)
    real(DP),                           optional, intent(out) :: ply_angle
    type(lamina_ig_point), allocatable, optional, intent(out) :: ig_points(:)
    real(DP),                           optional, intent(out) :: stress(NST)
    real(DP),                           optional, intent(out) :: strain(NST)
    real(DP),                           optional, intent(out) :: df

    if (present(eltype)) eltype = elem%eltype

    ! based on eltype, call the respective extract procedure to extract the
    ! requested components

    select case (trim(adjustl(elem%eltype)))

      case ('wedge')
        if (present(fstat))       call extract (elem%wedge, fstat=fstat)
        if (present(connec))      call extract (elem%wedge, connec=connec)
        if (present(ply_angle))   call extract (elem%wedge, ply_angle=ply_angle)
        if (present(ig_points))   call extract (elem%wedge, ig_points=ig_points)
        if (present(stress))      call extract (elem%wedge, stress=stress)
        if (present(strain))      call extract (elem%wedge, strain=strain)
        if (present(df))          call extract (elem%wedge, df=df)

      case ('brick')
        if (present(fstat))       call extract (elem%brick, fstat=fstat)
        if (present(connec))      call extract (elem%brick, connec=connec)
        if (present(ply_angle))   call extract (elem%brick, ply_angle=ply_angle)
        if (present(ig_points))   call extract (elem%brick, ig_points=ig_points)
        if (present(stress))      call extract (elem%brick, stress=stress)
        if (present(strain))      call extract (elem%brick, strain=strain)
        if (present(df))          call extract (elem%brick, df=df)

      case default
      ! this should not be reached

    end select


  end subroutine extract_abstPly_elem



  pure subroutine integrate_abstPly_elem (elem, nodes, material, Kmatrix, &
  & Fvector, istat, emsg, nofailure)
  ! extra modules needed to declare the type of some dummy args
  use fnode_module,           only : fnode
  use lamina_material_module, only : lamina_material
  use wedgePly_elem_module,   only : integrate
  use brickPly_elem_module,   only : integrate

      type(abstPly_elem),    intent(inout) :: elem
      type(fnode),              intent(in)    :: nodes(:)
      type(lamina_material),    intent(in)    :: material
      real(DP), allocatable,    intent(out)   :: Kmatrix(:,:), Fvector(:)
      integer,                  intent(out)   :: istat
      character(len=MSGLENGTH), intent(out)   :: emsg
      logical,        optional, intent(in)    :: nofailure

      ! local variables
      logical :: nofail

      ! initialize intent out and local variable
      istat  = STAT_SUCCESS
      emsg   = ''
      nofail = .false.

      if (present(nofailure)) nofail = nofailure

      ! call the respective integrate procedure
      ! note that in case of error, the appropriate actions on K and F should
      ! have already been done in the called procedure. nothing need to be done
      ! here.
      select case(trim(adjustl(elem%eltype)))
      
        case('wedge')
        
            ! check no. of nodes, exit program if incorrect
            if ( size(nodes) /= 6 ) then
              istat = STAT_FAILURE
              emsg  = 'size of nodes is not 6 for wedge base element, &
              & integrate, abstPly_elem_module'
              return
            end if
            
            call integrate(elem%wedge, nodes, material, Kmatrix, Fvector, &
            & istat, emsg, nofail)

        case('brick')
        
            ! check no. of nodes, exit program if incorrect
            if ( size(nodes) /= 8 ) then
              istat = STAT_FAILURE
              emsg  = 'size of nodes is not 8 for brick base element, &
              & integrate, abstPly_elem_module'
              return
            end if
            
            call integrate(elem%brick, nodes, material, Kmatrix, Fvector, &
            & istat, emsg, nofail)

        case default
            istat = STAT_FAILURE
            emsg  = 'unexpected elem type, integrate, abstPly_elem_module'
            return
            
      end select

  end subroutine integrate_abstPly_elem




end module abstPly_elem_module