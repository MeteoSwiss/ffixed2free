!+****************************************************************************
FUNCTION ToUpper(String)
!=============================================================================
!
! Returns lowercase characters as UPPERCASE
!
! Requires explicit interface in calling program unit.
!
! ____________________________________________________________________________
! I/O   Name            Type    Description
! ____________________________________________________________________________
! I     String          C(*)    String
!
! Author: 1994-1998 Pirmin Kaufmann
!
! Version history:
! 1.0   Basic version
!-----------------------------------------------------------------------------
IMPLICIT NONE
! Dummy arguments
CHARACTER(LEN=*),INTENT(IN)  :: String
CHARACTER(LEN=LEN(String))   :: ToUpper
! Local variables
INTEGER   :: ii
CHARACTER :: Ch

ToUpper = String
Do ii = 1, LEN_TRIM(String)
    Ch = String(ii:ii)
    if ('a' <= Ch .and. Ch <= 'z') then
       ToUpper(ii:ii) = ACHAR(IACHAR(Ch) - 32)
    end if
End Do

END FUNCTION ToUpper
