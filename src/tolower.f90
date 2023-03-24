!+****************************************************************************
FUNCTION ToLower(String)
!=============================================================================
!
! Returns UPPERCASE characters as lowercase.
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
CHARACTER(LEN=LEN(String))   :: ToLower
! Local variables
INTEGER   :: ii
CHARACTER :: Ch

ToLower = String
Do ii = 1, LEN_TRIM(String)
    Ch = String(ii:ii)
    if ('A' <= Ch .and. Ch <= 'Z') then
       ToLower(ii:ii) = ACHAR(IACHAR(Ch) + 32)
    end if
End Do

END FUNCTION ToLower
