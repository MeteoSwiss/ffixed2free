MODULE m_linked_list_indent_subroutines

  ! Description:

  ! This module contains both the structure definition of the items of a 
  ! linked list and the procedures to manipulate the list.

  ! M. Schraner, MeteoSwiss, 27 October 2011

  IMPLICIT NONE

  !--- Module variables:
  TYPE linked_list_item              ! Structure definition of the items 
     CHARACTER(LEN=20) :: keyword
     CHARACTER(LEN=200) :: name
     TYPE (linked_list_item), POINTER :: previous => null()  
     TYPE (linked_list_item), POINTER :: next => null()
  END TYPE linked_list_item

  TYPE (linked_list_item), POINTER, PRIVATE :: head => null(), tail => null()
  

CONTAINS

  
  SUBROUTINE new_item (keyword, name, item)

    ! Initialize new item.

    ! Dummy arguments:
    CHARACTER(LEN=*), INTENT(in) :: keyword, name
    TYPE (linked_list_item), POINTER :: item

    ! Local variable:
    INTEGER :: err

    ! Executable statements:

    ! Create new item:
    ALLOCATE(item, STAT=err)
    IF (err /= 0) THEN 
       ! Print error message and terminate processing:
       PRINT *, "Machine out of memory"
       STOP
    END IF

    ! Assign a value to each field of the new item:
    item%keyword = keyword
    item%name = name

  END SUBROUTINE new_item


  SUBROUTINE add_at_tail (item)

    ! Add new item at the end of the linked list.

    ! Dummy argument:
    TYPE (linked_list_item), POINTER :: item

    ! Executable statements:

    ! Check to see if the list is empty:
    IF (ASSOCIATED(head)) THEN          ! List is not empty
       tail%next => item                ! Attach new item at the end of list
       item%previous => tail
       tail => item                     ! Reset tail pointer
    ELSE                                ! List is empty
       head => item                     ! Start up list with item
       tail => item
    END IF
  END SUBROUTINE add_at_tail

  
  SUBROUTINE get_and_delete_at_tail (item)
    
    ! Return item at the end of the linked list and remove it from the list.

    ! Dummy argument:
    TYPE (linked_list_item), POINTER :: item

    ! Executable statements:

    ! Check to see if the list is empty:
    IF (ASSOCIATED(tail)) THEN         ! List is not empty

       item => tail                    ! Return pointer to last item

       ! Check to see if there is more than one item in the list:
       IF (ASSOCIATED(tail%previous)) THEN   ! More than one item in the list
          tail => tail%previous        ! Remove item from the list
          tail%next => null()          ! No successor
       ELSE                            ! Only one item in the list
          head => null()               ! List is now empty
          tail => null()
       END IF

    ELSE                               ! List is empty

       item => null()                  ! Return no element

    END IF

  END SUBROUTINE get_and_delete_at_tail


  SUBROUTINE get_at_tail (item)

    ! Return item at the end of the linked list.

    ! Dummy argument:
    TYPE (linked_list_item), POINTER :: item

    ! Executable statements:

    ! Check to see if the list is empty:
    IF (ASSOCIATED(tail)) THEN         ! List is not empty
       item => tail                    ! Return pointer to last item
    ELSE                               ! List is empty
       item => null()                  ! Return no element
    END IF

  END SUBROUTINE


  SUBROUTINE list

    ! List the contents of the linked_list.

    ! Local variable:
    TYPE (linked_list_item), POINTER :: ptr

    ! Executable statements:

    PRINT *, " "
    PRINT *, "Current items of linked list:"

    ! Check whter list is empty:
    IF (.NOT. ASSOCIATED(head)) THEN   ! List is empty

       ! Print message:
       PRINT *, "List is empty."

    ELSE                               ! List contains at least one item

       ! Set local pointer to head of list:
       ptr => head

       ! Loop to print all items in the list:
       DO
          ! Print details of this item:
          PRINT *, ptr%keyword, ptr%name
       
          ! Set pointer to next item:
          ptr => ptr%next

          ! Exit loop if there are no more items in the list:
          IF (.NOT. ASSOCIATED(ptr)) EXIT
       END DO

    END IF

  END SUBROUTINE list

END MODULE m_linked_list_indent_subroutines
          
    
