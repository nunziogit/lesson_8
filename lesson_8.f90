! Exercise 1, Lesson 8
! Allocatable Array rank 2

PROGRAM l8_c1
  !
  ! This program calculates the mean and the standard deviation
  ! of 2*n (unknown) numbers stored in the file dati.dat
  ! over two columns and writes the results to the file output.dat.
  ! The mean is calculated using a function, and the standard deviation
  ! is calculated using a subroutine. The data from dati.dat is stored
  ! in a rank-2 array using dynamic memory allocation.
  ! The data is passed through the argument list.
  !----------------------------------------------------------------------
  ! Variable declarations
  IMPLICIT NONE                              ! No implicit variable declarations
  INTEGER :: i                               ! Loop counter for DO loop
  INTEGER :: ierror                          ! I/O status
  INTEGER :: icont                           ! Counter for data in the file
  INTEGER :: n                               ! Maximum number of data points
  REAL, ALLOCATABLE, DIMENSION(:,:) :: x     ! Allocatable rank-2 array for data

  REAL :: media1, media2                     ! Mean values for the two columns
  REAL :: dev_std_1, dev_std_2               ! Standard deviation for the two columns
  REAL :: somma, calcola_media               ! Variables for sum and the mean function

  ! Open the file dati.dat (the file already exists and is read-only)
  OPEN(UNIT=3, FILE='dati.dat', STATUS='old', ACTION='read', IOSTAT=ierror)

  openif: IF (ierror == 0) THEN
     ! The file was opened successfully: read the values

     ! Read the file dati.dat (number of data points is unknown)
     ! The sole purpose of this routine is to determine n
     icont = 1  ! Initialize counter
     dati_in: DO
        READ(3,*,IOSTAT=ierror)  ! Read the line but store no values
        IF (ierror /= 0) EXIT
        icont = icont + 1
     END DO dati_in
     ! Rewind the file to read it again
     REWIND(3)

     ! The DO loop has terminated: was it due to a read error or the end of the file?
     readif: IF (ierror > 0) THEN
        WRITE(*,*) 'Error at line', icont 
     ELSE readif  ! The end of the data was reached correctly
        WRITE(*,1010) icont - 1
     END IF readif
1010 FORMAT(' ', "End of data reached. There are:", 1x, I6, 1x, "data points in the file")

     ! Assign the number of data points read to n
     n = icont - 1

     ! Now that n is known, allocate the array x
     ALLOCATE(x(2, n))

     ! Read the actual data from the input file
     DO i = 1, n
        READ(3,*) x(1,i), x(2,i)
     END DO

     !write(6,*) x(1,:)
     !read(5,*)

     ! Calculate the means for both columns
     media1 = calcola_media(x(1,:), n)
     media2 = calcola_media(x(2,:), n)

     ! Calculate the standard deviations for both columns
     CALL calcola_varianza(x(1,:), media1, n, dev_std_1)
     CALL calcola_varianza(x(2,:), media2, n, dev_std_2)

     ! Write the mean and standard deviation results to a file in a specific format
     OPEN(UNIT=4, FILE='output.dat', STATUS='replace', ACTION='write')
     WRITE(4,1030) media1, media2
     WRITE(4,1040) dev_std_1, dev_std_2
     CLOSE(4)

1030 FORMAT("The means of the data are:", 1x, E10.3, 1x, E10.3)
1040 FORMAT("The standard deviations of the data are:", 1x, ES10.3, 1x, ES10.3)

  ELSE openif
     ! Deallocate memory in case of error
     DEALLOCATE(x)
     ! Close the file
     CLOSE(3)
     WRITE(*,*) "Error opening the file: please check the data"
  END IF openif

  STOP
END PROGRAM l8_c1

!--------------------------------------------------------------------------
REAL FUNCTION calcola_media(array, n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(IN), DIMENSION(n) :: array
  ! Calculate the mean
  calcola_media = sum(array) / REAL(n)
  RETURN
END FUNCTION calcola_media
!--------------------------------------------------------------------------
SUBROUTINE calcola_varianza(array, media, n, varianza)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(IN), DIMENSION(n) :: array
  REAL, INTENT(IN) :: media
  REAL, INTENT(OUT) :: varianza
  ! Calculate the standard deviation
  varianza = SQRT(sum((array - media)**2) / REAL(n))
  RETURN
END SUBROUTINE calcola_varianza

