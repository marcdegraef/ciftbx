





program CIFextract

IMPLICIT NONE

include 'ciftbx90.cmf'
include 'ciftbx90.cmv'


logical                         :: f1,f2,f3,f4,f5,f6,more, occwarning, dbwwarning
character(NUMCHAR)              :: name, SGnumchar, progname
character(80)                   :: line, xtalname
character(4)                    :: label(6)
character(26)                   :: alpha
real                            :: cela,celb,celc,siga,sigb,sigc,celalpha,celbeta,celgamma
real                            :: x,y,z,u,sx,sy,sz,su, occ, socc
real                            :: numb,sdev,dum
real                            :: xf(6),yf(6),zf(6),uij(6,6)
integer                         :: i,j,nsite, SGnum, stat, xtal_system, ipos
integer,parameter               :: dataunit = 20

! first space group number for each crystal system, and conversion to EMsoft crystal system number
integer,parameter               :: SGCS(7) = (/ 1, 3, 16, 75, 143, 168, 195 /)
!                                                    a  m  o  t  R  h  c
integer,parameter               :: EMsoft_xs(7) = (/ 7, 6, 3, 2, 5, 4, 1 /)

!> numbers of the space groups with two settings
integer,parameter               :: tworig(24)=(/48,50,59,68,70,85,86,88,125,126,129,130,133,134,137,138,&
                                                141,142,201,203,222,224,227,228/)

integer                         :: numarg
integer                         :: iargc        !< external function for command line
character(NUMCHAR)              :: arg, CIFname, txtname 

character(2), parameter :: ATOM_sym(92)=(/' H','He','Li','Be',' B',' C',' N',' O',' F','Ne', &
                                          'Na','Mg','Al','Si',' P',' S','Cl','Ar',' K','Ca', &
                                          'Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu','Zn', &
                                          'Ga','Ge','As','Se','Br','Kr','Rb','Sr',' Y','Zr', &
                                          'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn', &
                                          'Sb','Te',' I','Xe','Cs','Ba','La','Ce','Pr','Nd', &
                                          'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb', &
                                          'Lu','Hf','Ta',' W','Re','Os','Ir','Pt','Au','Hg', &
                                          'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th', &
                                          'Pa',' U' /)

!>  SYM_SGname all space group names
! TRICLINIC SPACE GROUPS
character(11),parameter :: SGHMsymbol(230)= (/" P  1      " ," P -1      ", & ! MONOCLINIC SPACE GROUPS
        " P 2       " ," P 21      " ," C 2       " ," P m       ", &
        " P c       " ," C m       " ," C c       " ," P 2/m     ", &
        " P 21/m    " ," C 2/m     " ," P 2/c     " ," P 21/c    ", &
        " C 2/c     ", &                                              ! ORTHORHOMBIC SPACE GROUPS
        " P 2 2 2   " ," P 2 2 21  " ," P 21 21 2 " ," P 21 21 21", &
        " C 2 2 21  " ," C 2 2 2   " ," F 2 2 2   " ," I 2 2 2   ", &
        " I 21 21 21" ," P m m 2   " ," P m c 21  " ," P c c 2   ", &
        " P m a 2   " ," P c a 21  " ," P n c 2   " ," P m n 21  ", &
        " P b a 2   " ," P n a 21  " ," P n n 2   " ," C m m 2   ", &
        " C m c 21  " ," C c c 2   " ," A m m 2   " ," A b m 2   ", &
        " A m a 2   " ," A b a 2   " ," F m m 2   " ," F d d 2   ", &
        " I m m 2   " ," I b a 2   " ," I m a 2   " ," P m m m   ", &
        " P n n n   " ," P c c m   " ," P b a n   " ," P m m a   ", &
        " P n n a   " ," P m n a   " ," P c c a   " ," P b a m   ", &
        " P c c n   " ," P b c m   " ," P n n m   " ," P m m n   ", &
        " P b c n   " ," P b c a   " ," P n m a   " ," C m c m   ", &
        " C m c a   " ," C m m m   " ," C c c m   " ," C m m a   ", &
        " C c c a   " ," F m m m   " ," F d d d   " ," I m m m   ", &
        " I b a m   " ," I b c a   " ," I m m a   ", &                ! TETRAGONAL SPACE GROUPS  
        " P 4       " ," P 41      " ," P 42      " ," P 43      ", &
        " I 4       " ," I 41      " ," P -4      " ," I -4      ", &
        " P 4/m     " ," P 42/m    " ," P 4/n     " ," P 42/n    ", &
        " I 4/m     " ," I 41/a    " ," P 4 2 2   " ," P 4 21 2  ", &
        " P 41 2 2  " ," P 41 21 2 " ," P 42 2 2  " ," P 42 21 2 ", &
        " P 43 2 2  " ," P 43 21 2 " ," I 4 2 2   " ," I 41 2 2  ", &
        " P 4 m m   " ," P 4 b m   " ," P 42 c m  " ," P 42 n m  ", &
        " P 4 c c   " ," P 4 n c   " ," P 42 m c  " ," P 42 b c  ", &
        " I 4 m m   " ," I 4 c m   " ," I 41 m d  " ," I 41 c d  ", &
        " P -4 2 m  " ," P -4 2 c  " ," P -4 21 m " ," P -4 21 c ", &
        " P -4 m 2  " ," P -4 c 2  " ," P -4 b 2  " ," P -4 n 2  ", &
        " I -4 m 2  " ," I -4 c 2  " ," I -4 2 m  " ," I -4 2 d  ", &
        " P 4/m m m " ," P 4/m c c " ," P 4/n b m " ," P 4/n n c ", &
        " P 4/m b m " ," P 4/m n c " ," P 4/n m m " ," P 4/n c c ", &
        " P 42/m m c" ," P 42/m c m" ," P 42/n b c" ," P 42/n n m", &
        " P 42/m b c" ," P 42/m n m" ," P 42/n m c" ," P 42/n c m", &
        " I 4/m m m " ," I 4/m c m " ," I 41/a m d" ," I 41/a c d", & ! RHOMBOHEDRAL SPACE GROUPS  
        " P 3       " ," P 31      " ," P 32      " ," R 3       ", &
        " P -3      " ," R -3      " ," P 3 1 2   " ," P 3 2 1   ", &
        " P 31 1 2  " ," P 31 2 1  " ," P 32 1 2  " ," P 32 2 1  ", &
        " R 3 2     " ," P 3 m 1   " ," P 3 1 m   " ," P 3 c 1   ", &
        " P 3 1 c   " ," R 3 m     " ," R 3 c     " ," P -3 1 m  ", &
        " P -3 1 c  " ," P -3 m 1  " ," P -3 c 1  " ," R -3 m    ", &
        " R -3 c    ", &                                              ! HEXAGONAL SPACE GROUPS   
        " P 6       " ," P 61      " ," P 65      " ," P 62      ", &
        " P 64      " ," P 63      " ," P -6      " ," P 6/m     ", &
        " P 63/m    " ," P 6 2 2   " ," P 61 2 2  " ," P 65 2 2  ", &
        " P 62 2 2  " ," P 64 2 2  " ," P 63 2 2  " ," P 6 m m   ", &
        " P 6 c c   " ," P 63 c m  " ," P 63 m c  " ," P -6 m 2  ", &
        " P -6 c 2  " ," P -6 2 m  " ," P -6 2 c  " ," P 6/m m m ", &
        " P 6/m c c " ," P 63/m c m" ," P 63/m m c", &                ! CUBIC SPACE GROUPS
        " P 2 3     " ," F 2 3     " ," I 2 3     " ," P 21 3    ", &
        " I 21 3    " ," P m 3     " ," P n 3     " ," F m 3     ", &
        " F d 3     " ," I m 3     " ," P a 3     " ," I a 3     ", &
        " P 4 3 2   " ," P 42 3 2  " ," F 4 3 2   " ," F 41 3 2  ", &
        " I 4 3 2   " ," P 43 3 2  " ," P 41 3 2  " ," I 41 3 2  ", &
        " P -4 3 m  " ," F -4 3 m  " ," I -4 3 m  " ," P -4 3 n  ", &
        " F -4 3 c  " ," I -4 3 d  " ," P m 3 m   " ," P n 3 n   ", &
        " P m 3 n   " ," P n 3 m   " ," F m 3 m   " ," F m 3 c   ", &
        " F d 3 m   " ," F d 3 c   " ," I m 3 m   " ," I a 3 d   " /)

! first of all we need to print out the obligatory license and no-warranty information


! then we process the arguments which are either -h to print the help information
! or two arguments, namely the input CIF file and the output .txt file
 progname = 'CIFextract'
 numarg = iargc()
 if (numarg.gt.0) then ! there is at least one argument
  do i=1,numarg
    call getarg(i,arg)
    write (*,*) 'Found the following argument: '//trim(arg)
    if (i.eq.1) then
! does the argument start with a '-' character?    
      if (arg(1:1).eq.'-') then
        if (trim(arg).eq.'-h') then
          write(*,"(/' Program should be called as follows: ')")
          write(*,"('        CIFextract [-h] [CIFfile txtfile] ')")
          write(*,"(' To produce this message, type ',A,' -h')") trim(progname)
          write(*,"(' To extract information from a CIF file, list both the CIF file name')")
          write(*,"(' and the output text file name')")
         stop
        end if
      else
        CIFname = trim(arg)
      end if
    else
      txtname = trim(arg)
    end if 
  end do
 end if


write(*,'(/a)') ' Opening CIF file '//trim(CIFname)
write(*,'(/a)') ' Opening txt file '//trim(txtname)

open (unit=dataunit, file=trim(txtname), status='unknown',form='formatted')
 
!....... Assign the CIFtbx files
f1 = init_( 1, 2, 3, 6 )
 
!        Apply new clipping action for text fields
clipt_ = .true.
pclipt_ = .true.

!....... Open the CIF to be accessed
if (.not.ocif_(CIFname)) then
  write(*,'(a///)')  ' >>>>>>>>> CIF cannot be opened'
  stop
end if
write(*,'(/2a/)')  ' Reading data from CIF  ',trim(CIFname)

!....... Assign the data block to be accessed
if (.not.data_(' ')) then  
  write(*,'(/a/)')   ' >>>>>>> No data_ statement found'
  stop
end if 
write(*,'(/a,a/)') ' Accessing items in data block  ',bloc_
 
!....... First determine the crystal system; we'll do that from the space group number if 
! it is present; if not present, then we ask the user for the space group number
f1 = char_('_symmetry_Int_Tables_number', SGnumchar)
write (*,*) 'f1 = ',f1
if (f1.eqv..FALSE.) then
  read(SGnumchar,*,iostat=stat) SGnum
  write (*,*) ' Found Space Group number : ', SGnum
else
  write (*,"('Please enter the space group number (standard setting) : ',$)") 
  read (*,*) SGnum
end if 

i=1
do while (SGnum.gt.SGCS(i))
  i = i+1
end do
xtal_system = EMsoft_xs(i-1)
write (*,*) '   --->  EMsoft crystal system number : ',xtal_system
 
!....... Extract space group notation (expected char string)
! We use this to detect whether or not the space group has a non-standard setting, for
! instance Pbnm for the standard setting Pnma...; the goal is to automatically detect which
! setting is being used for monoclinic and orthorhombic space groups and to automatically 
! do the conversion to the standard setting ... 
f1 = char_('_symmetry_space_group_name_H-M', name)
if (f1.eqv..TRUE.) then
  if (trim(name(1:long_)).ne.trim(adjustl(SGHMsymbol(SGnum)))) then
    write (*,"('**************** ALERT ****************')")
    write (*,*) 'The space group symbol '//trim(name(1:long_))//' in the CIF file '
    write (*,*) 'is different from the standard setting '//trim(adjustl(SGHMsymbol(SGnum)))
  end if
end if

! 1. write the crystal system
! we need to test for the trigonal system whether the setting is hexagonal or rhombohedral
write(dataunit,"(I1)") xtal_system
if (xtal_system.eq.5) then 
  f1 = numb_('_cell_angle_gamma', celgamma, sigc)
  if (celgamma.eq.120.0) then 
    xtal_system = 4
    write(dataunit,"(I1)") 1
  else
    write(dataunit,"(I1)") 0
  end if
end if

! 2. lattice parameters (only the ones that are needed)
! a is always needed
 siga = 0.
 sigb = 0.
 sigc = 0.
 f1 = numb_('_cell_length_a', cela, siga)
 write(dataunit,"(F10.6)") cela/10.0
 select case (xtal_system)
    case (1)
  ! tetragonal
    case (2)
     f3 = numb_('_cell_length_c', celc, sigc)
     write(dataunit,"(F10.6)") celc/10.0
  ! orthorhombic
    case (3)
     f2 = numb_('_cell_length_b', celb, sigb)
     f3 = numb_('_cell_length_c', celc, sigc)
     write(dataunit,"(F10.6)") celb/10.0
     write(dataunit,"(F10.6)") celc/10.0
  ! hexagonal
    case (4)
     f3 = numb_('_cell_length_c', celc, sigc)
     write(dataunit,"(F10.6)") celc/10.0
  ! rhombohedral 
    case (5)
     f4 = numb_('_cell_angle_alpha', celalpha, siga)
     write(dataunit,"(F10.6)") celalpha
  ! monoclinic   
    case (6)
     f2 = numb_('_cell_length_b', celb, sigb)
     f3 = numb_('_cell_length_c', celc, sigc)
     f5 = numb_('_cell_angle_beta', celbeta, sigb)
     write(dataunit,"(F10.6)") celb/10.0
     write(dataunit,"(F10.6)") celc/10.0
     write(dataunit,"(F10.6)") celbeta
  ! triclinic    
    case (7) 
     f2 = numb_('_cell_length_b', celb, sigb)
     f3 = numb_('_cell_length_c', celc, sigc)
     f4 = numb_('_cell_angle_alpha', celalpha, siga)
     f5 = numb_('_cell_angle_beta', celbeta, sigb)
     f6 = numb_('_cell_angle_gamma', celgamma, sigc)
     write(dataunit,"(F10.6)") celb/10.0
     write(dataunit,"(F10.6)") celc/10.0
     write(dataunit,"(F10.6)") celalpha
     write(dataunit,"(F10.6)") celbeta
     write(dataunit,"(F10.6)") celgamma
 end select

! 3. space group number
 write(dataunit,"(I3)") SGnum

! 4. some space groups have a second setting
 if (minval(tworig-SGnum).eq.0) then 
   write (*,*) 'This space group has two origin settings; please make sure the correct '
   write (*,*) 'is used in the output text file '
   write(dataunit,"(I1)") 1
 end if

!
!
!....... Get the next name in the CIF and print it out
!
! f1 = name_(name)
! write(6,'(a,a/)') ' Next data name in CIF is   ', name(1:max(32,lastnb(name)))
!
!
!
!....... Extract atom site data in a loop
!
write (*,*) 'Extracting atom types, coordinates, site occupations, and Debye-Waller factors'
ipos = 1
occwarning = .FALSE.
dbwwarning = .FALSE.
more = .TRUE.
do while (more)
  f1 = char_('_atom_site_type_symbol', name)
  if (f1) then
! get the atomic number
    i = 1
    do while (trim(adjustl(ATOM_sym(i))).ne.trim(name(1:long_))) 
      i = i+1
    end do
    write (dataunit,"(I2)") i
  else
    write (*,"('**************** ALERT ****************')")
    write (*,"('An atom symbol is missing from the CIF file;')")
    write (*,"('The atomic number 0 is entered instead. Please edit the text file as needed.'//)")
    write (dataunit,"(I2)") 0
  end if
! get the coordinates, site occupation, and Debye-Waller factors  
  sx = 0.
  sy = 0.
  sz = 0.
  su = 0.
  socc = 0.
  f1 = numb_('_atom_site_occupancy', occ, socc)
  if (.not.f1) then 
    occ = 1.0
    occwarning = .TRUE.
  end if
  f2 = numb_('_atom_site_fract_x', x, sx)
  f2 = numb_('_atom_site_fract_y', y, sy)
  f2 = numb_('_atom_site_fract_z', z, sz)
  f3 = numb_('_atom_site_U_iso_or_equiv', u, su)
  if (.not.f3) then
    u = 0.004
    dbwwarning = .TRUE.
  else
    u = u/100.
  end if
  write(dataunit,"(4(f10.6,','),f10.6)") x,y,z,occ,u
  if(loop_) then 
    ipos = ipos + 1
    write (dataunit,"('y')")
  else
    more = .FALSE.
    write (dataunit,"('n')")
  end if
end do
write (*,"('Number of unique atom positions found ',I3//)") ipos 

if (occwarning) then
     write (*,"('**************** ALERT ****************')")
     write (*,"('Some or all of the occupation parameters are missing from the CIF file')")
     write (*,"('and were replaced by the value 1.000. Please edit the text file as needed.'//)")
end if

if (dbwwarning) then
     write (*,"('**************** ALERT ****************')")
     write (*,"('Some or all of the Debye-Waller factors are missing from the CIF file')")
     write (*,"('and were replaced by the value 0.004 [nm^2]. Please edit the text file as needed.'//)")
end if

write (*,"('Enter the crystal structure file name (*.xtal) ')")
read (*,*) xtalname
write (dataunit,"(A)") trim(xtalname)
write (dataunit,"('''',A,'''')") trim(CIFname)
write (*,*) 'The CIF file name '//trim(CIFname)//' will be used as the source line in the output file'
write (*,"(//'data extraction complete'//)")

close(unit=dataunit,status='keep')


end program CIFextract
