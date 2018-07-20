C
C
C
C                     CIF Tool Box Application 'tbx_ex3.f'
C                     ------------------------
C                     Version: November 2009
C
C
C     This file contains four separate examples of how to apply the
C     the CIF tool box 'CIFtbx'. These are simple software tools for
C     accessing and creating data in CIF format. A description of the
C     CIFtbx functions and user-accessible system variables is given
C     at the start of the tool box source file 'ciftbx.f'.
C
C     A full description of a CIF is given in the paper Hall, Allen
C     and Brown (1991) Acta Cryst. A47 pp655-685.
C
C     Communications about the application of the CIF tool box may be
C     directed to: Syd Hall fx:61(9)3801118 em:syd@crystal.uwa.edu.au
C
C     The examples shown below extract data from the supplied CIF
C     'test.cif'. The third example reads a list of data requests from
C     a supplied file 'test.req'. As distributed, 'tbx_ex3.f' attaches
C     'test.cif' to device 1, a scratch direct access file to device 3
C     and the error message file to device 6 (i.e. stdout). The fourth
C     example creates a new CIF called 'test.new'. This output file is
C     attached to device 2. On some machines these may not be suitable
C     device numbers and they may be changed within the application
C     with the init_ call.
C
C     The print file for this application is assumed to be stdout (i.e.
C     device number 6). The dictionary checking option dict_ has been
C     invoked in these examples. In this case the 2006 expanded Core
C     dictionary file 'cif_expanded.dic' has been used [note that other
C     dictionaries may be added by repetitive invocation of dict_]. If
C     it is not present a warning message will be issued and the run
C     will proceed without data name validation.
C
C
C     The user should note that most CIFtbx tools are logical functions.
C     If their invocation succeeds they are returned with value .true.
C     otherwise .false. This means that for each invocation the program
C     can take appropriate action. For example if a request to open a
C     specific CIF fails, the program can exit, or try another CIF. The
C     same philosophy applies to data_ and all CIFtbx functions. However,
C     if a function discovers a logical construction error in the CIF,
C     the whole process will stop with an error message.
C
C
C
C     Note the following requirements for any CIFtbx application.
C
C....... The tool box fortran source file 'ciftbx.f' must be included
C        once in the application source (as shown below), OR the object
C        file 'ciftbx.o' must be included in the link process.
C....... The fortran source file 'hash_funcs.f" must also be included
C        once in the application source (as shown below), OR the object
C        file 'hash_funcs.o' must be included in the link process.
C
         include       'ciftbx.f'
         include       'hash_funcs.f'
C
C
C....... The tool box common variable file 'ciftbx.cmn' must be present
C        at the start of EACH routine using the CIFtbx functions.
C
         include       'ciftbx.cmn'
C
C
C
         logical       f1,f2,f3
         character*32  name
         character*80  line
         character*4   label(6)
         character*26  alpha
         character*2   rsolidus
         character*1   slash
         real          cela,celb,celc,siga,sigb,sigc
         real          x,y,z,u,sx,sy,sz,su
         real          numb,sdev,dum
         real          xf(6),yf(6),zf(6),uij(6,6)
         integer       i,j,nsite
         data alpha    /'abcdefghijklmnopqrstuvwxyz'/
         data          cela,celb,celc,siga,sigb,sigc/6*0./
         data          x,y,z,u,sx,sy,sz,su/8*0./
         data          xf,yf,zf,uij/54*0./
         data          rsolidus/'\\'/

         slash = rsolidus(1:1)
C
C
C
C........................... Example 1 .......................................
C
C
C   This example illustrates how to extract non-loop and loop items.
C   Note carefully how the logical functions numb_ and char_ signal if
C   the request has been successful or not. Note how the logical variables
C   text_ and loop_ are used to control the text lines and the data loops.
C
C
C
C....... Assign the CIFtbx files
C
         f1 = init_( 1, 2, 3, 6 )
C
C....... Request dictionary validation check
C
         rdprn_ = .true.
         rdbkt_ = .true.
         rdbrc_ = .true.
         if(dict_('cif_expanded.dic','valid dtype'))  goto 100
         write(6,'(/a/)') ' Requested Core dictionary not present'
C
C....... Open the CIF to be accessed
C
100      name='test.cif'
         write(6,'(/2a/)')  ' Read data from CIF  ',name
         if(ocif_(name))      goto 120
         write(6,'(a///)')  ' >>>>>>>>> CIF cannot be opened'
         stop
C
C....... Assign the data block to be accessed
C
120      if(data_(' '))       goto 130
         write(6,'(/a/)')   ' >>>>>>> No data_ statement found'
         stop
130      write(6,'(/a,a/)') ' Access items in data block  ',bloc_
C
C
C....... Extract some cell dimensions; test all is OK
C
         siga = 0.
         sigb = 0.
         sigc = 0.
         f1 = numb_('_cell_length_a', cela, siga)
         f2 = numb_('_cell_length_b', celb, sigb)
         f3 = numb_('_cell_length_c', celc, sigc)
         if(.not.(f1.and.f2.and.f3))
     *   write(6,'(a)') ' Cell dimension(s) missing!'
         write(6,'(/a,3f10.4)') ' Cell ',cela,celb,celc
         write(6,'(a,3f10.4/)') '      ',siga,sigb,sigc
C
C
C....... Extract space group notation (expected char string)
C
         f1 = char_('_symmetry_space_group_name_Hall', name)
         write(6,'(a,a/)') ' Space group   ',name(1:long_)
C
C
C....... Get the next name in the CIF and print it out
C
         f1 = name_(name)
         write(6,'(a,a/)') ' Next data name in CIF is   ',name
C
C
C....... List the audit record (possible text line sequence)
C
         write(6,'(a/)') ' Audit record'
140      f1 = char_('_audit_update_record', line)
         write(6,'(a)') line
         if(text_)  goto 140
C
C
C....... Extract atom site data in a loop
C
         write(6,'(/a/)') ' Atom sites'
160      f1 = char_('_atom_site_label', name)
         sx = 0.
         sy = 0.
         sz = 0.
         f2 = numb_('_atom_site_fract_x', x, sx)
         f2 = numb_('_atom_site_fract_y', y, sy)
         f2 = numb_('_atom_site_fract_z', z, sz)
         f3 = numb_('_atom_site_U_iso_or_equiv', u, su)
         write(6,'(1x,a4,8f8.4)') name,x,y,z,u,sx,sy,sz,su
         if(loop_)  goto 160
C
C
C
C
C........................... Example 2 .......................................
C
C
C
C   In this example two separate data blocks are accessed. The first
C   contains looped publication authors and text addresses. The second
C   part of this example shows how data from two different loops may
C   be merged. Remember that data items from different loops may NOT
C   be accessed simultaneously, as this causes the CIFtbx loop counters
C   to be reset to the start of the loop (see Example 3).
C
C
C
C....... List the author addresses from publication data block
C
         if(data_('publication'))
     *   write(6,'(//a,a/)') ' Access items in data block  ',bloc_
         write(6,'(/a)') ' Author list'
C
210      f1 = char_('_publ_author_name', line)
         write(6,'(/1x,a)') line(1:long_)
C
220      f1 = char_('_publ_author_address', line)
         if(line(1:10).eq.'          ') goto 230
         write(6,'(1x,a)') line(1:50)
230      if(text_) goto 220
         if(loop_) goto 210

C
C
C....... Read and store the atom site data from other data block
C
         f1 = data_('mumbo_jumbo')
         write(6,'(///a,a/)') ' Access items in data block  ',bloc_
C
         nsite = 0
240      nsite = nsite+1
         f1 = char_('_atom_site_label', label(nsite))
         sx = 0.
         sy = 0.
         sz = 0.
         f2 = numb_('_atom_site_fract_x',  xf(nsite), sx)
         f2 = numb_('_atom_site_fract_y',  yf(nsite), sy)
         f2 = numb_('_atom_site_fract_z',  zf(nsite), sz)
         do 250 i=1,6
250      uij(nsite,i)=0.0
         if(loop_) goto 240
C
C....... Read the Uij loop and store in the site list
C
260      f1 = char_('_atom_site_aniso_label', name)
         do 270 i=1,nsite
         if(label(i).eq.name) goto 280
270      continue
         write(6,'(a)') ' Label mismatch between atom lists'
280      f1 = numb_('_atom_site_aniso_U_11', uij(i,1), dum)
         f1 = numb_('_atom_site_aniso_U_22', uij(i,2), dum)
         f1 = numb_('_atom_site_aniso_U_33', uij(i,3), dum)
         f1 = numb_('_atom_site_aniso_U_12', uij(i,4), dum)
         f1 = numb_('_atom_site_aniso_U_13', uij(i,5), dum)
         f1 = numb_('_atom_site_aniso_U_23', uij(i,6), dum)
         if(loop_) goto 260
C
C....... List the atom site data
C
         write(6,'(/a/)') ' Atom coordinates and Uij'
         do 290 i=1,nsite
         if(uij(i,1).gt.0.0001) goto 285
         write(6,'(1x,a,3f8.4)') label(i),xf(i),yf(i),zf(i)
         goto 290
285      write(6,'(1x,a,9f8.4)') label(i),xf(i),yf(i),zf(i),
     *                           (uij(i,j),j=1,6)
290      continue
C
C
C
C
C........................... Example 3 .......................................
C
C
C   This example serves to illustrate how a general list of data requests
C   may be handled. The logical function test_ is used to identify the
C   nature of the requested data item, and then numb_ and char_ are invoked
C   when applicable. The supplied list of requests on 'test.req' is not of
C   particular significance. They are intentionally jumbled up to show what
C   happens if a non-loop item is called within a loop [WARNING: CIFtbx
C   interprets this as a signal to end the loop and the next call for a loop
C   item will extract data from its first packet! Look at the output listing
C   to see what happens.]
C
C
C
C....... Loop over the data request file
C
         open(unit=8,file='test.req',status='old')
300      read(8,'(a)',end=400) name
C
         f1 = test_(name)
         write(6,'(/a,3x,a,2i5)') name,type_,long_,list_
C
         if(type_.ne.'numb')      goto 320
         sdev = 0.
         f1 = numb_(name, numb, sdev)
         write(6,'(2f10.4)') numb,sdev
         goto 300
C
320      if(type_.ne.'char')      goto 340
         f1 = char_(name, line)
         write(6,'(a)') line(1:long_)
         goto 300
C
340      if(type_.ne.'text')      goto 300
350      f1 = char_(name, line)
         write(6,'(a)') line
         if(text_)  goto 350
         goto 300
C
C
C
C
C........................... Example 4 .......................................
C
C
C
C   In this example a new CIF is created. Note that it will not overwrite
C   an existing CIF of the same name. Note also that reading and existing
C   CIF and writing a new CIF is possible at the same time, so that it is
C   feasible to use these tools to update or modify and existing CIF.
C   Some examples of the control of esd output are given.
C
C
C
C....... Open a new CIF
C
400      if(pfile_('m3test.new'))  goto 450
         write(6,'(//a/)') ' Output CIF by this name exists already!'
         goto 500
C
C....... Insert a data block code
C
450      f1 = pdata_('whoops_a_daisy')
C
C....... Enter various single data items to show how
C
         f1 = pchar_('_audit_creation_method','using CIFtbx')
         f1 = pchar_('_audit_creation_extra1','using_CIFtbx')
         f1 = pchar_('_audit_creation_extra2',"Terry O'Connell")
         f1 = pchar_('_audit_creation_extra3','Terry O"Connell')
C
         f1 = ptext_('_audit_creation_record',' Text data may be ')
         f1 = ptext_('_audit_creation_record',' entered like this')
         f1 = ptext_('_audit_creation_record',' or in a loop.')
C
         f1 = pnumb_('_cell_measurement_temperature', 293., 0.)
         f1 = pnumb_('_cell_volume', 1759.0, 13.)
         f1 = pnumb_('_cell_length_junk', 8.75353553524313,0.)
         f1 = pnumb_('_cell_length_c', 19.737, .003)
C
C....... Enter some looped data
C
         f1 = ploop_('_atom_type.symbol')
         f1 = ploop_('_atom_type.oxidation_number')
         f1 = ploop_('_atom_type.number_in_cell')
         do 470 i=1,10
         f1 = pchar_(' ',alpha(1:i))
         f1 = pnumb_(' ',float(i),float(i)*0.1)
470      f1 = pnumb_(' ',float(i)*8.64523,0.)
C
C....... Do it again but as contiguous data with text data
C
         f1 = ploop_('_atom_type_symbol')
         f1 = ploop_('_atom_type_oxidation_number')
         f1 = ploop_('_some_silly_text')
         do 480 i=1,3
         f1 = pchar_(' ',alpha(1:i))
         f1 = pnumb_(' ',float(i),float(i)*0.1)
480      f1 = ptext_(' ',' Hi Ho the diddly oh!')
C
C....... Now present some comments and various numeric and esd formats
C
         f1 = pcmnt_(' ')
         f1 = pcmnt_(' Loops with various numeric and esd formats')
         f1 = ploop_('_various_numeric_data')
         esdlim_ = 19
         pdecp_ = .false.
         plzero_ = .false.
         f1 = pcmnt_(' ')
         f1 = pcmnt_(
     *    ' esdlim_ = 19, pdecp_ = .false., plzero_ = .false.')
         f1 = pnumb_(' ', -.01, 1.)
         f1 = pnumb_(' ', -.1, 10.)
         f1 = pnumb_(' ',-1.,100.)
         f1 = pnumb_(' ',1.,100.)
         pdecp_ = .true.
         plzero_ = .false.
         f1 = pcmnt_(' ')
         f1 = pcmnt_(
     *    ' esdlim_ = 19, pdecp_ = .true., plzero_ = .false.')
         f1 = pnumb_(' ', -.01, 1.)
         f1 = pnumb_(' ', -.1, 10.)
         f1 = pnumb_(' ',-1.,100.)
         f1 = pnumb_(' ',1.,100.)
         esdlim_ = -9999
         plzero_ = .true.
         f1 = pcmnt_(' ')
         f1 = pcmnt_(
     *    ' esdlim_ = -9999, pdecp_ = .true., plzero_ = .true.')
         f1 = pnumb_(' ', -.01, 1.)
         f1 = pnumb_(' ', -.1, 10.)
         f1 = pnumb_(' ',-1.,100.)
         f1 = pnumb_(' ',1.,100.)
C
C
         pfold_ = 25
         f1 = ptext_('_folded_text',
     *    '1234567890 1234567890 1234567890 1234567890 1234567890')
         f1 = ptext_('_folded_text',
     *    '1234567890 1234567890 1234567890 1234567890 1234567890'
     *    //slash)
         f1 = ptext_('_folded_text',
     *    slash//
     *    '2345678901234567890123456789012345678901234567890')
         f1 = ptext_('_folded_text',
     *    '12345678901234567890123456789012345678901234567890'
     *    //slash)
C
C
         pfold_ = 80
         f1 = pchar_('_bracket_test',char(0))
         f1 = pdelim_('{',.false.,0)
         f1 = pchar_(char(0),'first element')
         f1 = pchar_(char(0),'second element')
         f1 = pdelim_('}',.false.,0)

500      call close_
         stop
         end
C
C
