000100000000     F************************************************************
000200030715     F*  PROGRAM NAME - Payroll
000300000000     F*  DESCRIPTION - Time reporting master file maintenance using
000400000000     F*                externally described workstation processing.
000500000000     F************************************************************
000600000000     F*  INDICATORS USED
000700060222     F*  50 - No record found on CHAIN operation
000800000000     F*  60 - General error condition
000900000000     F*  90 - Protect display on delete request
001000000000     F*  KC - End of job requested
001100000000     F*  KD - Return to application selection
001200000000     F*  KE - Return to employee selection
001300000000     F*  KF - Return to project selection
001400000000     F*  KG - Return to reason code selection
001500000000     F*  LR - Last record
001600000000     F************************************************************
001700000000     F*  SUBROUTINES USED
001800000000     F*  EDITSL - Edit application selection display (SELECT)
001900000000     F*  ACDESR - Edit action code for all maintenance requests
002000000000     F************************************************************
002100000000     F*  This program uses all externally described files.  Files
002200000000     F*  used are - MSTDSP  - maintenance display file
002300000000     F*           - EMPMST  - employee master file
002400000000     F*           - PRJMST  - project master file
002500000000     F*           - RSNMST  - reason code master file
002600000000     F************************************************************
002700000000     FMSTDSP    CF   E             WORKSTN
002800000000     FEMPMST    UF A E           K DISK
002900000000     FPRJMST    UF A E           K DISK
003000000000     FRSNMST    UF A E           K DISK
003100000000     D*
003200000000     D*  Compile time array containing error descriptions.
003300000000     D ERR             S             50    DIM(10) CTDATA PERRCD(1)
003400030715     D EMESS           S             50
003500000000     E*
003600000000     C******************************************************
003700000000     C*  MAINLINE CALCULATIONS
003800000000     C******************************************************
003900000000     C*  This mainline routine controls the display file processing and
004000000000     C*  editting.  Using the function keys described on each display
004100000000     C*  format, you can transfer from one maintenance application to
004200000000     C*  another.  The action code you select on the selection formats
004300000000     C*  determines if the program will add a new record to the file or
004400000000     C*  update an existing record in the file.
004500000000     C******************************************************
004600000000     C*  Housekeeping, clear display fields and reset indicators.
004700000000     C*
004800000000     C                   EXSR      MAIN
004900030715     C* If MAIN is done program ends
005000030713     C                   eval      *INLR = *on
005100000000      * MAIN SUBROUTINE
005200000000     C     MAIN          BEGSR
005300030715     C                   dou       *INKC
005400030715     C                   EVAL      *IN60  = *OFF
005500030713     C                   EVAL      EMESS  = *BLANK
005600030713     C                   EVAL      EMPAPL = *BLANK
005700030713     C                   EVAL      PRJAPL = *BLANK
005800030713     C                   EVAL      RSNAPL = *BLANK
005900000000     C*
006000000000     C*  Write the SELECT format to display.  If end of job requested,
006100030715     C*
006200000000     C*
006300030715     C                   DOU       not *IN60
006400000000     C                   EXFMT     SELECT
006500000000     C     *INKC         IFEQ      '1'
006600030713     C                   LEAVESR
006700000000     C                   ELSE
006800000000     C                   EXSR      EDITSL
006900000000     C                   END
007000000000     C*
007100000000     C*  IF the general error indicator *IN60 is on (equal to 1), the
007200030715     C*  program continues to loop
007300000000     C*
007400030713     C                   ENDdo
007500000000     C*
007600000000     C*  The application selection fields from the SELECT format are
007700000000     C*  tested and the program will branch to the section specific to
007800000000     C*  that application.
007900000000     C*  If EMPAPL (employee maintenance) equals X, the program
008000000000     C*  branches to label EMPTAG.
008100000000     C*  If PRJAPL (project maintenance) equals X, the program
008200000000     C*  branches to label PRJTAG.
008300000000     C*  If the prior two tests were not successful, you have chosen
008400000000     C*  reason code maintenance.  The program will continue with the
008500000000     C*  next executable operation.
008600000000     C*
008700030714     C                   SELECT
008800030714     C                   when      EMPAPL = 'X'
008900030713     C                   EXSR      EMPTAG
009000030715     C*
009100030714     C                   when      PRJAPL = 'X'
009200030714     C                   EXSR      PRJTAG
009300030715     C*
009400030714     C                   when      RSNAPL = 'X'
009500030714     C                   EXSR      RSNTAG
009600030714     C                   ENDSL
009700030715     C* if KC end program
009800030714     C                   if        *INKC
009900030714     C                   leave
010000030714     C                   endif
010100030714     C                   enddo
010200030714     C                   endsr
010300000000     C*
010400000000     C**********************************************************
010500000000     C*  Reason Code Maintenance.
010600000000     C**********************************************************
010700000000     C*
010800000000     C*  Housekeeping, clear display fields and reset indicators.
010900000000     C*
011000030714     C     RSNTAG        BEGSR
011100030714     C                   DOU       *INKC
011200000000     C                   MOVE      *BLANKS       EMESS
011300000000     C                   MOVE      *BLANKS       RSCDE
011400000000     C                   MOVE      *BLANKS       ACODE
011500030715     C* start of error loop
011600030714     C                   DOU       not *IN60
011700000000     C                   MOVE      *BLANKS       RSDSC
011800000000     C*
011900000000     C*  Display reason code selection format
012000000000     C*
012100000000     C                   EXFMT     RSNSEL
012200030714     C                   IF        *INKD
012300030715     C                   LEAVeSR
012400000000     C                   END
012500000000     C*
012600000000     C*  Access reason code master to validate action code request
012700000000     C*
012800030714     C                   IF        not *INKC
012900000000     C     RSCDE         CHAIN     RSNMST                             50
013000000000     C                   EXSR      ACDESR
013100000000     C                   ELSE
013200030713     C                   LEAVESR
013300000000     C                   END
013400030715     C* end of error loop
013500030714     C                   ENDDO
013600000000     C*
013700000000     C*  Display reason code maintenance format
013800000000     C*
013900000000     C                   EXFMT     RSNMNT
014000030715     C*
014100030714     C                   IF        *INKD
014200030714     C                   LEAVESR
014300000000     C                   END
014400030714     C                   IF        *INKG
014500030714     C                   ITER
014600000000     C                   END
014700000000     C*
014800000000     C*  Determine update mode and perform record add or update
014900000000     C*
015000030714     C                   IF        not *inkc
015100030714     C
015200030715     C                   Select
015300030715     C                   WHEN      ACODE = 'A' and *IN50
015400000000     C                   MOVE      'A'           ACREC
015500000000     C                   WRITE     RCRSN
015600030715     C*
015700030715     C                   WHEN      ACODE = 'A' and not *IN50 and ACREC = 'D'
015800000000     C                   MOVE      'A'           ACREC
015900000000     C                   UPDATE    RCRSN
016000030715     C*
016100030714     C                   When      ACODE = 'D'
016200000000     C                   MOVE      'D'           ACREC
016300000000     C                   UPDATE    RCRSN
016400030715     C*
016500030714     C                   WHEN      ACODE = 'C'
016600000000     C                   UPDATE    RCRSN
016700030714     C                   ENDSL
016800000000     C                   ELSE
016900030713     C                   LEAVESR
017000000000     C                   END
017100000000     C*
017200000000     C*  Your maintenance request has now been completed and the
017300000000     C*  program branches back to the RSNTAG TAG.
017400000000     C*
017500030714     C                   ENDDO
017600030714     C                   ENDsr
017700000000     C**********************************************************
017800000000     C*  Employee master maintenance routine.
017900000000     C**********************************************************
018000000000     C*
018100000000     C*  Housekeeping, clear display fields and reset indicators.
018200000000     C*
018300030713     C     EMPTAG        BEGSR
018400030714     C                   dou       *INKC
018500000000     C                   MOVE      '0'           *IN60
018600000000     C                   MOVE      *BLANKS       EMESS
018700000000     C                   Z-ADD     0             EMPNO
018800000000     C                   MOVE      *BLANKS       ACODE
018900030715     C* error loop start
019000030714     C                   dou       not *IN60
019100000000     C                   MOVE      *BLANKS       ENAME
019200000000     C                   MOVE      *BLANKS       EMCAT
019300000000     C                   MOVE      *BLANKS       EDEPT
019400000000     C                   MOVE      *BLANKS       ELOCN
019500000000     C                   MOVE      *BLANKS       EUSRI
019600000000     C                   Z-ADD     0             ENHRS
019700000000     C*
019800000000     C*  Display employee selection format
019900000000     C*
020000000000     C                   EXFMT     EMPSEL
020100030715     C* leave subroutine
020200030713     C*
020300030713     C                   IF        *INKD
020400030714     C                   leavesr
020500000000     C                   END
020600000000     C*
020700000000     C*  Access employee master to validate action code request
020800000000     C*
020900030713     C                   IF        NOT *INKC
021000000000     C     EMPNO         CHAIN     EMPMST                             50
021100000000     C                   EXSR      ACDESR
021200000000     C                   ELSE
021300030713     C                   LEAVESR
021400000000     C                   END
021500030715     C* end of error loop
021600030714     C                   ENDdo
021700000000     C*
021800000000     C*  Display employee maintenance format
021900000000     C*
022000000000     C                   EXFMT     EMPMNT
022100030713     C                   IF        *INKD
022200030715     C*end of subroutine
022300030713     C                   LEAVESR
022400000000     C                   END
022500030713     C                   IF        *INKE
022600030715     C* End of one loop
022700030714     C                   iter
022800000000     C                   END
022900000000     C*
023000000000     C*  Determine update mode and perform record add or update
023100000000     C*
023200030713     C                   IF        *INKC = *OFF
023300030714     C                   select
023400030714     C                   when      ACODE ='A'  and *IN50
023500000000     C                   MOVE      'A'           ACREC
023600000000     C                   WRITE     RCEMP
023700030715     C*
023800030714     C                   when      ACODE = 'A' and not *IN50 and ACREC = 'D'
023900000000     C                   MOVE      'A'           ACREC
024000000000     C                   UPDATE    RCEMP
024100030715     C*
024200030714     C                   when      ACODE = 'D'
024300000000     C                   MOVE      'D'           ACREC
024400000000     C                   UPDATE    RCEMP
024500030715     C*
024600030714     C                   when      ACODE = 'C'
024700000000     C                   UPDATE    RCEMP
024800030714     C                   ENDsl
024900000000     C                   ELSE
025000030713     C                   LEAVESR
025100000000     C                   END
025200000000     C*
025300000000     C*  Your maintenance request has now been completed and the
025400000000     C*  program branches back to the EMPTAG TAG.
025500000000     C*
025600030714     C                   enddo
025700030713     C                   ENDSR
025800000000     C**********************************************************
025900000000     C*  Project master maintenance routine.
026000000000     C**********************************************************
026100000000     C*
026200000000     C*  Housekeeping, clear display fields and reset indicators.
026300000000     C*
026400030715     C     PRJTAG        begsr
026500030714     C                   DOU       *INKC
026600000000     C                   MOVE      '0'           *IN60
026700000000     C                   MOVE      *BLANKS       EMESS
026800000000     C                   MOVE      *BLANKS       PRCDE
026900000000     C                   MOVE      *BLANKS       ACODE
027000030714     C                   DOU       not *IN60
027100000000     C                   MOVE      *BLANKS       PRDSC
027200000000     C                   MOVE      *BLANKS       PRRSP
027300000000     C                   Z-ADD     0             PRSTR
027400000000     C                   Z-ADD     0             PREND
027500000000     C                   Z-ADD     0             PRCMP
027600000000     C                   Z-ADD     0             PREST
027700000000     C*
027800000000     C*  Display project selection format
027900000000     C*
028000000000     C                   EXFMT     PRJSEL
028100030714     C                   IF        *INKD
028200030714     C                   leavesr
028300000000     C                   END
028400000000     C*
028500000000     C*  Access project master to validate action code request
028600000000     C*
028700030714     C                   IF        not *INKC
028800000000     C     PRCDE         CHAIN     PRJMST                             50
028900000000     C                   EXSR      ACDESR
029000000000     C                   ELSE
029100030713     C                   LEAVESR
029200000000     C                   END
029300030714     C                   ENDdo
029400000000     C*
029500000000     C*  Display project maintenance format
029600000000     C*
029700000000     C                   EXFMT     PRJMNT
029800030715     C* leave subroutine
029900030714     C                   IF        *INKD
030000030714     C                   LEAVESR
030100000000     C                   END
030200030715     C* end of loop
030300030714     C                   IF        *INKF
030400030714     C                   ITER
030500000000     C                   END
030600000000     C*
030700000000     C*  Determine update mode and perform record add or update
030800000000     C*
030900000000     C     *INKC         IFEQ      '0'
031000030714     C                   SELECT
031100030714     C                   WHEN       ACODE = 'A'  and *IN50
031200000000     C                   MOVE      'A'           ACREC
031300000000     C                   WRITE     RCPRJ
031400030715     C*
031500030714     C                   WHEN      ACODE = 'A' and *IN50 and ACREC = 'D'
031600000000     C                   MOVE      'A'           ACREC
031700000000     C                   UPDATE    RCPRJ
031800030715     C* Delete OP
031900030714     C                   WHEN      ACODE = 'D'
032000000000     C                   MOVE      'D'           ACREC
032100000000     C                   UPDATE    RCPRJ
032200030715     C* Change OP
032300030714     C                   WHEN      ACODE = 'C'
032400000000     C                   UPDATE    RCPRJ
032500030714     C                   ENDSL
032600000000     C                   ELSE
032700030713     C                   LEAVESR
032800000000     C                   END
032900030714     C                   ENDDO
033000000000     C*
033100000000     C*  Your maintenance request has now been completed and the
033200000000     C*  program branches back to the PRJTAG TAG.
033300000000     C*
033400030714     C*                  GOTO      PRJTAG
033500030714     C                   endsr
033600000000     C**********************************************************
033700000000     C*  End of job requested.  Control is passed to here when you press
033800000000     C*  F3 (*INKC).  The last record indicator *INLR is set on and the
033900000000     C*  program ends.
034000000000     C*
034100030714     C*                  ENDSR
034200000000     C*
034300000000     C********************************************************
034400000000     C*  EDITSL subroutine verifies the time reoprting application
034500000000     C*  selection display input.
034600000000     C********************************************************
034700000000     C     EDITSL        BEGSR
034800000000     C*
034900000000     C*  Housekeeping, clear display fields and reset indicators.
035000000000     C*
035100030715     C                   MOVE      *BLANKS       EMESS
035200030715     C                   EVAL      *IN60 = *OFF
035300000000     C*
035400000000     C*  The following IF AND OR combination checks the application
035500000000     C*  selection fields to ensure that only one application has been
035600000000     C*  selected.
035700000000     C*
035800000000     C     EMPAPL        IFEQ      'X'
035900000000     C     PRJAPL        ANDEQ     'X'
036000000000     C     EMPAPL        OREQ      'X'
036100000000     C     RSNAPL        ANDEQ     'X'
036200000000     C*    THE BUG IS HERE
036300000000     C*    ***************
036400000000     C                   MOVE      '1'           *IN60
036500000000     C                   MOVE      ERR(2)        EMESS
036600000000     C                   ELSE
036700000000     C                   MOVE      '0'           *IN60
036800000000     C                   END
036900000000     C*
037000000000     C*  The following IF AND combination ensures that at least one
037100000000     C*  application has been selected.
037200000000     C*
037300000000     C     EMPAPL        IFEQ      ' '
037400000000     C     PRJAPL        ANDEQ     ' '
037500000000     C     RSNAPL        ANDEQ     ' '
037600000000     C                   MOVE      '1'           *IN60
037700000000     C                   MOVE      ERR(3)        EMESS
037800000000     C                   END
037900000000     C*
038000000000     C*  The following code checks each application selection field to
038100000000     C*  ensure that it is either ' ' (blank) or equal to 'X'.
038200000000     C*
038300030715     C     EMPAPL        IFNE      ' '
038400000000     C     EMPAPL        ANDNE     'X'
038500000000     C                   MOVE      '1'           *IN60
038600000000     C                   MOVE      ERR(1)        EMESS
038700000000     C                   END
038800000000     C     PRJAPL        IFNE      ' '
038900000000     C     PRJAPL        ANDNE     'X'
039000000000     C                   MOVE      '1'           *IN60
039100000000     C                   MOVE      ERR(1)        EMESS
039200000000     C                   END
039300000000     C     RSNAPL        IFNE      ' '
039400000000     C     RSNAPL        ANDNE     'X'
039500000000     C                   MOVE      '1'           *IN60
039600000000     C                   MOVE      ERR(1)        EMESS
039700000000     C                   END
039800000000     C                   ENDSR
039900000000     C*
040000000000     C********************************************************
040100000000     C*  ACDESR subroutine verifies the time reporting action codes for
040200000000     C*  all maintenance selections.
040300000000     C********************************************************
040400000000     C     ACDESR        BEGSR
040500000000     C*
040600000000     C*  Housekeeping, clear display fields and reset indicators.
040700000000     C*
040800030715     C                   EVAL      EMESS = *BLANKS
040900030715     C                   EVAL      *IN60 = *OFF
041000030715     C                   EVAL      *IN90 = *OFF
041100000000     C*
041200030715     C*  The following  statements perform
041300030715     C*  two functions.  First they determine the type of maintenance
041400030715     C*  requested and branche to the appropriate subroutine and secondly
041500030715     C*  they determine if the maintenance code entered is invalid.
041600000000     C*
041700030715     C                   SELECT
041800030715     C                   WHEN      ACODE = 'A'
041900030715     C                   EXSR      ADDCDE
042000030715     C                   WHEN      ACODE = 'C'
042100030715     C                   EXSR      CHGCDE
042200030715     C                   WHEN      ACODE = 'D'
042300030715     C                   EXSR      DELCDE
042400030715     C                   OTHER
042500030715     C                   EXSR      INVCDE
042600030715     C                   ENDSL
042700030715     C                   ENDSR
042800000000     C*
042900000000     C*  The following code verifies the add request.
043000000000     C*
043100000000     C     ADDCDE        BEGSR
043200030715     C                   IF        NOT *IN50 AND ACREC = 'A'
043300030715     C                   EVAL      *IN60 = *ON
043400000000     C                   MOVE      ERR(5)        EMESS
043500000000     C                   ELSE
043600030715     C                   IF        NOT *IN50 AND ACREC = 'D'
043700000000     C                   MOVE      ERR(6)        EMESS
043800030715     C                   ENDIF
043900030715     C                   ENDIF
044000000000     C                   ENDSR
044100000000     C*
044200000000     C*  The following code verifies the change request.
044300000000     C*
044400000000     C     CHGCDE        BEGSR
044500030715     C                   IF        *IN50
044600030715     C                   EVAL      *IN60 = *ON
044700000000     C                   MOVE      ERR(7)        EMESS
044800000000     C                   ELSE
044900030715     C                   IF        NOT *IN50 AND ACREC = 'D'
045000030715     C                   EVAL      *IN60 = *ON
045100000000     C                   MOVE      ERR(8)        EMESS
045200030715     C                   ENDIF
045300030715     C                   ENDIF
045400000000     C                   ENDSR
045500030715      /FREE
045600030715       //*
045700030715       //*  The following code verifies the delete request.  The field
045800030715       //*  protect indicator *IN90 is first set on (equal to 1) to not
045900030715       //*  allow changes to existing data on a delete request.
046000030715       //*
046100030715       BEGSR DELCDE;
046200030715       *IN90 = *ON;
046300030715       IF *IN50;
046400030715          *IN60 = *ON;
046500030715          EMESS =  ERR(9);
046600030715       ELSE;
046700030715          IF NOT  *IN50 AND ACREC = 'D';
046800030715             *IN60 = *ON;
046900030715             EMESS = ERR(10);
047000030715          ENDIF;
047100030715       ENDIF;
047200030715       ENDSR;
047300030715       //*
047400030715       //*  Invalid action code
047500030715       //*
047600030715       BEGSR INVCDE;
047700030715       *IN60 = *ON;
047800030715       EMESS = ERR(4);
047900030715       ENDSR;
048000030715      /END-FREE
048100000000     O*
048200000000     O*  The compile time array ERR is entered below.  The array is
048300000000     O*  precceded by "** " to denote the beginning of the array.
048400000000     O*
048500000000**  Array ERR - Error descriptions
048600000000   MAINTENANCE SELECTION CODE NOT EQUAL TO "X"
048700000000MORE THAN ONE APPLICATION SELECTED FOR MAINTENANCE
048800000000     NO APPLICATION SELECTED FOR MAINTENANCE
048900000000    ACTION CODE NOT EQUAL TO "A", "C" OR "D"
049000000000 ADD REQUESTED BUT RECORD ALREADY EXISTS IN FILE
049100000000    WARNING - RECORD WAS PREVIOUSLY DELETED
049200000000   CHANGE REQUESTED BUT RECORD DOES NOT EXIST
049300000000     CHANGE REQUESTED BUT RECORD IS DELETED
049400000000   DELETE REQUESTED BUT RECORD DOES NOT EXIST
049500000000   DELETE REQUESTED BUT RECORD ALREADY DELETED
