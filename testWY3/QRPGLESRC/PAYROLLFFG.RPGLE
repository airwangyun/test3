000100140416       // ***********************************************************
000200180116       //   PROGRAM NAME - Payroll  wang yun change
000300140416       //   DESCRIPTION - Time reporting master file maintenance using
000400140416       //                 externally described workstation processing.
000500140416       // ***********************************************************
000600140416       //   INDICATORS USED
000700140416       //   50 - No record found on CHAIN operation
000800140416       //   60 - General error condition
000900140416       //   90 - Protect display on delete request
001000140416       //   KC - End of job requested
001100140416       //   KD - Return to application selection
001200140416       //   KE - Return to employee selection
001300140416       //   KF - Return to project selection
001400140416       //   KG - Return to reason code selection
001500140416       //   LR - Last record
001600140416       // ***********************************************************
001700140416       //   SUBROUTINES USED
001800140416       //   EDITSL - Edit application selection display (SELECT)
001900140416       //   ACDESR - Edit action code for all maintenance requests
002000140416       // ***********************************************************
002100140416       //   This program uses all externally described files.  Files
002200140416       //   used are - MSTDSP  - maintenance display file
002300140416       //            - EMPMST  - employee master file
002400140416       //            - PRJMST  - project master file
002500140416       //            - RSNMST  - reason code master file
002600140416       // ***********************************************************
002700140416       Dcl-F MSTDSP     WORKSTN;
002800140416       Dcl-F EMPMST     Usage(*Update:*Delete:*Output) Keyed;
002900140416       Dcl-F PRJMST     Usage(*Update:*Delete:*Output) Keyed;
003000140416       Dcl-F RSNMST     Usage(*Update:*Delete:*Output) Keyed;
003100140416       //
003200140416       //   Compile time array containing error descriptions.
003300140416       Dcl-S ERR             Char(50)        DIM(10) CTDATA PERRCD(1);
003400140416       Dcl-S EMESS           Char(50);
003500140416       //
003600140416       // *****************************************************
003700140416       //   MAINLINE CALCULATIONS
003800140416       // *****************************************************
003900140416       //   This mainline routine controls the display file processing and
004000140416       //   editting.  Using the function keys described on each display
004100140416       //   format, you can transfer from one maintenance application to
004200140416       //   another.  The action code you select on the selection formats
004300140416       //   determines if the program will add a new record to the file or
004400140416       //   update an existing record in the file.
004500140416       // *****************************************************
004600140416       //   Housekeeping, clear display fields and reset indicators.
004700140416       //
004800140416       Exsr MAIN;
004900140416       //  If MAIN is done program ends
005000140416       *INLR = *on;
005100140416       //  MAIN SUBROUTINE
005200140416       BegSr MAIN;
005300140416         DoU *INKC;
005400140416           *IN60  = *OFF;
005500140416           EMESS  = *BLANK;
005600140416           EMPAPL = *BLANK;
005700140416           PRJAPL = *BLANK;
005800140416           RSNAPL = *BLANK;
005900140416       //
006000140416       //   Write the SELECT format to display.  If end of job requested,
006100140416       //
006200140416       //
006300140416           DoU not *IN60;
006400140416             Exfmt SELECT;
006500140416             If *INKC = '1';
006600140416               LeaveSr;
006700140416             Else;
006800140416               Exsr EDITSL;
006900140416             EndIf;
007000140416       //
007100140416       //   IF the general error indicator *IN60 is on (equal to 1), the
007200140416       //   program continues to loop
007300140416       //
007400140416           EndDo;
007500140416       //
007600140416       //   The application selection fields from the SELECT format are
007700140416       //   tested and the program will branch to the section specific to
007800140416       //   that application.
007900140416       //   If EMPAPL (employee maintenance) equals X, the program
008000140416       //   branches to label EMPTAG.
008100140416       //   If PRJAPL (project maintenance) equals X, the program
008200140416       //   branches to label PRJTAG.
008300140416       //   If the prior two tests were not successful, you have chosen
008400140416       //   reason code maintenance.  The program will continue with the
008500140416       //   next executable operation.
008600140416       //
008700140416           Select;
008800140416             When EMPAPL = 'X';
008900140416               Exsr EMPTAG;
009000140416       //
009100140416             When PRJAPL = 'X';
009200140416               Exsr PRJTAG;
009300140416       //
009400140416             When RSNAPL = 'X';
009500140416               Exsr RSNTAG;
009600140416           EndSl;
009700140416       //  if KC end program
009800140416           If *INKC;
009900140416             Leave;
010000140416           EndIf;
010100140416         EndDo;
010200140416       EndSr;
010300140416       //
010400140416       // *********************************************************
010500140416       //   Reason Code Maintenance.
010600140416       // *********************************************************
010700140416       //
010800140416       //   Housekeeping, clear display fields and reset indicators.
010900140416       //
011000140416       BegSr RSNTAG;
011100140416         DoU *INKC;
011200140416           EMESS = *Blanks;
011300140416           RSCDE = *Blanks;
011400140416           ACODE = *Blanks;
011500140416       //  start of error loop
011600140416           DoU not *IN60;
011700140416             RSDSC = *Blanks;
011800140416       //
011900140416       //   Display reason code selection format
012000140416       //
012100140416             Exfmt RSNSEL;
012200140416             If *INKD;
012300140416               LeaveSr;
012400140416             EndIf;
012500140416       //
012600140416       //   Access reason code master to validate action code request
012700140416       //
012800140416             If not *INKC;
012900140416               Chain RSCDE RSNMST;
012901140416               *IN50 = not %Found;
013000140416               Exsr ACDESR;
013100140416             Else;
013200140416               LeaveSr;
013300140416             EndIf;
013400140416       //  end of error loop
013500140416           EndDo;
013600140416       //
013700140416       //   Display reason code maintenance format
013800140416       //
013900140416           Exfmt RSNMNT;
014000140416       //
014100140416           If *INKD;
014200140416             LeaveSr;
014300140416           EndIf;
014400140416           If *INKG;
014500140416             Iter;
014600140416           EndIf;
014700140416       //
014800140416       //   Determine update mode and perform record add or update
014900140416       //
015000140416           If not *inkc;
015100140416       //
015200140416             Select;
015300140416               When ACODE = 'A' and *IN50;
015400140416                 ACREC = 'A';
015500140416                 Write RCRSN;
015600140416       //
015700140416               When ACODE = 'A' and not *IN50 and ACREC = 'D';
015800140416                 ACREC = 'A';
015900140416                 Update RCRSN;
016000140416       //
016100140416               When ACODE = 'D';
016200140416                 ACREC = 'D';
016300140416                 Update RCRSN;
016400140416       //
016500140416               When ACODE = 'C';
016600140416                 Update RCRSN;
016700140416             EndSl;
016800140416           Else;
016900140416             LeaveSr;
017000140416           EndIf;
017100140416       //
017200140416       //   Your maintenance request has now been completed and the
017300140416       //   program branches back to the RSNTAG TAG.
017400140416       //
017500140416         EndDo;
017600140416       EndSr;
017700140416       // *********************************************************
017800140416       //   Employee master maintenance routine.
017900140416       // *********************************************************
018000140416       //
018100140416       //   Housekeeping, clear display fields and reset indicators.
018200140416       //
018300140416       BegSr EMPTAG;
018400140416         DoU *INKC;
018500140416           *IN60 = '0';
018600140416           EMESS = *Blanks;
018700140416           EMPNO = 0;
018800140416           ACODE = *Blanks;
018900140416       //  error loop start
019000140416           DoU not *IN60;
019100140416             ENAME = *Blanks;
019200140416             EMCAT = *Blanks;
019300140416             EDEPT = *Blanks;
019400140416             ELOCN = *Blanks;
019500140416             EUSRI = *Blanks;
019600140416             ENHRS = 0;
019700140416       //
019800140416       //   Display employee selection format
019900140416       //
020000140416             Exfmt EMPSEL;
020100140416       //  leave subroutine
020200140416       //
020300140416             If *INKD;
020400140416               LeaveSr;
020500140416             EndIf;
020600140416       //
020700140416       //   Access employee master to validate action code request
020800140416       //
020900140416             If NOT *INKC;
021000140416               Chain EMPNO EMPMST;
021001140416               *IN50 = not %Found;
021100140416               Exsr ACDESR;
021200140416             Else;
021300140416               LeaveSr;
021400140416             EndIf;
021500140416       //  end of error loop
021600140416           EndDo;
021700140416       //
021800140416       //   Display employee maintenance format
021900140416       //
022000140416           Exfmt EMPMNT;
022100140416           If *INKD;
022200140416       // end of subroutine
022300140416             LeaveSr;
022400140416           EndIf;
022500140416           If *INKE;
022600140416       //  End of one loop
022700140416             Iter;
022800140416           EndIf;
022900140416       //
023000140416       //   Determine update mode and perform record add or update
023100140416       //
023200140416           If *INKC = *OFF;
023300140416             Select;
023400140416               When ACODE ='A'  and *IN50;
023500140416                 ACREC = 'A';
023600140416                 Write RCEMP;
023700140416       //
023800140416               When ACODE = 'A' and not *IN50 and ACREC = 'D';
023900140416                 ACREC = 'A';
024000140416                 Update RCEMP;
024100140416       //
024200140416               When ACODE = 'D';
024300140416                 ACREC = 'D';
024400140416                 Update RCEMP;
024500140416       //
024600140416               When ACODE = 'C';
024700140416                 Update RCEMP;
024800140416             EndSl;
024900140416           Else;
025000140416             LeaveSr;
025100140416           EndIf;
025200140416       //
025300140416       //   Your maintenance request has now been completed and the
025400140416       //   program branches back to the EMPTAG TAG.
025500140416       //
025600140416         EndDo;
025700140416       EndSr;
025800140416       // *********************************************************
025900140416       //   Project master maintenance routine.
026000140416       // *********************************************************
026100140416       //
026200140416       //   Housekeeping, clear display fields and reset indicators.
026300140416       //
026400140416       BegSr PRJTAG;
026500140416         DoU *INKC;
026600140416           *IN60 = '0';
026700140416           EMESS = *Blanks;
026800140416           PRCDE = *Blanks;
026900140416           ACODE = *Blanks;
027000140416           DoU not *IN60;
027100140416             PRDSC = *Blanks;
027200140416             PRRSP = *Blanks;
027300140416             PRSTR = 0;
027400140416             PREND = 0;
027500140416             PRCMP = 0;
027600140416             PREST = 0;
027700140416       //
027800140416       //   Display project selection format
027900140416       //
028000140416             Exfmt PRJSEL;
028100140416             If *INKD;
028200140416               LeaveSr;
028300140416             EndIf;
028400140416       //
028500140416       //   Access project master to validate action code request
028600140416       //
028700140416             If not *INKC;
028800140416               Chain PRCDE PRJMST;
028801140416               *IN50 = not %Found;
028900140416               Exsr ACDESR;
029000140416             Else;
029100140416               LeaveSr;
029200140416             EndIf;
029300140416           EndDo;
029400140416       //
029500140416       //   Display project maintenance format
029600140416       //
029700140416           Exfmt PRJMNT;
029800140416       //  leave subroutine
029900140416           If *INKD;
030000140416             LeaveSr;
030100140416           EndIf;
030200140416       //  end of loop
030300140416           If *INKF;
030400140416             Iter;
030500140416           EndIf;
030600140416       //
030700140416       //   Determine update mode and perform record add or update
030800140416       //
030900140416           If *INKC = '0';
031000140416             Select;
031100140416               When  ACODE = 'A'  and *IN50;
031200140416                 ACREC = 'A';
031300140416                 Write RCPRJ;
031400140416       //
031500140416               When ACODE = 'A' and *IN50 and ACREC = 'D';
031600140416                 ACREC = 'A';
031700140416                 Update RCPRJ;
031800140416       //  Delete OP
031900140416               When ACODE = 'D';
032000140416                 ACREC = 'D';
032100140416                 Update RCPRJ;
032200140416       //  Change OP
032300140416               When ACODE = 'C';
032400140416                 Update RCPRJ;
032500140416             EndSl;
032600140416           Else;
032700140416             LeaveSr;
032800140416           EndIf;
032900140416         EndDo;
033000140416       //
033100140416       //   Your maintenance request has now been completed and the
033200140416       //   program branches back to the PRJTAG TAG.
033300140416       //
033400140416       //                   GOTO      PRJTAG
033500140416       EndSr;
033600140416       // *********************************************************
033700140416       //   End of job requested.  Control is passed to here when you press
033800140416       //   F3 (*INKC).  The last record indicator *INLR is set on and the
033900140416       //   program ends.
034000140416       //
034100140416       //                   ENDSR
034200140416       //
034300140416       // *******************************************************
034400140416       //   EDITSL subroutine verifies the time reoprting application
034500140416       //   selection display input.
034600140416       // *******************************************************
034700140416       BegSr EDITSL;
034800140416       //
034900140416       //   Housekeeping, clear display fields and reset indicators.
035000140416       //
035100140416         EMESS = *Blanks;
035200140416         *IN60 = *OFF;
035300140416       //
035400140416       //   The following IF AND OR combination checks the application
035500140416       //   selection fields to ensure that only one application has been
035600140416       //   selected.
035700140416       //
035800140416         If EMPAPL = 'X'
035900140416           and PRJAPL = 'X'
036000140416           or EMPAPL = 'X'
036100140416           and RSNAPL = 'X';
036200140416       //     THE BUG IS HERE
036300140416       //     ***************
036400140416           *IN60 = '1';
036500140416           EMESS = ERR(2);
036600140416         Else;
036700140416           *IN60 = '0';
036800140416         EndIf;
036900140416       //
037000140416       //   The following IF AND combination ensures that at least one
037100140416       //   application has been selected.
037200140416       //
037300140416         If EMPAPL = ' '
037400140416           and PRJAPL = ' '
037500140416           and RSNAPL = ' ';
037600140416           *IN60 = '1';
037700140416           EMESS = ERR(3);
037800140416         EndIf;
037900140416       //
038000140416       //   The following code checks each application selection field to
038100140416       //   ensure that it is either ' ' (blank) or equal to 'X'.
038200140416       //
038300140416         If EMPAPL <> ' '
038400140416           and EMPAPL <> 'X';
038500140416           *IN60 = '1';
038600140416           EMESS = ERR(1);
038700140416         EndIf;
038800140416         If PRJAPL <> ' '
038900140416           and PRJAPL <> 'X';
039000140416           *IN60 = '1';
039100140416           EMESS = ERR(1);
039200140416         EndIf;
039300140416         If RSNAPL <> ' '
039400140416           and RSNAPL <> 'X';
039500140416           *IN60 = '1';
039600140416           EMESS = ERR(1);
039700140416         EndIf;
039800140416       EndSr;
039900140416       //
040000140416       // *******************************************************
040100140416       //   ACDESR subroutine verifies the time reporting action codes for
040200140416       //   all maintenance selections.
040300140416       // *******************************************************
040400140416       BegSr ACDESR;
040500140416       //
040600140416       //   Housekeeping, clear display fields and reset indicators.
040700140416       //
040800140416         EMESS = *BLANKS;
040900140416         *IN60 = *OFF;
041000140416         *IN90 = *OFF;
041100140416       //
041200140416       //   The following  statements perform
041300140416       //   two functions.  First they determine the type of maintenance
041400140416       //   requested and branche to the appropriate subroutine and secondly
041500140416       //   they determine if the maintenance code entered is invalid.
041600140416       //
041700140416         Select;
041800140416           When ACODE = 'A';
041900140416             Exsr ADDCDE;
042000140416           When ACODE = 'C';
042100140416             Exsr CHGCDE;
042200140416           When ACODE = 'D';
042300140416             Exsr DELCDE;
042400140416           Other;
042500140416             Exsr INVCDE;
042600140416         EndSl;
042700140416       EndSr;
042800140416       //
042900140416       //   The following code verifies the add request.
043000140416       //
043100140416       BegSr ADDCDE;
043200140416         If NOT *IN50 AND ACREC = 'A';
043300140416           *IN60 = *ON;
043400140416           EMESS = ERR(5);
043500140416         Else;
043600140416           If NOT *IN50 AND ACREC = 'D';
043700140416             EMESS = ERR(6);
043800140416           EndIf;
043900140416         EndIf;
044000140416       EndSr;
044100140416       //
044200140416       //   The following code verifies the change request.
044300140416       //
044400140416       BegSr CHGCDE;
044500140416         If *IN50;
044600140416           *IN60 = *ON;
044700140416           EMESS = ERR(7);
044800140416         Else;
044900140416           If NOT *IN50 AND ACREC = 'D';
045000140416             *IN60 = *ON;
045100140416             EMESS = ERR(8);
045200140416           EndIf;
045300140416         EndIf;
045400140416       EndSr;
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
048100140416       //
048200140416       //   The compile time array ERR is entered below.  The array is
048300140416       //   precceded by "** " to denote the beginning of the array.
048400140416       //
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
