000101160128       ctl-opt Dftactgrp(*no);
000102160128        // ***********************************************************
000103160128        //   PROGRAM NAME - Payroll
000104160128        //   DESCRIPTION - Time reporting master file maintenance using
000105160128        //                 externally described workstation processing.
000106160128        // ***********************************************************
000107160128        //   INDICATORS USED
000108160128        //   50 - No record found on CHAIN operation
000109160128        //   60 - General error condition
000110160128        //   90 - Protect display on delete request
000111160128        //   KC - End of job requested
000112160128        //   KD - Return to application selection
000113160128        //   KE - Return to employee selection
000114160128        //   KF - Return to project selection
000115160128        //   KG - Return to reason code selection
000116160128        //   LR - Last record
000117160128        // ***********************************************************
000118160128        //   SUBROUTINES USED
000119160128        //   ValidateFileToMaintainSelection - Edit application selection displa
000120160128        //   ValidateActionCode - Edit action code for all maintenance requests
000121160128        // ***********************************************************
000122160128        //   This program uses all externally described files.  Files
000123160128        //   used are - MSTDSP  - maintenance display file
000124160128        //            - EMPMST  - employee master file
000125160128        //            - PRJMST  - project master file
000126160128        //            - RSNMST  - reason code master file
000127160128        // ***********************************************************
000128160128        Dcl-F MSTDSP     WORKSTN;
000129160128        Dcl-F EMPMST     Usage(*Update:*Delete:*Output) Keyed;
000130160128        Dcl-F PRJMST     Usage(*Update:*Delete:*Output) Keyed;
000131160128        Dcl-F RSNMST     Usage(*Update:*Delete:*Output) Keyed;
000132160128        //
000133160128        //   Compile time array containing error descriptions.
000134160128        Dcl-S ERR             Char(50)        DIM(10) CTDATA PERRCD(1);
000135160128        Dcl-S EMESS           Char(50);
000136160128
000137160128        Dcl-C INVALID_ACTION_CODE 4;
000138160128        //
000139160128        // *****************************************************
000140160128        //   MAINLINE CALCULATIONS
000141160128        // *****************************************************
000142160128        //   This mainline routine controls the display file processing and
000143160128        //   editting.  Using the function keys described on each display
000144160128        //   format, you can transfer from one maintenance application to
000145160128        //   another.  The action code you select on the selection formats
000146160128        //   determines if the program will add a new record to the file or
000147160128        //   update an existing record in the file.
000148160128        // *****************************************************
000149160128        //   Housekeeping, clear display fields and reset indicators.
000150160128        //
000151160128        CALLP MAIN();
000152160128        //  If MAIN is done program ends
000153160128        *INLR = *on;
000154160128        // *********************************************************
000155160128        //   End of job requested.  Control is passed to here when you press
000156160128        //   F3 (*INKC).  The last record indicator *INLR is set on and the
000157160128        //   program ends.
000158160128        // *******************************************************
000159160128
000160160128
000161160128        //  Main screen - select file to maintain
000162160128        //
000163160128        Dcl-Proc MAIN;
000164160128          DoU *INKC;
000165160128            *IN60  = *OFF;
000166160128            EMESS  = *BLANK;
000167160128            EMPAPL = *BLANK;
000168160128            PRJAPL = *BLANK;
000169160128            RSNAPL = *BLANK;
000170160128            //
000171160128            //   Write the SELECT format to display until end of job requested,
000172160128            //
000173160128            DoU not *IN60;
000174160128              //   IF the general error indicator *IN60 is on (equal to 1), the
000175160128              //   program continues to loop
000176160128              Exfmt SELECT;
000177160128              If *INKC = '1'; // F3 = end of job
000178160128                Return;
000179160128              Else;
000180160128                ValidateFileToMaintainSelection();
000181160128              EndIf;
000182160128            EndDo;
000183160128        //
000184160128        //   The application selection fields from the SELECT format are
000185160128        //   tested and the program will branch to the section specific to
000186160128        //   that application.
000187160128        //   If EMPAPL (employee maintenance) equals X, the program
000188160128        //   branches to  EmployeeMaintenance() proc.
000189160128        //   If PRJAPL (project maintenance) equals X, the program
000190160128        //   branches to ProjectMaintenance() proc
000191160128        //   If the prior two tests were not successful, you have chosen
000192160128        //   reason code maintenance.  The program will continue with the
000193160128        //   next executable operation.
000194160128        //
000195160128            Select;
000196160128              When EMPAPL = 'X';
000197160128                CallP EmployeeMaintenance();
000198160128        //
000199160128              When PRJAPL = 'X';
000200160128                CallP ProjectMaintenance();
000201160128        //
000202160128              When RSNAPL = 'X';
000203160128                ReasonMaintenance();
000204160128            EndSl;
000205160128        //  if KC end program
000206160128            If *INKC;
000207160128              Leave;
000208160128            EndIf;
000209160128          EndDo;
000210160128        End-Proc;
000211160128        //
000212160128        // *********************************************************
000213160128        //   Reason Code Maintenance.
000214160128        // *********************************************************
000215160128        Dcl-Proc ReasonMaintenance;
000216160128          DoU *INKC;
000217160128        //
000218160128        //   Housekeeping, clear display fields and reset indicators.
000219160128        //
000220160128            EMESS = *Blanks;
000221160128            RSCDE = *Blanks;
000222160128            ACODE = *Blanks;
000223160128            //  start of error loop
000224160128            DoU not *IN60;
000225160128              RSDSC = *Blanks;
000226160128        //
000227160128        //   Display reason code selection format
000228160128        //
000229160128              Exfmt RSNSEL;
000230160128              If *INKD;
000231160128                Return;
000232160128              EndIf;
000233160128        //
000234160128        //   Access reason code master to validate action code request
000235160128        //
000236160128              If not *INKC;
000237160128                Chain RSCDE RSNMST;
000238160128                *IN50 = not %Found;
000239160128                CallP ValidateActionCode();
000240160128              Else;
000241160128                Return;
000242160128              EndIf;
000243160128        //  end of error loop
000244160128            EndDo;
000245160128        //
000246160128        //   Display reason code maintenance format
000247160128        //
000248160128            Exfmt RSNMNT;
000249160128        //
000250160128            If *INKD;
000251160128              Return;
000252160128            EndIf;
000253160128            If *INKG;
000254160128              Iter;
000255160128            EndIf;
000256160128        //
000257160128        //   Determine update mode and perform record add or update
000258160128        //
000259160128            If not *inkc;
000260160128        //
000261160128              Select;
000262160128                When ACODE = 'A' and *IN50;
000263160128                  ACREC = 'A';
000264160128                  Write RCRSN;
000265160128        //
000266160128                When ACODE = 'A' and not *IN50 and ACREC = 'D';
000267160128                  ACREC = 'A';
000268160128                  Update RCRSN;
000269160128        //
000270160128                When ACODE = 'D';
000271160128                  ACREC = 'D';
000272160128                  Update RCRSN;
000273160128        //
000274160128                When ACODE = 'C';
000275160128                  Update RCRSN;
000276160128              EndSl;
000277160128            Else;
000278160128              Return;
000279160128            EndIf;
000280160128        //
000281160128        //   Your maintenance request has now been completed and the
000282160128        //   program branches back to the ReasonMaintenance loop
000283160128        //
000284160128          EndDo;
000285160128        End-Proc;
000286160128        // *********************************************************
000287160128        //   Employee master maintenance routine.
000288160128        // *********************************************************
000289160128       Dcl-Proc EmployeeMaintenance;
000290160128          DoU *INKC;
000291160128        //
000292160128        //   Housekeeping, clear display fields and reset indicators.
000293160128        //
000294160128            *IN60 = '0';
000295160128            EMESS = *Blanks;
000296160128            EMPNO = 0;
000297160128            ACODE = *Blanks;
000298160128        //  error loop start
000299160128            DoU not *IN60;
000300160128              ENAME = *Blanks;
000301160128              EMCAT = *Blanks;
000302160128              EDEPT = *Blanks;
000303160128              ELOCN = *Blanks;
000304160128              EUSRI = *Blanks;
000305160128              ENHRS = 0;
000306160128        //
000307160128        //   Display employee selection format
000308160128        //
000309160128              Exfmt EMPSEL;
000310160128              // Process keys
000311160128              If *INKC;     //F3 = exit program
000312160128                Return;
000313160128              Elseif *INKD; //F4 = return to main screen
000314160128                Return;
000315160128              EndIf;
000316160128              //
000317160128              //   Access employee master to validate action code request
000318160128              //
000319160128              Chain EMPNO EMPMST;
000320160128              *IN50 = not %Found;
000321160128              ValidateActionCode();
000322160128            EndDo;   //  end of error loop
000323160128        //
000324160128        //   Display employee maintenance format
000325160128        //
000326160128            Exfmt EMPMNT;
000327160128            If *INKC;     //F3 = exit program
000328160128              Return;
000329160128            Elseif *INKD; //F4 = return to main screen
000330160128              Return;
000331160128            Elseif *INKE; //F5 = return to employee maintenance screen
000332160128              Iter;
000333160128            EndIf;
000334160128        //
000335160128        //   Determine update mode and perform record add or update
000336160128        //
000337160128              Select;
000338160128                When isAddNewRecordRequest();
000339160128                  ACREC = 'A';
000340160128                  Write RCEMP;
000341160128                When isAddPreviouslyDeletedRecordRequest();
000342160128                  ACREC = 'A';
000343160128                  Update RCEMP;
000344160128        // Mark record deleted
000345160128                When ACODE = 'D';
000346160128                  ACREC = 'D';
000347160128                  Update RCEMP;
000348160128        // Change record
000349160128                When ACODE = 'C';
000350160128                  Update RCEMP;
000351160128              EndSl;
000352160128          EndDo;  // EmployeeMainenance screen loop
000353160128        End-Proc;
000354160128
000355160128        // *********************************************************
000356160128        //   Project master maintenance routine.
000357160128        // *********************************************************
000358160128        Dcl-Proc ProjectMaintenance;
000359160128          DoU *INKC;
000360160128        //
000361160128        //   Housekeeping, clear display fields and reset indicators.
000362160128        //
000363160128            *IN60 = '0';
000364160128            EMESS = *Blanks;
000365160128            PRCDE = *Blanks;
000366160128            ACODE = *Blanks;
000367160128            DoU not *IN60;
000368160128              PRDSC = *Blanks;
000369160128              PRRSP = *Blanks;
000370160128              PRSTR = 0;
000371160128              PREND = 0;
000372160128              PRCMP = 0;
000373160128              PREST = 0;
000374160128              //
000375160128              //   Display project selection format
000376160128              //
000377160128              Exfmt PRJSEL;
000378160128              If *INKD;
000379160128                Return;
000380160128              EndIf;
000381160128              //
000382160128              //   Access project master to validate action code request
000383160128              //
000384160128              If not *INKC;
000385160128                Chain PRCDE PRJMST;
000386160128                *IN50 = not %Found;
000387160128                CallP ValidateActionCode();
000388160128              Else;
000389160128                Return;
000390160128              EndIf;
000391160128            EndDo;
000392160128            //
000393160128            //   Display project maintenance format
000394160128            //
000395160128            Exfmt PRJMNT;
000396160128            If *INKD; // F4 = return to main selection
000397160128              Return;
000398160128            EndIf;
000399160128            If *INKF; // F7 = return to project maintenance screen
000400160128              Iter;
000401160128            EndIf;
000402160128            //
000403160128            //   Determine update mode and perform record add or update
000404160128            //
000405160128            If *INKC = '0';
000406160128              Select;
000407160128                // Add of new record
000408160128                When isAddNewRecordRequest();
000409160128                  ACREC = 'A';
000410160128                  Write RCPRJ;
000411160128                // Add of previously deleted
000412160128                When isAddPreviouslyDeletedRecordRequest();
000413160128                  ACREC = 'A';
000414160128                  Update RCPRJ;
000415160128                //  Delete OP = just mark record deleted
000416160128                When ACODE = 'D';
000417160128                  ACREC = 'D';
000418160128                  Update RCPRJ;
000419160128                //  Change OP
000420160128                When ACODE = 'C';
000421160128                  Update RCPRJ;
000422160128              EndSl;
000423160128            Else;
000424160128              Return; // F3 = exit
000425160128            EndIf;
000426160128          EndDo; // Project maintenance loop
000427160128        End-Proc;
000428160128
000429160128        // *******************************************************
000430160128        // Return true iff a new record should be added to file
000431160128        Dcl-Proc isAddNewRecordRequest;
000432160128          Dcl-Pi *n ind End-Pi;
000433160128          Return  ACODE = 'A'  and *IN50;
000434160128        End-Proc;
000435160128
000436160128        // *******************************************************
000437160128        // Return true iff a previously record should be readded to file
000438160128        Dcl-Proc isAddPreviouslyDeletedRecordRequest;
000439160128          Dcl-Pi *n ind End-Pi;
000440160128          Return  ACODE = 'A' and not *IN50 and ACREC = 'D';
000441160128        End-Proc;
000442160128
000443160128        // *******************************************************
000444160128        //   ValidateFileToMaintainSelection procedure verifies the time reporti
000445160128        Dcl-Proc ValidateFileToMaintainSelection;
000446160128          //
000447160128          //   Housekeeping, clear display fields and reset indicators.
000448160128          //
000449160128          EMESS = *Blanks;
000450160128          *IN60 = *OFF;
000451160128          //
000452160128          //   The following IF AND OR combination checks the application
000453160128          //   selection fields to ensure that only one application has been
000454160128          //   selected.
000455160128          //
000456160128         If EMPAPL = 'X' and PRJAPL = 'X'
000457160128         or EMPAPL = 'X' and RSNAPL = 'X'
000458160128         or RSNAPL = 'X' and PRJAPL = 'X';
000459160128            DisplayError(2);
000460160128          EndIf;
000461160128          //
000462160128          //   The following IF AND combination ensures that at least one
000463160128          //   application has been selected.
000464160128          //
000465160128          If    EMPAPL = ' '
000466160128            and PRJAPL = ' '
000467160128            and RSNAPL = ' ';
000468160128            DisplayError(3);
000469160128          EndIf;
000470160128          //
000471160128          //   The following code checks each application selection field to
000472160128          //   ensure that it is either ' ' (blank) or equal to 'X'.
000473160128          //
000474160128          If    EMPAPL <> ' '
000475160128            and EMPAPL <> 'X'
000476160128          or
000477160128                PRJAPL <> ' '
000478160128            and PRJAPL <> 'X'
000479160128          or
000480160128                RSNAPL <> ' '
000481160128            and RSNAPL <> 'X';
000482160128            DisplayError(1);
000483160128          EndIf;
000484160128        End-Proc;
000485160128        //
000486160128        // *******************************************************
000487160128        //   ValidateActionCode procedure verifies the time reporting action cod
000488160128        //   all maintenance selections.
000489160128        // *******************************************************
000490160128        Dcl-Proc ValidateActionCode;
000491160128        //
000492160128        //   Housekeeping, clear display fields and reset indicators.
000493160128        //
000494160128       EMESS = *BLANKS;
000495160128       *IN60 = *OFF;
000496160128       *IN90 = *OFF;
000497160128       //
000498160128       //   The following  statements perform
000499160128       //   two functions.  First they determine the type of maintenance
000500160128       //   requested and branche to the appropriate subroutine and secondly
000501160128       //   they determine if the maintenance code entered is invalid.
000502160128       //
000503160128       Select;
000504160128       When ACODE = 'A';
000505160128       ValidateAdd();
000506160128       When ACODE = 'C';
000507160128       ValidateChange();
000508160128       When ACODE = 'D';
000509160128       ValidateDelete();
000510160128       Other;
000511160128       DisplayError(INVALID_ACTION_CODE);
000512160128       EndSl;
000513160128        End-Proc;
000514160128        //
000515160128        //   The following code verifies the add request.
000516160128        //
000517160128        Dcl-Proc ValidateAdd;
000518160128        If NOT *IN50 AND ACREC = 'A';
000519160128        DisplayError(5);
000520160128        Else;
000521160128        // record found but previously deleted - warning
000522160128        If NOT *IN50 AND ACREC = 'D';
000523160128        EMESS = ERR(6);
000524160128        EndIf;
000525160128        EndIf;
000526160128        End-Proc;
000527160128        //
000528160128        //   The following code verifies the change request.
000529160128        //
000530160128        Dcl-Proc ValidateChange;
000531160128          If *IN50;
000532160128            DisplayError(7);
000533160128          Else;
000534160128            If NOT *IN50 AND ACREC = 'D';
000535160128              DisplayError(8);
000536160128            EndIf;
000537160128          EndIf;
000538160128        End-Proc;
000539160128        //*
000540160128        //*  The following code verifies the delete request.  The field
000541160128        //*  protect indicator *IN90 is first set on (equal to 1) to not
000542160128        //*  allow changes to existing data on a delete request.
000543160128        //*
000544160128        Dcl-Proc ValidateDelete;
000545160128          *IN90 = *ON;
000546160128          IF *IN50;
000547160128            DisplayError(9);
000548160128          ELSE;
000549160128            IF NOT  *IN50 AND ACREC = 'D';
000550160128              DisplayError(10);
000551160128            ENDIF;
000552160128          ENDIF;
000553160128        End-Proc;
000554160128        //
000555160128        // Display error at the bottom of the screen with the given error code
000556160128        Dcl-Proc DisplayError;
000557160128          Dcl-Pi *n;
000558160128            errCode uns(3) const;
000559160128          End-Pi;
000560160128          *IN60 = *ON;
000561160128          EMESS = ERR(errCode);
000562160128        End-Proc;
000563160128        //
000564160128        //   The compile time array ERR is entered below.  The array is
000565160128        //   precceded by "** " to denote the beginning of the array.
000566160128        //
000567160128**  Array ERR - Error descriptions
000568160128   MAINTENANCE SELECTION CODE NOT EQUAL TO "X"
000569160128MORE THAN ONE APPLICATION SELECTED FOR MAINTENANCE
000570160128     NO APPLICATION SELECTED FOR MAINTENANCE
000571160128    ACTION CODE NOT EQUAL TO "A", "C" OR "D"
000572160128 ADD REQUESTED BUT RECORD ALREADY EXISTS IN FILE
000573160128    WARNING - RECORD WAS PREVIOUSLY DELETED
000574160128   CHANGE REQUESTED BUT RECORD DOES NOT EXIST
000575160128     CHANGE REQUESTED BUT RECORD IS DELETED
000576160128   DELETE REQUESTED BUT RECORD DOES NOT EXIST
000577160128   DELETE REQUESTED BUT RECORD ALREADY DELETED
