
/*------------------------------------------------------------------------
    File        : ExportToCsv.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sooraj
    Created     : Thu Nov 02 19:00:49 IST 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW. 


DEFINE INPUT PARAMETER ipquery  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipTname   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdir    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipfile    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipfields AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE ttExportDate 
    FIELD Field_num  AS INTEGER
    FIELD Field_data AS CHARACTER.
    
DEFINE STREAM ExportToCsv.

DEFINE VARIABLE qh       AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_table  AS HANDLE   NO-UNDO.
DEFINE VARIABLE inum     AS INTEGER  NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE FieldList AS CHARACTER NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION GetDir RETURNS CHARACTER 
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */

CREATE QUERY qh.
CREATE BUFFER h_table FOR TABLE ipTname.
OUTPUT STREAM ExportToCsv TO VALUE(GetDir()).

ASSIGN inum = 1.

qh:SET-BUFFERS(h_table).
qh:QUERY-PREPARE(ipquery).
qh:QUERY-OPEN.

DO iLoop = 1 TO NUM-ENTRIES(ipfields, ","):
    ASSIGN FieldList = FieldList + "," + (ENTRY(iLoop, ipfields, ",")).
END.

PUT STREAM ExportToCsv UNFORMATTED LEFT-TRIM(FieldList, ",") SKIP.

repeating:
    REPEAT WITH FRAME y:
        qh:GET-NEXT().
        IF qh:QUERY-OFF-END THEN LEAVE repeating.

        CREATE ttExportDate.

        DO iLoop = 1 TO NUM-ENTRIES(ipfields, ","):
            DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
            ASSIGN cFieldName = ENTRY(iLoop, ipfields, ",").
            ASSIGN ttExportDate.Field_num = inum.
            ttExportDate.Field_data = ttExportDate.Field_data + "," +
                REPLACE(STRING(h_table:BUFFER-FIELD(cFieldName):BUFFER-VALUE), ",", "/")
                NO-ERROR.
        END.

        RELEASE ttExportDate.
        ASSIGN inum = inum + 1.
    END.

qh:QUERY-CLOSE().

FOR EACH ttExportDate NO-LOCK:
    PUT STREAM ExportToCsv UNFORMATTED LEFT-TRIM(ttExportDate.Field_data, ",") SKIP.
END.

OUTPUT STREAM ExportToCsv CLOSE.
RUN OpenExcel(INPUT GetDir()).
DELETE OBJECT qh.
ASSIGN qh = ?.

IF VALID-HANDLE(h_table) THEN DELETE OBJECT h_table.
  ASSIGN h_table = ?.


/* **********************  Internal Procedures  *********************** */

PROCEDURE OpenExcel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER vcFilename AS CHARACTER NO-UNDO.

DEFINE VARIABLE chExcel        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE SaveFilename   AS CHARACTER NO-UNDO.

ASSIGN SaveFilename = REPLACE(vcFilename,".csv",".xlsx").

CREATE "Excel.Application" chExcel.
 ASSIGN
    chExcel:VISIBLE = TRUE.
chWorkbook=chExcel:Workbooks:Open(vcFilename)NO-ERROR.
chWorkbook = chExcel:Workbooks:Item(1).
chWorkSheet = chExcel:Sheets:Item(1).
chWorksheet:Range("1:1"):SELECT.
chExcel:Selection:FONT:Bold = TRUE.
chWorksheet:Cells:Select.
chWorksheet:Cells:EntireColumn:AutoFit.
chWorksheet:Range("A2"):SELECT.
chExcel:DisplayAlerts = FALSE.
  
chWorkbook:SaveAs(vcFilename,51,,,,,).



END PROCEDURE.    

/* ************************  Function Implementations ***************** */


FUNCTION GetDir RETURNS CHARACTER 
    ( ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    

    DEFINE VARIABLE VcfileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE VcDirName  AS CHARACTER NO-UNDO.
    
    FILE-INFO:FILE-NAME = ipdir.
    IF FILE-INFO:FULL-PATHNAME <> ? THEN
       ASSIGN VcDirName = ipdir.
    ELSE DO:
      ASSIGN VcDirName = SESSION:TEMP-DIR
             VcDirName = REPLACE(VcDirName,"\","/").
    END.
       
       
    ASSIGN VcfileName =  RIGHT-TRIM(VcDirName, "/") + "/" + ipfile + ".csv".
    
    RETURN VcfileName.
       
END FUNCTION.
