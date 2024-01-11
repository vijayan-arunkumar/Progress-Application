
/*------------------------------------------------------------------------
    File        : 
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE ipquery  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipTname  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipdir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipfile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE params1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE params2  AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Retrieving the value of params environment variable */
ASSIGN params1 = OS-GETENV("params1")
       params2 = OS-GETENV("params2").

DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
DEFINE VARIABLE qTable AS HANDLE NO-UNDO.

CREATE BUFFER hTable FOR TABLE "excelexport".
CREATE QUERY qTable.
qTable:ADD-BUFFER(hTable).

/* Construct the query */
qTable:QUERY-PREPARE("FOR EACH excelexport WHERE excelexport.filename = '" + params1 + "' AND excelexport.fieldnames = '" + params2 + "' NO-LOCK .").

qTable:QUERY-OPEN().

 DO:

    qTable:GET-FIRST().
    DO WHILE NOT qTable:QUERY-OFF-END:
        ASSIGN
            ipquery   = hTable:BUFFER-FIELD("querycopy"):BUFFER-VALUE
            ipfile    = hTable:BUFFER-FIELD("filename"):BUFFER-VALUE
            ipTname   = hTable:BUFFER-FIELD("tableName"):BUFFER-VALUE
            ipdir     = hTable:BUFFER-FIELD("filelocation"):BUFFER-VALUE
            ipfields  = hTable:BUFFER-FIELD("fieldnames"):BUFFER-VALUE.

        qTable:GET-NEXT().
    END.

    qTable:QUERY-CLOSE().
    DELETE OBJECT qTable.
    DELETE OBJECT hTable.

END.

DEFINE TEMP-TABLE ttExportDate 
    FIELD Field_num  AS INTEGER
    FIELD Field_data AS CHARACTER.
    
DEFINE STREAM ExportToCsv.

DEFINE VARIABLE qh       AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_table  AS HANDLE   NO-UNDO.
DEFINE VARIABLE inum     AS INTEGER  NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE FieldList AS CHARACTER NO-UNDO.
 


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

DELETE OBJECT qh.
ASSIGN qh = ?.

IF VALID-HANDLE(h_table) THEN DELETE OBJECT h_table.
  ASSIGN h_table = ?.
   

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
    ELSE
       ASSIGN VcDirName = SESSION:TEMP-DIR.
       
    ASSIGN VcfileName =  RIGHT-TRIM(VcDirName, "/") + "/" + ipfile + ".csv".
    
    RETURN VcfileName.
       
END FUNCTION.
