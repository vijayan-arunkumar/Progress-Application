&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

 DEFINE VARIABLE del_ok AS LOGICAL NO-UNDO.
 
/* Local Variable Definitions ---                                       */
 DEFINE TEMP-TABLE ttvalues 
 FIELD ttquery        AS CHARACTER FORMAT "X(80)"  COLUMN-LABEL "Query"
 FIELD ttfileName     AS CHARACTER FORMAT "X(20)"  COLUMN-LABEL "File Nmae"
 FIELD ttTablename    AS CHARACTER FORMAT "X(20)"  COLUMN-LABEL "Table Name"
 FIELD ttfilelocation AS CHARACTER FORMAT "X(20)"  COLUMN-LABEL "File Location"
 FIELD ttfieldnames   AS CHARACTER FORMAT "X(20)"  COLUMN-LABEL "Field Names".

 DEFINE TEMP-TABLE ttselected LIKE ttvalues.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_ViewQuery

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttvalues

/* Definitions for BROWSE Br_ViewQuery                                  */
&Scoped-define FIELDS-IN-QUERY-Br_ViewQuery ttvalues.ttfileName ttvalues.ttquery ttvalues.tttableName ttvalues.ttfilelocation ttvalues.ttfieldnames   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ViewQuery   
&Scoped-define SELF-NAME Br_ViewQuery
&Scoped-define QUERY-STRING-Br_ViewQuery FOR EACH ttvalues NO-LOCK
&Scoped-define OPEN-QUERY-Br_ViewQuery OPEN QUERY {&SELF-NAME} FOR EACH ttvalues NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Br_ViewQuery ttvalues
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ViewQuery ttvalues


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Br_ViewQuery}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 Br_ViewQuery q-delt BUTTON-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     FONT 0.

DEFINE BUTTON BUTTON-5 
     LABEL "Export" 
     SIZE 20 BY 1.14
     FONT 0.

DEFINE BUTTON q-delt 
     LABEL "Delete" 
     SIZE 20 BY 1.14
     FONT 0.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_ViewQuery FOR 
      ttvalues SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_ViewQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ViewQuery C-Win _FREEFORM
  QUERY Br_ViewQuery DISPLAY
      ttvalues.ttfileName
  ttvalues.ttquery
  ttvalues.tttableName
  ttvalues.ttfilelocation
  ttvalues.ttfieldnames
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 140 BY 16.14
         FONT 3 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 1.95 COL 136 WIDGET-ID 4
     Br_ViewQuery AT ROW 4.86 COL 11 WIDGET-ID 200
     q-delt AT ROW 21.95 COL 11 WIDGET-ID 10
     BUTTON-5 AT ROW 21.95 COL 131.2 WIDGET-ID 6
     "<- Saved Query" VIEW-AS TEXT
          SIZE 26 BY .95 AT ROW 1.95 COL 11.2 WIDGET-ID 2
          FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 23.95 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "View Saved Queries"
         HEIGHT             = 23.95
         WIDTH              = 160
         MAX-HEIGHT         = 33.19
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.19
         VIRTUAL-WIDTH      = 273.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB Br_ViewQuery BUTTON-1 DEFAULT-FRAME */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ViewQuery
/* Query rebuild information for BROWSE Br_ViewQuery
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttvalues NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Br_ViewQuery */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* View Saved Queries */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* View Saved Queries */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Export */
DO:
  IF  AVAIL ttvalues THEN
   RUN exporttocsv.p(INPUT ttvalues.ttquery,INPUT ttvalues.tttableName,INPUT ttvalues.ttfilelocation ,INPUT ttvalues.ttfileName ,INPUT ttvalues.ttfieldnames).
  ELSE 
   RETURN.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME q-delt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL q-delt C-Win
ON CHOOSE OF q-delt IN FRAME DEFAULT-FRAME /* Delete */
DO:
  IF  AVAIL ttvalues THEN
  DO:
   RUN deleteQuery(INPUT ttvalues.ttquery,INPUT ttvalues.tttableName,INPUT ttvalues.ttfilelocation ,INPUT ttvalues.ttfileName ,INPUT ttvalues.ttfieldnames,OUTPUT del_ok).
   IF del_ok THEN
   DO:
    RUN getvalues.   
   END.
  END.
  ELSE 
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_ViewQuery
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN getvalues.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteQuery C-Win 
PROCEDURE deleteQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipquery  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipTname   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdir    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipfile    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipfields AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER del_ok AS LOGICAL NO-UNDO.
                                    
DEFINE VARIABLE hTable  AS HANDLE     NO-UNDO.
DEFINE VARIABLE qTable  AS HANDLE     NO-UNDO.

CREATE BUFFER hTable  FOR TABLE "excelexport".

CREATE QUERY qTable.
qTable:ADD-BUFFER(hTable).
qTable:QUERY-PREPARE("FOR EACH excelexport WHERE excelexport.querycopy = '" + ipquery + "' AND excelexport.fieldnames = '" + ipfields + "' AND excelexport.filelocation = '" + ipdir +
                                           "' AND excelexport.FILENAME = '" + ipfile + "' AND excelexport.tableName = '" + ipTname + "' :").
qTable:QUERY-OPEN().

qTable:GET-FIRST().
DO WHILE hTable:AVAILABLE:
 IF hTable:AVAILABLE THEN DO:
   DO TRANSACTION:
            qTable:GET-CURRENT(EXCLUSIVE-LOCK).
            hTable:BUFFER-DELETE().
        END.
   
   CLOSE QUERY Br_ViewQuery.
   ASSIGN del_ok =TRUE.
 END.
 ELSE RETURN.
 
qTable:GET-NEXT().

END.

qTable:QUERY-CLOSE(). 
DELETE OBJECT qTable.
DELETE OBJECT hTable.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BUTTON-1 Br_ViewQuery q-delt BUTTON-5 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getvalues C-Win 
PROCEDURE getvalues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 EMPTY TEMP-TABLE ttvalues. 

 
DEFINE VARIABLE hTable  AS HANDLE     NO-UNDO.
DEFINE VARIABLE qTable  AS HANDLE     NO-UNDO.

CREATE BUFFER hTable  FOR TABLE "excelexport".

CREATE QUERY qTable.
qTable:ADD-BUFFER(hTable).
qTable:QUERY-PREPARE("FOR EACH excelexport NO-LOCK").
qTable:QUERY-OPEN().

qTable:GET-FIRST().
DO WHILE hTable:AVAILABLE:

   CREATE ttvalues.
   ASSIGN ttvalues.ttquery        = hTable:BUFFER-FIELD("querycopy"):BUFFER-VALUE
          ttvalues.ttfileName     = hTable:BUFFER-FIELD("filename"):BUFFER-VALUE
          ttvalues.tttableName    = hTable:BUFFER-FIELD("tableName"):BUFFER-VALUE
          ttvalues.ttfilelocation = hTable:BUFFER-FIELD("filelocation"):BUFFER-VALUE
          ttvalues.ttfieldnames   = hTable:BUFFER-FIELD("fieldnames"):BUFFER-VALUE  .
          
   
   qTable:GET-NEXT().

END.

qTable:QUERY-CLOSE(). 
DELETE OBJECT qTable.
DELETE OBJECT hTable.
{&OPEN-QUERY-Br_ViewQuery} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

