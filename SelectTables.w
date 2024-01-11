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
DEFINE INPUT PARAMETER ipdbName AS CHARACTER NO-UNDO.
   
/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttTables NO-UNDO
  FIELD tableNum    AS INTEGER                   COLUMN-LABEL "Table Number"
  FIELD tableName   AS CHARACTER FORMAT "X(32)"  COLUMN-LABEL "Table Name"
  FIELD tableDBName AS CHARACTER FORMAT "X(20)"  COLUMN-LABEL "Database"
  FIELD tableDump   AS CHARACTER FORMAT "X(10)"  COLUMN-LABEL "Dump Name"
  FIELD tableLabel  AS CHARACTER FORMAT "X(75)"  COLUMN-LABEL "Label"
  FIELD tableDesc   AS CHARACTER FORMAT "X(255)" COLUMN-LABEL "Description"

  INDEX PRIMARY tableNum
  INDEX NAME    tableName
  INDEX DUMP    tableDump.

DEFINE TEMP-TABLE tttable1 
FIELD tname AS CHAR.

DEFINE TEMP-TABLE ttselected LIKE  ttTables.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-15

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTables ttselected

/* Definitions for BROWSE BROWSE-15                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-15 ttTables.tableName ttTables.tableLabel ttTables.tableNum   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-15   
&Scoped-define SELF-NAME BROWSE-15
&Scoped-define QUERY-STRING-BROWSE-15 FOR EACH ttTables
&Scoped-define OPEN-QUERY-BROWSE-15 OPEN QUERY {&SELF-NAME} FOR EACH ttTables.
&Scoped-define TABLES-IN-QUERY-BROWSE-15 ttTables
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-15 ttTables


/* Definitions for BROWSE BROWSE-17                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-17 ttselected.tableName ttselected.tableLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-17   
&Scoped-define SELF-NAME BROWSE-17
&Scoped-define QUERY-STRING-BROWSE-17 FOR EACH ttselected.  APPLY 'VALUE-CHANGED':U TO ViewQuery
&Scoped-define OPEN-QUERY-BROWSE-17 OPEN QUERY {&SELF-NAME} FOR EACH ttselected.  APPLY 'VALUE-CHANGED':U TO ViewQuery.
&Scoped-define TABLES-IN-QUERY-BROWSE-17 ttselected
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-17 ttselected


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-15}~
    ~{&OPEN-QUERY-BROWSE-17}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-15 BUTTON-1 BROWSE-17 BUTTON-6 ~
BUTTON-7 ViewQuery 
&Scoped-Define DISPLAYED-OBJECTS ViewQuery 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1  NO-FOCUS
     LABEL "Move to query builder" 
     SIZE 34 BY 1.19
     FGCOLOR 13 FONT 3.

DEFINE BUTTON BUTTON-6 
     LABEL "Add >" 
     SIZE 12 BY 1.14
     FONT 3.

DEFINE BUTTON BUTTON-7 
     LABEL "< Remove" 
     SIZE 12 BY 1.14
     FONT 3.

DEFINE VARIABLE ViewQuery AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 61 BY 4.76 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-15 FOR 
      ttTables SCROLLING.

DEFINE QUERY BROWSE-17 FOR 
      ttselected SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-15 C-Win _FREEFORM
  QUERY BROWSE-15 DISPLAY
      ttTables.tableName 
      ttTables.tableLabel
       ttTables.tableNum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 11.43
         FONT 3 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-17 C-Win _FREEFORM
  QUERY BROWSE-17 DISPLAY
      ttselected.tableName 
      ttselected.tableLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 11.43
         FONT 3 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-15 AT ROW 5.76 COL 11 WIDGET-ID 400
     BUTTON-1 AT ROW 1.95 COL 117.2 WIDGET-ID 4
     BROWSE-17 AT ROW 5.76 COL 91 WIDGET-ID 500
     BUTTON-6 AT ROW 9.95 COL 75 WIDGET-ID 6
     BUTTON-7 AT ROW 11.81 COL 75 WIDGET-ID 8
     ViewQuery AT ROW 19.33 COL 11 NO-LABEL WIDGET-ID 14
     "Query" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 18.19 COL 11.2 WIDGET-ID 10
          FONT 3
     "<- Query" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 1.95 COL 11 WIDGET-ID 2
          FONT 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.4 BY 24.05
         BGCOLOR 16  WIDGET-ID 100.


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
         TITLE              = "Table Selection"
         HEIGHT             = 24.05
         WIDTH              = 160.4
         MAX-HEIGHT         = 24.05
         MAX-WIDTH          = 160.4
         VIRTUAL-HEIGHT     = 24.05
         VIRTUAL-WIDTH      = 160.4
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-15 TEXT-2 DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-17 BUTTON-1 DEFAULT-FRAME */
ASSIGN 
       BUTTON-1:MANUAL-HIGHLIGHT IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       ViewQuery:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-15
/* Query rebuild information for BROWSE BROWSE-15
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTables.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-15 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-17
/* Query rebuild information for BROWSE BROWSE-17
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttselected.

APPLY 'VALUE-CHANGED':U TO ViewQuery.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-17 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Table Selection */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Table Selection */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Move to query builder */
DO:
   RUN selectfields.w(INPUT TABLE ttselected ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* Add > */
DO:
  IF NOT AVAIL ttTables THEN
    RETURN.
  
  CREATE ttselected.
  BUFFER-COPY ttTables TO ttselected.
  RELEASE ttselected.
  
  DELETE ttTables.
      
 {&OPEN-QUERY-BROWSE-17}
 {&OPEN-QUERY-BROWSE-15}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 C-Win
ON CHOOSE OF BUTTON-7 IN FRAME DEFAULT-FRAME /* < Remove */
DO:
  IF NOT AVAIL ttselected THEN
    RETURN.
  
  CREATE ttTables.
  BUFFER-COPY ttselected TO ttTables.
  RELEASE ttTables.
  
  DELETE ttselected.
      
 {&OPEN-QUERY-BROWSE-17}
 {&OPEN-QUERY-BROWSE-15}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ViewQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ViewQuery C-Win
ON VALUE-CHANGED OF ViewQuery IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE query1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE first1 AS LOGICAL   NO-UNDO.
    
    ASSIGN first1 = TRUE
           query1 = "".
           
    FOR EACH ttselected NO-LOCK:
     IF AVAILABLE ttselected THEN DO:
       IF first1 THEN
         ASSIGN query1 = "FOR EACH " + ttselected.tableName  + " NO-LOCK :".
       ELSE DO:
         ASSIGN query1 =  REPLACE(query1, ":",",").
                query1 =  query1 + "~n EACH " + ttselected.tableName + " NO-LOCK :".
       END.
       ASSIGN first1 =  FALSE.
      END.
      
    END.
    
    ASSIGN ViewQuery = query1 .
    DISPLAY ViewQuery  WITH FRAME default-frame  IN WINDOW c-win . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-15
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
RUN retrivetables(INPUT ipdbName).  
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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY ViewQuery 
      WITH FRAME DEFAULT-FRAME.
  ENABLE BROWSE-15 BUTTON-1 BROWSE-17 BUTTON-6 BUTTON-7 ViewQuery 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retrivetables C-Win 
PROCEDURE retrivetables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipDB AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hTables  AS HANDLE     NO-UNDO.
DEFINE VARIABLE qTables  AS HANDLE     NO-UNDO.

CREATE BUFFER hTables  FOR TABLE STRING(ipDB) + ""  BUFFER-NAME "b_FILE".

CREATE QUERY qTables.
qTables:ADD-BUFFER(hTables).
qTables:QUERY-PREPARE("FOR EACH " + hTables:NAME + " WHERE " +  hTables:NAME + "._Tbl-Type = " + '"' + "T" + '"' + " NO-LOCK").
qTables:QUERY-OPEN().

qTables:GET-FIRST().
DO WHILE hTables:AVAILABLE:

   CREATE ttTables.
   ASSIGN ttTables.tableNum    = hTables:RECID
          ttTables.tableName   = hTables:BUFFER-FIELD("_file-name"):BUFFER-VALUE
          ttTables.tableDump   = hTables:BUFFER-FIELD("_dump-name"):BUFFER-VALUE
          ttTables.tableLabel  = hTables:BUFFER-FIELD("_file-label"):BUFFER-VALUE
          ttTables.tableDesc   = hTables:BUFFER-FIELD("_desc"):BUFFER-VALUE
          ttTables.tableDBName = ipDB.   
qTables:GET-NEXT().

END.

qTables:QUERY-CLOSE().
DELETE OBJECT qTables.
DELETE OBJECT hTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rfield C-Win 
PROCEDURE rfield :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

