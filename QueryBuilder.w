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
 
{t_table.i}
{t_field.i}

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR ttTables.
DEFINE INPUT PARAMETER TABLE FOR ttfields.   /* selected fields list */ 


/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttfields02 LIKE ttfields. 
DEFINE VARIABLE temp-string AS CHARACTER NO-UNDO.
DEFINE VARIABLE vWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE vQuery AS CHARACTER NO-UNDO.
DEFINE VARIABLE OutValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE OutOK AS LOGICAL NO-UNDO.
DEFINE VARIABLE Q_ok2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE f_ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE ipquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipTname AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcdir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipvcfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipvcdir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbFnd_ok AS logic NO-UNDO.
DEFINE VARIABLE updt_ok AS logic NO-UNDO.
DEFINE VARIABLE timenow AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_ViewFields

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttfields02

/* Definitions for BROWSE Br_ViewFields                                 */
&Scoped-define FIELDS-IN-QUERY-Br_ViewFields ttfields02.fieldOrder ttfields02.fieldName ttfields02.fieldLabel ttfields02.fieldDataType ttfields02.fieldFormat ttfields02.fieldMandatory   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ViewFields   
&Scoped-define SELF-NAME Br_ViewFields
&Scoped-define QUERY-STRING-Br_ViewFields FOR EACH ttfields02
&Scoped-define OPEN-QUERY-Br_ViewFields OPEN QUERY {&SELF-NAME} FOR EACH ttfields02.
&Scoped-define TABLES-IN-QUERY-Br_ViewFields ttfields02
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ViewFields ttfields02


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Br_ViewFields}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Table BUTTON-3 ViewQuery btn_Save ~
Btn_cncl Btn_exprt Btn_Clear 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Table ViewQuery 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_and 
     LABEL "AND" 
     SIZE 20.4 BY 1.14
     FONT 3.

DEFINE BUTTON Btn_Clear 
     LABEL "Clear" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_cncl AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     FONT 3.

DEFINE BUTTON btn_eq 
     LABEL "=" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON Btn_exprt 
     LABEL "Export To Excel" 
     SIZE 23 BY 1.14.

DEFINE BUTTON btn_GE 
     LABEL ">=" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON btn_Gt 
     LABEL ">" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON btn_LE 
     LABEL "<=" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON btn_lt 
     LABEL "<" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON btn_Neq 
     LABEL "<>" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON btn_or 
     LABEL "OR" 
     SIZE 20.4 BY 1.14
     FONT 3.

DEFINE BUTTON btn_Save 
     LABEL "Save" 
     SIZE 15 BY 1.14
     FONT 3.

DEFINE BUTTON Btn_Where 
     LABEL "WHERE" 
     SIZE 20.4 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Check Syntax" 
     SIZE 20.4 BY 1.14.

DEFINE VARIABLE Cmb_Table AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 3 NO-UNDO.

DEFINE VARIABLE ViewQuery AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 80 BY 4.57 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_ViewFields FOR 
      ttfields02 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_ViewFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ViewFields C-Win _FREEFORM
  QUERY Br_ViewFields DISPLAY
      ttfields02.fieldOrder
ttfields02.fieldName
ttfields02.fieldLabel
ttfields02.fieldDataType
ttfields02.fieldFormat
ttfields02.fieldMandatory
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 10.48
         BGCOLOR 8 FONT 3 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Cmb_Table AT ROW 1.95 COL 19.2 COLON-ALIGNED WIDGET-ID 2
     Br_ViewFields AT ROW 4 COL 11 WIDGET-ID 200
     btn_eq AT ROW 4 COL 95.8 WIDGET-ID 4
     btn_Neq AT ROW 4 COL 106.2 WIDGET-ID 6
     btn_lt AT ROW 5.29 COL 95.8 WIDGET-ID 8
     btn_Gt AT ROW 5.29 COL 106.2 WIDGET-ID 20
     btn_LE AT ROW 6.62 COL 95.8 WIDGET-ID 22
     btn_GE AT ROW 6.62 COL 106.2 WIDGET-ID 24
     btn_or AT ROW 7.91 COL 95.8 WIDGET-ID 30
     btn_and AT ROW 9.19 COL 95.8 WIDGET-ID 26
     Btn_Where AT ROW 10.52 COL 95.8 WIDGET-ID 50
     BUTTON-3 AT ROW 11.86 COL 95.8 WIDGET-ID 56
     ViewQuery AT ROW 15.76 COL 11 NO-LABEL WIDGET-ID 48
     btn_Save AT ROW 20.67 COL 10.8 WIDGET-ID 36
     Btn_cncl AT ROW 20.67 COL 26.8 WIDGET-ID 38
     Btn_exprt AT ROW 20.67 COL 42.8 WIDGET-ID 40
     Btn_Clear AT ROW 20.67 COL 67 WIDGET-ID 52
     "Where Criteria" VIEW-AS TEXT
          SIZE 23 BY .95 AT ROW 14.81 COL 11 WIDGET-ID 32
          FONT 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.6 BY 24
         FONT 3
         CANCEL-BUTTON Btn_cncl WIDGET-ID 100.


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
         TITLE              = "Filters"
         HEIGHT             = 24.05
         WIDTH              = 160.6
         MAX-HEIGHT         = 24.05
         MAX-WIDTH          = 160.6
         VIRTUAL-HEIGHT     = 24.05
         VIRTUAL-WIDTH      = 160.6
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
/* BROWSE-TAB Br_ViewFields Cmb_Table DEFAULT-FRAME */
/* SETTINGS FOR BROWSE Br_ViewFields IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_and IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_eq IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_GE IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_Gt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_LE IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_lt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_Neq IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_or IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Where IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ViewQuery:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ViewFields
/* Query rebuild information for BROWSE Br_ViewFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttfields02.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Br_ViewFields */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Filters */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Filters */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_and
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_and C-Win
ON CHOOSE OF btn_and IN FRAME DEFAULT-FRAME /* AND */
DO:
  ASSIGN vQuery = vQuery + " AND ~n".
  APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
  ENABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
  DISABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear C-Win
ON CHOOSE OF Btn_Clear IN FRAME DEFAULT-FRAME /* Clear */
DO:
  ASSIGN ViewQuery = ""
         vWhere = ""
         vQuery = "".
         
  APPLY "VALUE-CHANGED":U TO  Cmb_Table IN FRAME default-frame.
  DISABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_cncl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_cncl C-Win
ON CHOOSE OF Btn_cncl IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_eq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_eq C-Win
ON CHOOSE OF btn_eq IN FRAME DEFAULT-FRAME /* = */
DO:
   RUN GetInput(OUTPUT OutValue,OUTPUT OutOk).
   IF Outok THEN
   DO:
     ASSIGN vQuery = vQuery + ttfields02.fieldName + " = " + OutValue.
     
     APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
     ENABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
     DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_exprt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_exprt C-Win
ON CHOOSE OF Btn_exprt IN FRAME DEFAULT-FRAME /* Export To Excel */
DO:
 
 IF NOT Q_ok2 THEN DO:
     MESSAGE "Please Check Syntax" 
      VIEW-AS ALERT-BOX INFORMATION .
    RETURN.
  END.
        

  RUN GetFileAndDir(OUTPUT f_ok,OUTPUT vcfile,OUTPUT vcdir).

  IF f_ok THEN
  DO:
    RUN exporttocsv.p(INPUT ipquery,INPUT ipTname,INPUT vcdir,INPUT vcfile ,INPUT ipfields). 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_GE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_GE C-Win
ON CHOOSE OF btn_GE IN FRAME DEFAULT-FRAME /* >= */
DO:
  RUN GetInput(OUTPUT OutValue,OUTPUT OutOk).
   IF Outok THEN
   DO:
     ASSIGN vQuery = vQuery +  ttfields02.fieldName + " >= " + OutValue. 
     APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
     DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
   ENABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Gt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Gt C-Win
ON CHOOSE OF btn_Gt IN FRAME DEFAULT-FRAME /* > */
DO:
  RUN GetInput(OUTPUT OutValue,OUTPUT OutOk).
   IF Outok THEN
   DO:
     ASSIGN vQuery = vQuery + ttfields02.fieldName + " >" + OutValue. 
     DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
     ENABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
   END.
   APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_LE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_LE C-Win
ON CHOOSE OF btn_LE IN FRAME DEFAULT-FRAME /* <= */
DO:
  RUN GetInput(OUTPUT OutValue,OUTPUT OutOk).
   IF Outok THEN
   DO:
     ASSIGN vQuery = vQuery + ttfields02.fieldName + " <= " + OutValue.
      DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
     ENABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
   END.
   APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_lt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_lt C-Win
ON CHOOSE OF btn_lt IN FRAME DEFAULT-FRAME /* < */
DO:
  RUN GetInput(OUTPUT OutValue,OUTPUT OutOk).
   IF Outok THEN
   DO:
     ASSIGN vQuery = vQuery + ttfields02.fieldName + " > " + OutValue.
     ENABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
      DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
   END.
   APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Neq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Neq C-Win
ON CHOOSE OF btn_Neq IN FRAME DEFAULT-FRAME /* <> */
DO:
  RUN GetInput(OUTPUT OutValue,OUTPUT OutOk).
   IF Outok THEN
   DO:
     ASSIGN vQuery = vQuery + ttfields02.fieldName + " <> " + OutValue.
     ENABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
      DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
   END.
   APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_or
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_or C-Win
ON CHOOSE OF btn_or IN FRAME DEFAULT-FRAME /* OR */
DO:
   ASSIGN vQuery = vQuery + " OR ~n".
   APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
   ENABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
   DISABLE Btn_Or Btn_And WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Save C-Win
ON CHOOSE OF btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  IF NOT Q_ok2 THEN DO:
     MESSAGE "Please Check Syntax" 
      VIEW-AS ALERT-BOX WARNING.
    RETURN.
  END.
    

  RUN GetFileAndDir(OUTPUT f_ok,OUTPUT vcfile,OUTPUT vcdir).

   IF f_ok THEN
   DO:
      ASSIGN ipvcfile = vcfile
             ipvcdir  = vcdir.
   RUN savedb(INPUT ipquery,INPUT ipTname,INPUT vcdir,INPUT vcfile ,INPUT ipfields,OUTPUT updt_ok).    
   END.
   IF updt_ok THEN
   DO:
   MESSAGE "Do you want to add this task scheduler ? " 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
   IF lChoice THEN
   DO:
   RUN CreateTask.W(INPUT ipvcfile,INPUT ipvcdir,INPUT ipfields).   
   END.
   ELSE RETURN.
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Where
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Where C-Win
ON CHOOSE OF Btn_Where IN FRAME DEFAULT-FRAME /* WHERE */
DO:
   ASSIGN vWhere = " WHERE".
   APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
   
  ENABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
  DISABLE Btn_WHERE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Check Syntax */
DO:
  RUN CheckSyntax(OUTPUT Q_ok2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Table C-Win
ON VALUE-CHANGED OF Cmb_Table IN FRAME DEFAULT-FRAME /* Table */
DO:
  FIND FIRST ttTables WHERE ttTables.tableName = Cmb_Table:SCREEN-VALUE NO-ERROR.
  IF AVAIL ttTables THEN
     RUN rfields(INPUT ttTables.tableDBName,INPUT ttTables.tableNum).
     
   ASSIGN ipTname = Cmb_Table:SCREEN-VALUE.
 
  APPLY "VALUE-CHANGED":U TO  ViewQuery IN FRAME default-frame.
  
  {&OPEN-QUERY-Br_ViewFields} 
  ENABLE Br_ViewFields Btn_Where WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ViewQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ViewQuery C-Win
ON VALUE-CHANGED OF ViewQuery IN FRAME DEFAULT-FRAME
DO:
   DEFINE VARIABLE query1 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE first1 AS LOGICAL   NO-UNDO. 
    
    ASSIGN query1 = "FOR EACH " + Cmb_Table:SCREEN-VALUE + SUBSTITUTE("&1 &2 NO-LOCK :",vWhere,vQuery).
    ASSIGN ViewQuery = query1
           ipquery   = query1.
    
    DISPLAY ViewQuery  WITH FRAME default-frame  IN WINDOW c-win . 
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_ViewFields
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
  
  FOR EACH ttTables NO-LOCK:
    ASSIGN temp-string = IF temp-string = "" THEN ttTables.tableName 
                         ELSE temp-string + "," + ttTables.tableName.
  END.
  
  FOR EACH ttfields NO-LOCK:
    ASSIGN ipfields = ipfields + ttfields.fieldName + ",". 
  END.
  
  ASSIGN Cmb_Table:LIST-ITEMS IN FRAME DEFAULT-FRAME = temp-string.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckSyntax C-Win 
PROCEDURE CheckSyntax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETE q_ok AS LOGICAL NO-UNDO.

DEFINE VARIABLE iMsgNo  AS INTEGER NO-UNDO.
DEFINE VARIABLE Qhandle AS HANDLE  NO-UNDO.
DEFINE VARIABLE hb      AS HANDLE  NO-UNDO.
DEFINE VARIABLE ErMsg   AS CHAR    NO-UNDO.

CREATE QUERY Qhandle.
CREATE BUFFER hb FOR TABLE STRING(ipTname).
Qhandle:SET-BUFFERS(hb).

q_ok = Qhandle:QUERY-PREPARE(ipquery)NO-ERROR.

IF NOT q_ok THEN  DO:
 DO iMsgNo = 1 TO ERROR-STATUS:NUM-MESSAGES:
 
    ASSIGN ErMsg = ErMsg +  "Error number: " + string(ERROR-STATUS:GET-NUMBER(iMsgNo)) + " " + 
    ERROR-STATUS:GET-MESSAGE(iMsgNo) + " ~n" .
    
 END.
 MESSAGE ErMsg
    VIEW-AS ALERT-BOX ERROR.
END.
ELSE 
    MESSAGE "Syntax is correct."
         VIEW-AS ALERT-BOX INFORMATION .
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableFields C-Win 
PROCEDURE DisableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DISABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableFields C-Win 
PROCEDURE EnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF ttfields02.fieldName <> "" AND ttfields02.fieldName <> ? THEN
 DO:
   ENABLE btn_eq btn_GE btn_Gt btn_LE btn_lt btn_Neq WITH FRAME {&FRAME-NAME}. 
 END.

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
  DISPLAY Cmb_Table ViewQuery 
      WITH FRAME DEFAULT-FRAME.
  ENABLE Cmb_Table BUTTON-3 ViewQuery btn_Save Btn_cncl Btn_exprt Btn_Clear 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFileAndDir C-Win 
PROCEDURE GetFileAndDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER f_ok   AS LOGICAL NO-UNDO.

DEFINE OUTPUT PARAMETER vcfile   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vcdir   AS CHARACTER NO-UNDO.


DEFINE VARIABLE f_file AS CHARACTER NO-UNDO.
DEFINE VARIABLE f_dir  AS CHARACTER NO-UNDO.

RUN getdir.w(OUTPUT f_file, OUTPUT f_dir , OUTPUT f_ok).
IF f_ok THEN
DO:
   ASSIGN vcfile = f_file

          vcdir = f_dir.      

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInput C-Win 
PROCEDURE GetInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER OutValue AS CHARACTER.
 DEFINE OUTPUT PARAMETER OutOK AS LOGICAL.
RUN getinput.w(INPUT ttfields02.fieldDataType,INPUT ttfields02.fieldFormat,INPUT "Enter Value",OUTPUT OutValue,OUTPUT OutOK).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rfields C-Win 
PROCEDURE rfields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipDB    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipRecId AS INTEGER    NO-UNDO.

DEFINE VARIABLE hFields AS HANDLE     NO-UNDO.
DEFINE VARIABLE qFields AS HANDLE     NO-UNDO.
EMPTY TEMP-TABLE  ttfields02.
CREATE BUFFER hFields  FOR TABLE ipDB + "._FIELD" BUFFER-NAME "b_FIELD".

CREATE QUERY qFields.
qFields:ADD-BUFFER(hFields).
qFields:QUERY-PREPARE("FOR EACH " + hFields:NAME + " NO-LOCK WHERE " + hFields:NAME + "._file-recid = INT('" + STRING(ipRecId) + "')").
qFields:QUERY-OPEN().

qFields:GET-FIRST().
DO WHILE hFields:AVAILABLE:

   CREATE ttfields02.
   ASSIGN ttfields02.fieldNum       = hFields:RECID
          ttfields02.tableNum       = ipRecId
          ttfields02.fieldOrder     = hFields:BUFFER-FIELD("_order"):BUFFER-VALUE
          ttfields02.fieldName      = hFields:BUFFER-FIELD("_field-name"):BUFFER-VALUE
          ttfields02.fieldMandatory = hFields:BUFFER-FIELD("_mandatory"):BUFFER-VALUE
          ttfields02.fieldDataType  = hFields:BUFFER-FIELD("_Data-type"):BUFFER-VALUE
          ttfields02.fieldFormat    = hFields:BUFFER-FIELD("_format"):BUFFER-VALUE
          ttfields02.fieldLabel     = hFields:BUFFER-FIELD("_label"):BUFFER-VALUE
          ttfields02.fieldDesc      = hFields:BUFFER-FIELD("_desc"):BUFFER-VALUE.

   qFields:GET-NEXT().

END.
qFields:QUERY-CLOSE().

DELETE OBJECT qFields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE savedb C-Win 
PROCEDURE savedb :
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
DEFINE OUTPUT PARAMETER updt_ok AS LOGICAL NO-UNDO.
ASSIGN timenow = TIME.

DEFINE VARIABLE hTable  AS HANDLE     NO-UNDO.
DEFINE VARIABLE qTable  AS HANDLE     NO-UNDO.
DO TRANSACTION ON ERROR UNDO, THROW: 
CREATE BUFFER hTable  FOR TABLE "excelexport".

CREATE QUERY qTable.
qTable:ADD-BUFFER(hTable).
qTable:QUERY-PREPARE("for each excelexport no-lock:").
qTable:QUERY-OPEN().

qTable:GET-FIRST().
DO:
   hTable:BUFFER-CREATE().
   ASSIGN hTable:BUFFER-FIELD("querycopy"):BUFFER-VALUE = ipquery
          hTable:BUFFER-FIELD("filename"):BUFFER-VALUE = ipfile
          hTable:BUFFER-FIELD("tableName"):BUFFER-VALUE = ipTname
          hTable:BUFFER-FIELD("filelocation"):BUFFER-VALUE = ipdir
          hTable:BUFFER-FIELD("fieldnames"):BUFFER-VALUE = ipfields
          hTable:BUFFER-FIELD("Qtime"):BUFFER-VALUE = STRING(timenow,'HH:MM:SS') .
   
   qTable:GET-NEXT().
END.

qTable:QUERY-CLOSE(). 
DELETE OBJECT qTable.
DELETE OBJECT hTable.
END.

IF ERROR-STATUS:ERROR THEN
    MESSAGE "Error updating table: " + ERROR-STATUS:ERROR-STRING.
ELSE DO:
    MESSAGE "Query details saved successfully.." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN  updt_ok = TRUE.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

