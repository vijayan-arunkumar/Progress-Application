&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

 DEFINE INPUT PARAMETER ipfilename AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER ipdirname AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER ipfields AS CHARACTER NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
 DEFINE VARIABLE tskName AS CHARACTER NO-UNDO.
 DEFINE VARIABLE outbatName AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mve AS LOGICAL NO-UNDO.
 DEFINE VARIABLE Fl_time AS CHARACTER FORMAT "9(4)" NO-UNDO.  
 DEFINE VARIABLE time1 AS CHARACTER NO-UNDO.
 DEFINE VARIABLE time2 AS CHARACTER NO-UNDO.     
 DEFINE VARIABLE time3 AS INTEGER NO-UNDO.
 DEFINE VARIABLE time4 AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fl_name t-day t-hrs t-mins Cm_tri ~
btn_create-2 Btn_cancel 
&Scoped-Define DISPLAYED-OBJECTS Fl_name t-day t-hrs t-mins Cm_tri fl_rr ~
fl_Rm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetTimeStatus C-Win 
FUNCTION GetTimeStatus RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_cancel AUTO-END-KEY  NO-CONVERT-3D-COLORS
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_create-2 
     LABEL "Create" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Cm_tri AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ONCE","DAILY","WEEKLY","MONTHLY" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE t-day AS CHARACTER FORMAT "X(256)" INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "AM","PM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE Fl_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Use format~"Folder\task Name ~" to create task under specific folder" NO-UNDO.

DEFINE VARIABLE fl_Rm AS CHARACTER FORMAT "99":U 
     LABEL "Day of Every Month" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE fl_rr AS CHARACTER FORMAT "99":U INITIAL "1" 
     LABEL "Recur Every" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE t-hrs AS CHARACTER FORMAT "9(2)":U 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY .95 TOOLTIP "Enter hour" NO-UNDO.

DEFINE VARIABLE t-mins AS CHARACTER FORMAT "9(2)":U 
     VIEW-AS FILL-IN 
     SIZE 5.2 BY .95 TOOLTIP "Enter minutes" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Fl_name AT ROW 2.95 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     t-day AT ROW 4.76 COL 40.2 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     t-hrs AT ROW 4.81 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     t-mins AT ROW 4.81 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     Cm_tri AT ROW 6.62 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fl_rr AT ROW 6.67 COL 63 COLON-ALIGNED WIDGET-ID 20
     fl_Rm AT ROW 8.57 COL 34 COLON-ALIGNED WIDGET-ID 22
     btn_create-2 AT ROW 10.48 COL 16 WIDGET-ID 60
     Btn_cancel AT ROW 10.48 COL 43 WIDGET-ID 34
     "Task Time :" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 4.81 COL 8 WIDGET-ID 12
     "Task Name" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 2.91 COL 8 WIDGET-ID 10
     ":" VIEW-AS TEXT
          SIZE 1.2 BY .95 AT ROW 4.81 COL 33 WIDGET-ID 54
     "Task Trigger :" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 6.71 COL 8 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 3
         DEFAULT-BUTTON Btn_cancel CANCEL-BUTTON Btn_cancel WIDGET-ID 100.


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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Schedule Tasks in  Task Scheduler"
         COLUMN             = 61.6
         ROW                = 13.48
         HEIGHT             = 11.57
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fl_Rm IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fl_rr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Schedule Tasks in  Task Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Schedule Tasks in  Task Scheduler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_cancel C-Win
ON CHOOSE OF Btn_cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_create-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_create-2 C-Win
ON CHOOSE OF btn_create-2 IN FRAME DEFAULT-FRAME /* Create */
DO:
    APPLY "leave" TO Fl_name.
    APPLY "leave" TO t-hrs.
    APPLY "leave" TO t-mins.
    APPLY "leave" TO fl_rr.
    APPLY "leave" TO fl_Rm.
    
    IF Fl_name:SCREEN-VALUE = "" OR Fl_name:SCREEN-VALUE = ?  THEN
    DO:
        MESSAGE "Please Enter Task Name." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO Fl_name.
        RETURN.           
    END.
    APPLY "entry" TO t-hrs.
    IF t-hrs = "" OR t-hrs = ? OR t-hrs = "00"  THEN
    DO:
        MESSAGE "Please Enter Task Hour." VIEW-AS ALERT-BOX ERROR .
        APPLY "entry" TO t-hrs. 
        RETURN.      
    END.
    
    ELSE IF t-mins = "" OR t-mins = ? THEN
    DO:
        MESSAGE "Please Enter Task Minute." VIEW-AS ALERT-BOX ERROR .
        APPLY "entry" TO t-mins. 
        RETURN.      
    END.
      
    ELSE IF Fl_time = "" OR Fl_time = ?  THEN
    DO:
        MESSAGE "Please Enter Task Time." VIEW-AS ALERT-BOX ERROR .
        APPLY "entry" TO t-hrs. 
        RETURN.
    END.
       
    IF t-day:SCREEN-VALUE = "AM" AND integer(Fl_time) = 1200 THEN
    DO:
    ASSIGN time1 = Fl_time.
    ASSIGN time4 = SUBSTRING(STRING(time1), 3,2) .
    ASSIGN Fl_time = "00" + time4.
    END.
    
    IF t-day:SCREEN-VALUE = "PM" AND Fl_time < "1200" THEN
    DO:
      ASSIGN time1 = Fl_time.
      ASSIGN time2 = SUBSTRING(STRING(time1), 1, 2) .
      ASSIGN time4 = SUBSTRING(STRING(time1), 3,2) .
      ASSIGN time3 = INTEGER(time2) + 12.
      IF time3 > 23 THEN
      DO:
       ASSIGN Fl_time = "00" + time4.
      END.
       ASSIGN Fl_time = STRING(time3) + time4.
    END.
    
    ELSE IF Cm_tri:SCREEN-VALUE = "" OR Cm_tri:SCREEN-VALUE = ?  THEN
    DO:
        MESSAGE "Please Select Any Task Trigger." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "entry" TO Cm_tri.
        RETURN.
    END.
    
    ELSE IF Cm_tri:SCREEN-VALUE = "ONCE" AND GetTimeStatus()  THEN
    DO:
        MESSAGE "Please Enter Valid Time" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "entry" TO t-hrs.
        RETURN.
    END.
    
    ELSE IF Cm_tri:SCREEN-VALUE = "MONTHLY" AND fl_Rm:SCREEN-VALUE = ""  THEN
    DO:
        MESSAGE "Please Enter Day of Every Month" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "entry" TO fl_Rm.
        RETURN.
    END.
    
    ELSE IF (Cm_tri:SCREEN-VALUE = "WEEKLY" OR Cm_tri:SCREEN-VALUE = "DAILY") AND fl_rr:SCREEN-VALUE = ""  THEN
    DO:
        MESSAGE "Please Enter Recur Every" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "entry" TO fl_rr.
        RETURN.
    END.
    
    ASSIGN tskName = Fl_name:SCREEN-VALUE.
    IF tskName <>'' AND tskName <>? THEN
    DO:
      RUN createBatFile(INPUT tskName,OUTPUT mve,OUTPUT outbatName).
      IF mve THEN
      DO:
      RUN createTask(INPUT tskName,INPUT outbatName).    
      END.
      APPLY "CLOSE" TO THIS-PROCEDURE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cm_tri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cm_tri C-Win
ON VALUE-CHANGED OF Cm_tri IN FRAME DEFAULT-FRAME
DO:
  ASSIGN Cm_tri = Cm_tri:SCREEN-VALUE.
  IF Cm_tri:SCREEN-VALUE = "DAILY" THEN
  DO:
     ENABLE fl_rr WITH FRAME {&FRAME-NAME}.
     DISABLE fl_Rm WITH FRAME {&FRAME-NAME}. 
  END.
  ELSE IF Cm_tri:SCREEN-VALUE = "ONCE" THEN
  DO:
   
     DISABLE fl_Rm fl_rr WITH FRAME {&FRAME-NAME}.   
  END.
  ELSE IF Cm_tri:SCREEN-VALUE = "WEEKLY" THEN
  DO:
      ENABLE fl_rr WITH FRAME {&FRAME-NAME}.
      DISABLE fl_Rm WITH FRAME {&FRAME-NAME}. 
  END.
  ELSE 
  DO:
     DISABLE fl_rr WITH FRAME {&FRAME-NAME}.
     ENABLE fl_Rm WITH FRAME {&FRAME-NAME}. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fl_name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fl_name C-Win
ON LEAVE OF Fl_name IN FRAME DEFAULT-FRAME
DO:
  ASSIGN Fl_name = Fl_name:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fl_Rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl_Rm C-Win
ON LEAVE OF fl_Rm IN FRAME DEFAULT-FRAME /* Day of Every Month */
DO:
   ASSIGN fl_Rm = fl_Rm:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fl_rr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fl_rr C-Win
ON LEAVE OF fl_rr IN FRAME DEFAULT-FRAME /* Recur Every */
DO:
   ASSIGN fl_rr = fl_rr:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-day
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-day C-Win
ON VALUE-CHANGED OF t-day IN FRAME DEFAULT-FRAME
DO:
  ASSIGN t-day = t-day:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-hrs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-hrs C-Win
ON LEAVE OF t-hrs IN FRAME DEFAULT-FRAME
DO:
 ASSIGN t-hrs = t-hrs:SCREEN-VALUE.
 IF INTEGER(t-hrs) < 1 OR INTEGER(t-hrs) > 12 THEN
  DO:
    MESSAGE 'please enter correct format "12 hours" ' VIEW-AS ALERT-BOX ERROR BUTTON OK.
    ASSIGN t-hrs = "" .
    DISPLAY t-hrs WITH FRAME default-frame.
    APPLY "entry":U TO t-hrs.
    RETURN.
  END.
 ELSE DO:
 IF INTEGER(t-hrs) < 10 THEN
    DO:
      ASSIGN t-hrs = STRING(INTEGER(t-hrs), "99").
      DISPLAY t-hrs WITH FRAME default-frame.     
    END.
 END. 
 ASSIGN Fl_time = t-hrs.
 APPLY "entry" TO t-mins.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-mins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-mins C-Win
ON LEAVE OF t-mins IN FRAME DEFAULT-FRAME
DO:
 ASSIGN t-mins = t-mins:SCREEN-VALUE.
 IF INTEGER(t-mins) > 59 THEN
  DO:
    MESSAGE 'please enter correct format "01-59 minutes" ' VIEW-AS ALERT-BOX ERROR BUTTON OK.
    ASSIGN t-mins = "" .
    DISPLAY t-mins WITH FRAME default-frame.
    APPLY "entry" TO t-hrs.
    RETURN.
  END.
 ELSE DO:
 IF INTEGER(t-mins) < 10 THEN
    DO:
      ASSIGN t-mins = STRING(INTEGER(t-mins), "99").
      DISPLAY t-mins WITH FRAME default-frame.
      
    END.
 END.
 ASSIGN Fl_time = Fl_time + t-mins.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createBatFile C-Win 
PROCEDURE createBatFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iptskNmae AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER mve AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER outbatName AS CHARACTER.
DEFINE VARIABLE batFileHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE batFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE batFolder AS CHARACTER NO-UNDO.
DEFINE VARIABLE fileExtension AS CHARACTER NO-UNDO.

ASSIGN batFolder = "C:\OpenEdge\WRK\Batfiles"
       batFileName = iptskNmae
       fileExtension = ".bat". 
/* Get the user-specified .bat file name */
batFileName = batFolder + "\" + batFileName + fileExtension.

/* Create the .bat file */         
OUTPUT TO VALUE(batFileName).
PUT 'echo off' SKIP.
PUT 'echo "++++++++++++++++++++++++++++++"' SKIP.
PUT UNFORMATTED 'echo "' iptskNmae + fileExtension '" ' SKIP.
PUT 'echo "______________________________"' SKIP.
PUT 'SET ProWin=C:\Progress\OpenEdge\bin\prowin.exe' SKIP.
PUT 'SET IniPath=C:\Progress\OpenEdge\bin\progress.ini' SKIP.
PUT UNFORMATTED 'SET params1='ipfilename SKIP.
PUT UNFORMATTED 'SET params2='ipfields SKIP.
PUT 'SET ParamPath=C:\ProgressApp\parmfile.pf' SKIP.
PUT 'SET ProgramPath=C:\ProgressApp\BatchProcessCsv.R' SKIP.
PUT 'call %ProWin% -basekey INI -ininame %IniPath% -p %ProgramPath% -pf %ParamPath% -T %temp% -debugalert -b' SKIP.
PUT 'exit' SKIP.
OUTPUT CLOSE.
ASSIGN mve = TRUE
outbatName = batFileName .
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTask C-Win 
PROCEDURE CreateTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER intskName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER inbatName AS CHARACTER NO-UNDO.



DEFINE VARIABLE cCmd   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tCmd   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tTime  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tTrgr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tRecur AS CHARACTER NO-UNDO.
DEFINE VARIABLE Oserror AS INTEGER NO-UNDO.

DEFINE VARIABLE batName AS CHARACTER NO-UNDO.
ASSIGN batName =inbatName.

ASSIGN tName = SUBSTITUTE('"&1"',intskName)
       tTime = SUBSTRING(Fl_time, 1, 2) + ":" + SUBSTRING(Fl_time, 3, 2)
       tTrgr = Cm_tri
       tCmd = SUBSTITUTE('"&1"',batName).


CASE Cm_tri:
  WHEN  "ONCE" THEN
     ASSIGN tRecur = "".
  WHEN  "DAILY" THEN
     ASSIGN tRecur = " /mo " + fl_rr.
  WHEN  "WEEKLY" THEN
     ASSIGN tRecur = "".
  WHEN  "MONTHLY" THEN
     ASSIGN tRecur = " /d " + fl_Rm.
END CASE.

ASSIGN cCmd = SUBSTITUTE("schtasks /create /tn &1 /tr &2 /sc &3 /st &4 &5",tName,tCmd,tTrgr,tTime,tRecur).

OS-COMMAND SILENT VALUE(cCmd).
ASSIGN Oserror = OS-ERROR.
IF Oserror = 0 THEN
DO:
  MESSAGE "Task Created Successfully" VIEW-AS  ALERT-BOX INFO BUTTON OK.  
END.
ELSE 
  MESSAGE "Failed to Create Task" VIEW-AS ALERT-BOX ERROR BUTTONS OK .
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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
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
  DISPLAY Fl_name t-day t-hrs t-mins Cm_tri fl_rr fl_Rm 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Fl_name t-day t-hrs t-mins Cm_tri btn_create-2 Btn_cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetTimeStatus C-Win 
FUNCTION GetTimeStatus RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE crnttime AS CHARACTER NO-UNDO.
   ASSIGN crnttime = STRING(TIME,"HH:MM")
          crnttime = REPLACE(crnttime,":","").
    IF INTEGER(Fl_time) < INTEGER(crnttime) THEN DO:
      RETURN TRUE.
    END.
    ELSE
       RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

