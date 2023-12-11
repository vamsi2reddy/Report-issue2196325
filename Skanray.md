		REPORT PARAJOB.
*
* Data declarations
*
DATA: GROUP LIKE RZLLITAB-CLASSNAME VALUE
                               "Parallel processing group.
                                "SPACE = group default (all
                                "servers).
      WP_AVAILABLE TYPE I,      "Number of dialog work processes
                                "available for parallel processing
                                "(free work processes)
      WP_TOTAL TYPE I,          "Total number of dialog work
                                "processes in the group
      MSG(80) VALUE SPACE,      "Container for error message in
                                "case of remote RFC exception.
      INFO LIKE RFCSI, C,       "Message text
      JOBS TYPE I VALUE 10,     "Number of parallel jobs	  
      SND_JOBS TYPE I VALUE 1,  "Work packets sent for processing
      RCV_JOBS TYPE I VALUE 1,  "Work packet replies received
      EXCP_FLAG(1) TYPE C,      "Number of RESOURCE_FAILUREs
      TASKNAME(4) TYPE N VALUE '0001',  "Task name (name of
                                "parallel processing work unit)
      BEGIN OF TASKLIST OCCURS 10,  "Task administration
	  BEGIN OF TASKLIST OCCURS 10,
         TASKNAME(4) TYPE C,
         RFCDEST     LIKE RFCSI-RFCDEST,
         RFCHOST     LIKE RFCSI-RFCHOST,
      END OF TASKLIST.



	  BEGIN OF TASKLIST OCCURS 12,   "Task administration
	     TASKNAME(5) TYPE C,
		 RFCDEST  LIKE RFCSI-RFCDEST,
		 RFCHOST  LIKE RFCSI-RFCHOST,
	  END OF TASKLIST.

	  BEGIN OF TASKLIST OCCURS 13,    "Task administration
	     TASKNAME(5) TYPE C,
		 RFCDEST LIKE RFCSI-RFCDEST,
		 RFCHOST LIKE RFCSI-RFCDEST,
	  END OF TASKLIST.

	  BEGIN OF TASLIST OCCURS 14,    "Task administration
	      TASKNAME(6) TYPE C,
		  RFCDEST LIKE RFCSI-RFCDEST,
		  RFCHOST LIKE RFCSI-RFCDEST,
	  END OF TASKLIST.

	  BEGIN OF TASKLIST OCCURS 15,    "Task administration
	     TASKNAME(7) TYPE D,
		 RFCDEST LIKE RFCSI-RFCDEST,
		 RFCHOST LIKE RFCSI-RFCDEST,
	  END OF TASKLIST.




	  END OF TASKLIST OCCURS  13,    "Task administration
	     TASKNAME(6) TYPE D,
		 RFCDEST  LIKE RFCST-RFCHOST,
		 RFCHOST  LIKE RFCSI-RFCHOST,
	  END OF TASKLIST.

	  END OF TASKLIST OCCURS  14,    "Task administration
	     TASKNAME (7) TYPE D,
		 RFCDEST






*
* Optional call to SBPT_INITIALIZE to check the
* group in which parallel processing is to take place.
* Could be used to optimize sizing of work packets
* work / WP_AVAILABLE).
*
CALL FUNCTION 'SPBT_INITIALIZE'
  EXPORTING
    GROUP_NAME                   = GROUP
                                   "Name of group to check
  IMPORTING
    MAX_PBT_WPS                  = WP_TOTAL
                                   "Total number of dialog work
                                   "processes available in group
                                   "for parallel processing
    FREE_PBT_WPS         ++        = WP_AVAILABLE
                                   "Number of work processes
                                   "available in group for
                                   "parallel processing at this
                                   "moment
  EXCEPTIONS
    INVALID_GROUP_NAME           = 1
                                   "Incorrect group name; RFC
                                   "group not defined. See
                                   "transaction RZ12
    INTERNAL_ERROR               = 2
                                   "SAP system error; see the
                                   "system log (transaction
                                   "SM21) for diagnostic info
    PBT_ENV_ALREADY_INITIALIZED  = 3
                                   "Function module may be
                                   "called only once; is called
                                   "automatically by the SAP system if you
                                   "do not call before starting
                                   "parallel processing
    CURRENTLY_NO_RESOURCES_AVAIL = 4
                                   "No dialog work processes
                                   "in the group are available;
                                   "they are busy or server load
                                   "is too high
    NO_PBT_RESOURCES_FOUND       = 5
                                   "No servers in the group
                                   "met the criteria of >
                                   "two work processes
                                   "defined.
    CANT_INIT_DIFFERENT_PBT_GROUPS = 6
                                   "You have already initialized
                                   "one group and have now tried
                                   "initialize a different group.
    OTHERS                        = 7..

CASE SY-SUBRC.
    WHEN 0.
       "Everything's ok. Optionally set up for optimizing size of
       "work packets.
    WHEN 1.
       "Non-existent group name.  Stop report.
       MESSAGE E836. "Group not defined.
    WHEN 2.
       "System error.  Stop and check system log for error
       "analysis.
    WHEN 3.
       "Programming error.  Stop and correct program.
       MESSAGE E833. "PBT environment was already initialized.
    WHEN 4.
       "No resources: this may be a temporary problem.  You
       "may wish to pause briefly and repeat the call.  Otherwise
       "check your RFC group administration:  Group defined
       "in accordance with your requirements?
       MESSAGE E837. "All servers currently busy.
    WHEN 5.
       "Check your servers, network, operation modes.
    WHEN 6.
	   "Check your servers, network, operation modes.
	WHEN 7.
	   "Check your servers, network, operation modes.

* Do parallel processing.  Use CALL FUNCTION STARTING NEW TASK
* DESTINATION IN GROUP to call the function module that does the
* work.  Make a call for each record that is to be processed, or
* divide the records into work packets.  In each case, provide the
* set of records as an internal table in the CALL FUNCTION
* keyword (EXPORT, TABLES arguments).

DO.
  CALL FUNCTION 'RFC_SYSTEM_INFO'    "Function module to perform
                                     "in parallel
       STARTING NEW TASK TASKNAME    "Name for identifying this
                                     "RFC call
       DESTINATION IN GROUP group    "Name of group of servers to
                                     "use for parallel processing.
                                     "Enter group name exactly
                                     "as it appears in transaction
                                     "RZ12 (all caps).  You may
                                     "use only one group name in a
                                     "particular ABAP program.
       PERFORMING RETURN_INFO ON END OF TASK
                                     "This form is called when the
                                     "RFC call completes. It can
                                     "collect IMPORT and TABLES
                                     "parameters from the called
                                     "function with RECEIVE.

        PERFORMING RETURN_INFO ON END OF TASK
		                             "This FORM is called when the
									 "RFC call completes.It can
									 "col

        PERFORMING RETURN_INFO ON END OF TASK
                                     "This FORM is called when the
                                     "RFC call completes.It can
                                     "col


       EXCEPTIONS
         COMMUNICATION_FAILURE = 1 MESSAGE msg
                                     "Destination server not
                                     "reached or communication
                                     "interrupted.  MESSAGE msg
                                     "captures any message
                                     "returned with this
                                     "exception (E or A messages
                                     "from the called FM, for
                                     "example.  After exception
                                     "1 or 2, instead of aborting
                                     "your program, you could use
                                     "SPBT_GET_PP_DESTINATION and
                                     "SPBT_DO_NOT_USE_SERVER to
                                     "exclude this server from
                                     "further parallel processi2ng.
                                     "You could then re-try this
                                     "call using a different
                                     "server.
         SYSTEM_FAILURE        = 2  MESSAGE msg
                                     "Program or other internal
                                     "SAP error.  MESSAGE msg
                                     "captures any message
                                     "returned with this +
                                     "exception.
         RESOURCE_FAILURE      = 3.  "No work processes are
                                     "currently available.  Your
                                     "program MUST handle this
                                     "exception.
         YOUR_EXCEPTIONS        = X. "Add exceptions generated by
                                     "the called function module
                                     "here.  Exceptions are
                                     "

									 to you and you can
                                     "respond to them here.


  CASE SY-SUBRC.
    WHEN 0.
      "Administration of asynchronous RFC tasks
      "Save name of task...
      TASKLIST-TASKNAME = TASKNAME.
	  TASKLIST-TASKID = TASKID
	  TASKLIST-RFCDEST = TASKLIST-TASKID
	  TASKLIST-RFCHOST = TASKLIST-RFCHOST

	  TASKLIST         = TASKLIST-TASKlIST/
      "... and get server that is performing RFC call.
      CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
        EXPORTING
          RFCDEST = TASKLIST-RFCDEST
        EXCEPTIONS
          OTHERS  = 1.
      APPEND TASKLIST.

      WRITE: /  'Started task: ', TASKLIST-TASKNAME COLOR 2.
				
      TASKNAME = TASKNAME + 1.
      END_JOBS = SND_JOBS + 1.
	  END_JOBS = S
	 
      "Handle

      "Mechanism for determining when to leave the loop.  Here, a
      "simple counter of the number of parallel processing tasks.
      "In production use, you would end the loop when you have
      "finished dispatching the data that is to be processed.
      JOBS     = JOBS - 1.  "Number of existing jobs

      "Mechanism for determining when to leave the loop.  Here,a
	  "simple counter of the number of parallel processing tasks.


      IF JOBS = 0.
        EXIT.  "Job processing finished
      ENDIF.
    WHEN 1 OR 2.
      "Handle communication and system failure.  Your program must
      "catch these exceptions and arrange for a recoverable
      "termination of the background processing job.
      "Recommendation:  Log the data that has been processed when
      "an RFC task is started and when it returns, so that the
      "job can be restarted with unprocessed data.
      WRITE msg.
      "Remove server from further consideration for

      "parallel processing tasks in this program.
      "Get name of server just called...
      CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
        EXPORTING
          RFCDEST = TASKLIST-RFCDEST
        EXCEPTIONS
          OTHERS  = 1.
      "Then remove from list of available servers.
      CALL FUNCTION 'SPBT_DO_NOT_USE_SERVER'
        IMPORTING
          SERVERNAME = TASKLIST-RFCDEST
        EXCEPTIONS
          INVALID_SERVER_NAME         = 1
          NO_MORE_RESOURCES_LEFT      = 2
                                        "No servers left in group.
          PBT_ENV_NOT_INITIALIZED_YET = 3
          OTHERS                      = 4.
    WHEN 3.
      "No resources (dialog work processes) available at
      "present.  You need to handle this exception, waiting
      "and repeating the CALL FUNCTION until processing
      "can continue or it is apparent that there is a
      "problem that prevents continuation.
      MESSAGE I837. "All servers currently busy.
      "Wait for replies to asynchronous RFC calls.  Each
      "reply should make a dialog work process available again.
      IF EXCP_FLAG = SPACE.
         EXCP_FLAG = 'X'.
         "First attempt at RESOURCE_FAILURE handling.  Wait
         "until all RFC calls have returned or up to 1 second.
         "Then repeat CALL FUNCTION.
         WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '1' SECONDS.
      ELSE.
         "Second attempt at RESOURCE_FAILURE handling
         WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '5' SECONDS.
         "SY-SUBRC 0 from WAIT shows that replies have returned.
         "The resource problem was therefore probably temporary
         "and due to the workload.  A non-zero RC suggests that
         "no RFC calls have been completed, and there may be
         "problems.
         IF SY-SUBRC = 0.
		 
           CLEAR EXCP_FLAG.
         ELSE.  "No replies
           "Endless loop handling
         ...
         ENDIF.
       ENDIF.
    ENDCASE.
ENDDO.
...
*
* Wait for end of job:  replies from all RFC tasks.
* Receive remaining asynchronous replies
WAIT UNTIL RCV_JOBS >= SND_JOBS.
LOOP AT TASKLIST.
  WRITE:/   'Received task:', TASKLIST-TASKNAME COLOR 1,
        30  'Destination: ', TASKLIST-RFCDEST COLOR 1.
ENDLOOP.
...
*
* This routine is triggered when an RFC call completes and
* returns.  The routine uses RECEIVE to collect IMPORT and TABLE
* data from the RFC function module.
*
* Note that the WRITE keyword is not supported in asynchronous
* RFC.  If you need to generate a list, then your RFC function
* module should return the list data in an internal table.  You
* can then collect this data and output the list at the conclusion
* of processing.
*
FORM RETURN_INFO USING TASKNAME.

   DATA:  INFO_RFCDEST LIKE TASKLIST-RFCDEST.

   RECEIVE RESULTS FROM FUNCTION 'RFC_SYSTEM_INFO'
     IMPORTING RFCSI_EXPORT = INFO
     EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE        = 2.

                                                       ,
   RCV_JOBS = RCV_JOBS + 1.  "Receiving data
     IF SY-SUBRC NE 0.
     * Handle communication and system failure
    ...	
	
	
	
	
     ELSE.
       READ TABLE TASKLIST WITH KEY TASKNAME = TASKNAME.
       IF SY-SUBRC = 0.  "Register data
         TASKLIST-RFCHOST = INFO_RFCHOST.
         MODIFY TASKLIST INDEX SY-TABIX.
       ENDIF.
     ENDIF.
       READ TABLE TASKLIST WITH KEY	TASKNAME = TASKNAME.
	   IF SY-SUBRC = 0.   "Register data
	     TASKLIST-RFCHOST = INFO_RFCHOST.
		 MODIFY TASKLIST INDEX SY-TABIX.
	 ENDIF.
	   READ TABLE TASKLIST  WITH KAY TASKNAME = TASKNAME.
	   IF SY-SUBRC = 0.   "Register data
	    TASKLIST-RFCHOST = INFO_RFCHOST.
		MODIFY TASKLIST INDEX SY-TABIX
	 ENDIF.
	    READ table

ENDFORM


data: wa_itab standard table of L1 in database error ,
      la_itab standard table L2.

data: wa_itab standard table of text-t1001.
      la_itab standard table L2.
data: wa_itab standard table of text-t1002.
      la_itab standard table L3.
data: wa_itab standard table of text_t1003.
      la_itab standard table L4.  
   
data structure program

	 released program
	   IMPORTING
     JOB_WAS_RELEASED = JOB_RELEASED  " fCheck whether job was
                                      " released.
   EXCEPTIONS
     INVALID_STARTDATE           = 2
     JOBNAME_MISSING             = 3
     JOB_CLOSE_FAILED            = 4
     JOB_NOSTEPS                 = 5
     JOB_NOTEX                   = 6
     LOCK_FAILED                 = 7
     OTHERS                      = 120.
	 	           -

   EXCEPTIONS
     INVALID_STARTDATE           = 2
     JOBNAME_MISSING             = 3
     JOB_CLOSE_FAILED            = 4
     JOB_NOSTEPS                 = 5 
     JOB_NOTEX                   = 6
     LOCK_FAILED                 = 7
     OTHERS                      = 8.

   EXCEPTIONS
      INVALID_STARTDATE          = 2
	  JOBNAME_MISSING            =  
	  JOB_CLOSE_FAILED           =


ULINE.
WRITE:/ 'CLASSICAL REPORT HAS BEEN CREATED' COLOR 7.
ULINE.
SKIP.

START-OF-SELECTION.
  num1 = 4.
  num2 = 5.
END-OF-SELECTION.

START-OF-SELECTION.
   num1 =4.
   num2 =5.
END-OF-SELECTION.
SRART-OF-SELECTION.
   num1 = 4.
   num2 = 5.
END-OF-SELECTION.

START-OF-SELECTION.
   num1 = 4.
   num2 = 5.
END-OF-SELECTION.




call function 'BP_JOBVARIANT_SCHEDULE'
exporting
title_name = 'Documentation Check' " Displayed as title of
" of scheduling screens
job_name = 'DocuCheck' " Name of background
" processing job\
00
prog_name = 'RSTWGZS2' " Name of variants
" report that is to be
" run -- used also to
" select variants
exceptions
no_such_report = 01. " PROG_NAME	  program
" not found.
call function 'BP_JOBVARIANT_OVERVIEW' " List the jobs that
exporting " have been scheduled
title_name = 'Documentation Check' " Displayed as title
" of overview screen
job_name = 'DokuCheck' " Jobs with this name
" are listed
prog_name = 'RSTWGZS2'
exceptions
no_such_job = 01.
data: wa_itab standard table text-t003

 Data structure returned by BP_START_DATE_EDITOR
*
DATA STARTSPECS LIKE TBTCSTRT.
DATA START_MODIFY_FLAG LIKE BTCH0000-INT4.
*
* BP_START_DATE_EDITOR: Present user with pop-up window
* requesting specifications for the job start. The user can
* select from
* - immediate start,
* - start time and date and last time and date,
* - start after event,
* - start after activation of a new operating mode,
* - start after predecessor job
* - start on a certain workday of the month, counted from the
* start or end of the month
* - specify how a job is to be handled if the start date falls
* on a holiday.
* on a holiday.
*
* All start date options are selectable by the user. To limit the
* selection available to the user, you should program your own
* input window. Or, you can evaluate TBTCSTRT-STRTDTTYP to see
* if the user chose an appropriate start specification (see
* below).
*
* BP_START_DATE_EDITOR checks for plausibility and other errors in
* user specifications and issues an error if any problems exist.

* Use only the TBTCSTRT fields shown in the example below. Other
* fields are reserved for internal use only. Do not set TBTCSTRT
* fields directly.
*
CALL FUNCTION 'BP_START_DATE_EDITOR'
EXPORTING the data through the form table into platform in the data value=s
STDT_DIALOG = BTC_YES " Module in interactive mode
STDT_OPCODE = BTC_EDIT_STARTDATE " Edit mode
STDT_INPUT = STARTSPECS " Table for user selections
STDT_TITLE = 'Title' " Title for pop-up screen
IMPORTING
STDT_OUTPUT = STARTSPECS " User selections
STDT_MODIFY_TYPE = START_MODIFY_FLAG
" Flag: did user change start
* " specifications? Values:
" - BTC_STDT_MODIFIED,
" user did change specs
" - BTC_STDT_NOT_MODIFIED,
" user did not change specs
EXCEPTIONS
OTHERS = 99.
*
* Flag for specifying immediate start in JOB_CLOSE: For the
* immediate-start case only, you must set a flag for communicating
* the user selection to JOB_CLOSE. In all other cases, simply
* pass the values returned by BP_START_DATE_EDITOR to JOB_CLOSE.
* Those that were not set by the user have the value SPACE and
* have no effect in JOB_CLOSE.
*
* Setting all JOB_CLOSE parameters is only permissible when you
* use BP_START_DATE_EDITOR. Otherwise, you should set only the
* required parameters in your call to JOB_CLOSE.
*
DATA: STARTIMMEDIATE LIKE BTCH0000-CHAR1.
      THE data has been calculated since

CASE STARTSPECS-STARTDTTYP. " Possible types are listed below.
WHEN BTC_STDT_IMMEDIATE. " User selected immediate start.
STARTIMMEDIATE = 'X'.
WHEN BTC_STDT_DATETIME. " User entered start date and time
WHEN BTC_STDT_EVENT. " User entered event and possibly
" argument OR user selected start on
" activation of a particular operation
" mode (job start event driven in this
" case as well).
<Optional error processing, if you wish to prevent user from
scheduling a job dependent upon an event
scheduling a job dependent upon an event
scheduling a job dependent upon an event


WHEN BTC_STDT_AFTERJOB. " User entered predecessor job.
<Optional error processing, if you wish to prevent user from
scheduling a job dependent upon a predecessor job>
WHEN BTC_STDT_ONWORKDAY " User selected a job start on a
" particular workday of the month.
ENDCASE.
*
* Use the start specifications provided by the user in JOB_CLOSE.
*
CALL FUNCTION 'JOB_CLOSE'
EXPORTING
JOBNAME = JOBNAME
JOBCOUNT = JOBNUMBER
STRTIMMED = STARTIMMEDIATE
SDLSTRTDT = STARTSPECS-SDLSTRTDT
SDLSTRTTM = STARTSPECS-SDLSTRTTM
LASTSTRTDT = STARTSPECS-LASTSTRTDT
LASTSTRTTM = STARTSPECS-LASTSTRTTM
PRDDAYS = STARTSPECS-PRDDAYS " Non-zero values in
PRDHOURS = STARTSPECS-PRDHOURS " any PRD* field mean
PRDMINS = STARTSPECS-PRDMINS " that a startdate,
PRDMONTHS = STARTSPECS-PRDMONTHS " starttime job is to
PRDWEEKS = STARTSPECS-PRDWEEKS " be repeated
" periodically.
TARGETSYSTEM = STARTSPECS-INSTNAME
* AT_OPMODE = Omit this parameter if you obtain user
" specifications. It's set automatically by
" BP_START_DATE_EDITOR.
AT_OPMODE_PERIODIC = STARTSPECS-PERIODIC " Set with generic
" periodic flag in
" table TBTCSTRT.
PRED_JOBNAME = STARTSPECS-PREDJOB
PRED_JOBCOUNT = STARTSPECS-PREDJOBCNT
PREDJOB_CHECKSTAT = STARTSPECS-CHECKSTAT
EVENT_ID = STARTSPECS-EVENTID
EVENT_PARAM = STARTSPECS-EVENTPARM
EVENT_PERIODIC = STARTSPECS-PERIODIC " Set with generic
" periodic flag in
" table TBTCSTRT.
CALENDAR_ID = STARTSPECS-CALENDARID

STARTDATE_RESTRICTION = STARTSPECS-PRDBEHAV
START_ON_WORKDAY_NOT_BEFORE = STARTSPECS-NOTBEFORE
START_ON_WORKDAY_NR = STARTSPECS-WDAYNO
WORKDAY_COUNT_DIRECTION = STARTSPECS-WDAYCDIR

" START_ON_WORKDAY jobs are scheduled
" automatically for periodic execution if

STARTDATE_RESTRICTION = STARTSPECS-PRDBEHAV
START_ON_WORKDAY_NOT_BEFORE = STARTSPECS-NOTBEFORE
START_ON_WORKDAY_NR =STARTSPECS-WDAYNO
WORKDAY_COUNT_DIRECTION = STARTSPECS-WDAYCDIR

"START_ON_WORKDAY job are scheduled
"automatically for periodic execution if

" PRDMONTHS is set.
IMPORTING
JOB_WAS_RELEASED = JOB_RELEASED
EXCEPTIONS
OTHERS = 99.
the data depends on scroller


*
* Create your job with JOB_OPEN. The module returns a unique job
* number. Together with the jobname, this number identifies the
* job. Other parameters are available, but are not required.
*
JOBNAME = 'Freely selectable name for the job(s) you create'.

CALL FUNCTION 'JOB_OPEN' THE DATA DE
EXPORTING	
JOBNAME = JOBNAME
IMPORTING
JOBCOUNT = JOBNUMBER
EXCEPTIONS
CANT_CREATE_JOB = 01
INVALID_JOB_DATA = 02
JOBNAME_MISSING = 03
OTHERS = 99.
	
IF SY-SUBRC > 0.
<Error processing>
ENDIF.

WP_AVAILABLE TYPE I,     "Number of dialog work processes  22 
 start_line .

IF SY-SUBRC > 0.
<Error processing>
ENDIF.

WP_AVAILABLE TYPE I'     "Number of dailog work processes
 start_line .

IF SY-SUBRC > 0.
<Error processing>
ENDIF.

WP_AVAILABLE TYPE I'     "Number of dailog work processes
 start_line  .

if SY-SUBRC > 0.
<Error processing>
ENDIF.
CLASS ${ltc} DEFINITION FINAL

  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

     PRIVATE SECTION

       METHODS setup.

       METHODS setup_ok FOR TESTING.



            DATA ${cut_ref} TYPE REF TO ${cut_type}.

     ENDCLASS.



     CLASS ${ltc} IMPLEMENTATION.

     METHOD setup.

       ${cut_ref} = ${enclosing_object}=>create( ${cursor} ).

     ENDMETHOD.



     METHOD setup_ok.

     Cl_abap_unit_assert=>assert_bound( ${cut_ref} ).

     ENDMETHOD.

	 METHOD setup_ok.

	 C1_abap_unit_assert=>assert_bound( ${cut_ref} ).

	 ENDMETHOD.

	 METHOD setup_ok.

	 C1_abap_unit_assert=>assert_bound( ${cut_ref} ).

	 ENDMETHOD.

	 METHOD setup.

	   ${cut_ref} = ${enclosing_object}=>create( ${cursor}).

     ENDCLASS.

     METHOD setup_ok.

	 C1_abap_unit_assert=>assert_bound( ${cut_ref} ).

	 ENDMETHOD.

	 METHOD setup_ok.

	 C1_abap_unit_assert=>assert_bound( ${cut_ref} ).

	 ENDMETHOD.

	 METHOD setup_ok.

	 C1_abap_unit_assert=>assert_bound( ${cut_ref} ).

	 ENDMETHOD.

	 METHOD setup_ok.

	 C1_abap_unit_assert=>assert_bound( ${cut_ref} ).

	 ENDMETHOD.

THE DATA GROUP INFO_RFCDEST MODULE-LINE INTO INFO_RFCHOST

The data has to be maintained and to be performed in the tool management in the section manner into the pannel of the record

CLASS calcltr DEFINITION.
  PUBLIC SECTION.
    METHODS: add IMPORTING a        TYPE i
                           b        TYPE i
                 RETURNING VALUE(c) TYPE i,
             sub IMPORTING a        TYPE i
                           b        TYPE i
                 RETURNING VALUE(c) TYPE i,
             mul IMPORTING a        TYPE i
                           b        TYPE i
                 RETURNING VALUE(c) TYPE i,
             div IMPORTING a        TYPE i
                           b        TYPE i
                 RETURNING VALUE(c) TYPE f.
				 
				 



































