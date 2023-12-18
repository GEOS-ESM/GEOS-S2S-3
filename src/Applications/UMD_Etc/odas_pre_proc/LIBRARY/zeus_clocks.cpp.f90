!DIR$ ID "%Z%	$RCSfile: zeus_clocks.cpp.f90,v $ %Y% $Date: 2023/12/05 20:23:02 $ $Revision: 1.1 $"



   MODULE ZEUS_CLOCKS
    implicit none

   !*---------------------------------------------------------------
   !*-
   !*-- Zeus is a set of functions for time management
   !*-- It consists of an array of clocks, each clock consists
   !*-- of an array of alarms. Functions are used to manipulate
   !*-- the alarms of a clock. The julian calendar is used
   !*-- and all alarms are sticky (i.e. stay ringing till switched off
   !*-- by default).
   !*--------------------------------------------------------------


   !*-- Set and get global error options

   public :: Z_SET_OPTIONS, Z_GET_OPTIONS





   !*-- Time types

   public :: T_Time &! The fundamental time type
   , T_DateTime &! a derived type with hour,minute,sec,etc.
   , T_TimeInterval &! a time interval
   , T_DateTimeInterval




   !*-- The T_DateTimeInterval type is a special time interval
   ! that is mostly useful for incrementing times by months
   ! or years. It has components for second,minute,hour,day
   ! but there is no significant difference between a
   ! DateTimeInterval of 1.5 days and (86400 * 1.5) seconds
   ! The special behavior comes when the datetimeinterval
   ! has a non-zero month or year component. In these cases
   ! the time is incremented in units of months and/or years



   !*-- Time inquiry and casting properties (All operate on T_Time or T_DateTime)

   public :: Z_YEAR, Z_MONTH, Z_DAY ,&! extract year, month, day from time
             Z_HOUR, Z_MINUTE,Z_SECOND ,&! extract hour, minute and second
             Z_DAY_AD,Z_DATE_AD ,&! Identical to Z_DAY_AD
             Z_SECOND_AD ,&! Present time days of modern era
             Z_JULIAN_DAY,Z_JULIAN_DATE,&! Identical to Z_JULIAN_DAY
             Z_JULIAN_SECOND ,&! time in seconds
             Z_SEC_OF_DAY ,&! fractional part of a date * 86400
             Z_DAY_OF_YEAR ,&! Which day of the year
             Z_DAYSINM ! Number of days in a month

   !*-- Convert to Time types:

   public :: AS_TIME, AS_DATETIME, AS_SECONDS, &
             AS_TIMEINTERVAL, AS_DAYS

   !*-- Time operators and functions


   public :: OPERATOR(+), OPERATOR(-), OPERATOR(==)
   public :: OPERATOR(*), OPERATOR(/)

   ! Operators are defined for:
   ! T_time = T_datetime
   ! T_time = T_time + T_TimeInterval
   ! T_time = T_time - T_TimeInterval
   ! T_time = T_time + T_DateTimeInterval
   ! T_time = T_time - T_DateTimeInterval
   ! T_time = T_timeInterval + T_Time
   ! T_time = T_dateTimeInterval + T_Time

   ! T_datetime = T_time
   ! T_datetime = T_datetime + T_TimeInterval
   ! T_datetime = T_datetime - T_TimeInterval
   ! T_datetime = T_TimeInterval + T_datetime
   ! T_datetime = T_datetime + T_DateTimeInterval
   ! T_datetime = T_datetime - T_DateTimeInterval
   ! T_datetime = T_DateTimeInterval + T_datetime

   ! You can not assign a number to a time (this is a private
   ! function for this module only)

   ! A time interval is the difference between two times.
   ! It may also be assgned to and from a number giving the seconds
   ! Wherever you can use a T_TimeInterval, you may use a
   ! numeric representation of the interval in seconds as a
   ! real*8, real*4 or default integer value. ("number" below):
   !
   ! T_timeInterval = T_time - T_time
   ! T_timeInterval = T_datetime - T_datetime
   ! T_timeInterval = number
   ! number = T_timeInterval
   ! T_timeInterval = T_timeInterval + T_TimeInterval
   ! T_timeInterval = T_timeInterval - T_TimeInterval
   ! T_timeInterval = T_timeInterval + number
   ! T_timeInterval = number + T_timeInterval


   ! You may also multiply and divide time intervals by numeric values
   ! T_timeInterval = T_timeinterval * number
   ! T_timeInterval = T_timeInterval / number


   !*-- Other Time utilities

   public :: CAL_MONTH &! Calendar month of a day of year
   , Z_DAT2DMY &! obtain year, month,day,hour,minute,second
   , Z_DATFDMY &! time from year,month,day,hour,minute,second
   , JULIAN_COMPUTE ! Get Julian day from year,month...






   !*-- CLOCKS

   public :: T_Clock &! The clock type
   , T_Alarm_Data ! (Used by clock to hold alarms)

   !*-- Clock methods

   public :: Z_INIT_CLOCK &! Request a clock
   , Z_TICK &! Advance clock time
   , Z_SET_TIME &! Set Date and time of clock
   , Z_SET_INTERVAL &! Set size of TICK
   , Z_FREE_CLOCK ! Free up allocation and pointers when done

   !*-- Clock properties

   public :: Z_TIME, &! Present time of clock
             Z_DATETIME, &! Present time of clock (in datetime)
             Z_INTERVAL, &! Size of TICK
             Z_NALARMS ! Number of alarms allocated for this clock

   ! The clock has a TIME and an INTERVAL (among other things)
   ! Inquiry functions on T_Clock variables are overloaded
   ! with the inquiry functions on T_Time or T_TimeInterval
   ! variables, except that ambiguous entries apply to the
   ! TIME, not the INTERVAL:
   !
   ! The following apply to clock%time:
   ! as_time,as_datetime,
   ! Z_year,z_month,z_day,z_hour,z_minute,z_second,
   ! z_day_ad,z_date_ad,z_second_ad
   ! z_julian_day,z_julian_date,z_julian_second
   ! z_sec_of_day,z_day_of_year,z_daysinm

   ! The following apply to clock%interval
   ! as_TimeInterval

   ! The function AS_SECONDS seems ambiguous, so it is not available
   ! for clocks

   ! The operator = is assigned for
   ! T_time = T_clock ( result is clock "now" time )
   ! T_dateTime = T_clock ( result is clock "now" time )
   ! T_timeInterval = T_clock ( result is clock interval )

   ! The operator - is assigned for
   ! T_TimeInterval = T_clock - T_time
   ! T_TimeInterval = T_clock - T_dateTime
   ! T_TimeInterval = T_clock - T_clock

   ! You can not increment a clock by adding a timeInterval to it
   ! use Z_TICK for this if you want it to poll the alarms,
   ! or use z_SET_TIME( clock, as_time(clock) + timeInterval )
   ! if you want to skip to a new time without ringing the alarms
   ! Note that the difference between two clocks is just the time interval
   ! not any information about increments or alarms.


   !*-- ALARMS

   public :: T_Alarm


   !*-- Alarm methods

   public :: Z_INIT_ALARM , &! Request and initialize an alarm
             Z_SET_ALARM , &! Set origin and interval for alarm
             Z_SHUT_ALARM , &! stop an alarm from ringing
             Z_SET_ALARM_RINGING , &! Force an alarm to ring
             Z_RINGING , &! Test to see if an alarm is ringing
             Z_GET_STATE , &! Get restart info
             Z_RESET_STATE , &! restore info from restart
             Z_FREE_ALARM

   ! overloaded with clock:
   ! z_set_time specify origin for ring calculations
   ! z_set_interval specify interval between rings


   ! As with the clocks, operations which involve time or dateTime
   ! types and alarms apply to the alarm ORIGIN. operations which
   ! involve timeInterval types apply to the alarm INTERVAL.


   ! The following apply to alarm%origin:
   ! as_time,as_datetime,
   ! Z_year,z_month,z_day,z_hour,z_minute,z_second,
   ! z_day_ad,z_date_ad,z_second_ad
   ! z_julian_day,z_julian_date,z_julian_second
   ! z_sec_of_day,z_day_of_year,z_daysinm

   ! The following apply to alarm%interval
   ! as_TimeInterval




   !*-- Character conversion functions

   public :: Z_PRINT_CLOCK &! Print a bunch of stuff
   , CZ_TIME &! Time 'hh:mm:ss'
   , CZ_DATE &! Date as mmm dd, yyyy
   , CZ_MONTH &! Month as mmm
   , CZ_TIMELINE




   !*-- public Error codes

    public :: zcBAD_CLOCKID,zcBAD_ALARM,zcNO_ERROR

    integer, parameter :: zcNO_ERROR = 0
    integer, parameter :: zcBAD_CLOCKID = 10
    integer, parameter :: zcBAD_ALARM = 30

   !*-- public constants

    public :: zcSEC_PER_DAY, zcSEC_PER_MIN, zcSEC_PER_HOUR

    integer, parameter :: zcSEC_PER_MIN = 60
    integer, parameter :: zcSEC_PER_HOUR = 3600
    integer, parameter :: zcSEC_PER_DAY = 86400





   integer, dimension(12) :: days = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   integer, dimension(12) :: daysl = (/31,29,31,30,31,30,31,31,30,31,30,31/)

   public :: Month_Name

   character*3, parameter, dimension(12) :: &
     month_name=(/'Jan','Feb','Mar','Apr','May','Jun',&
                  'Jul','Aug','Sep','Oct','Nov','Dec'/)

   TYPE T_time
     PRIVATE
     real (kind=8) :: value !intern. type for handling time
   END TYPE

   TYPE T_TimeInterval
     PRIVATE
     real (kind=8) :: value !intern. type for handling time
   END TYPE

   TYPE T_Datetime
    integer :: year, month, day, hour, minute
    real :: second
   END TYPE


   TYPE T_DatetimeInterval
    real :: years, months, days, hours, minutes
    real :: seconds
   END TYPE

   integer, parameter :: MAX_NAME_LEN = 60

   TYPE T_Alarm_data
    PRIVATE
    type (T_time) :: origin ! origin of alarm
    type (T_TimeInterval) :: interval ! interval of alarm
    logical :: ring ! ringing status of an alarm
    logical :: taken ! used flag
    logical :: sticky ! TRUE if alarms stays on until
                                           ! Z_SHUT_ALARM
    character(LEN=MAX_NAME_LEN) :: name
   END TYPE

   TYPE T_Clock
    PRIVATE
    type (T_TimeInterval) :: interval ! current clock increment
    type (T_time) :: time ! current clock time in seconds
    type (T_Alarm_data),pointer :: alarms(:) ! list of alarms
   END TYPE

   TYPE T_Alarm
    PRIVATE
    integer :: i ! integer handle of alarm
    type (T_Clock), pointer :: clock ! pointer to clock associated with alarm
   END TYPE






   !*-- private data





   real , parameter :: DAYS_IN_TROPICAL_YEAR = 365.24219
   real (kind=8), parameter :: JULIAN_OFFSET = 1721423.5

   logical,save :: verbose = .TRUE. ! flag for verbose messages
   logical,save :: abort_on_error = .TRUE. ! flag for aborting
   integer,save :: emu = 0 ! error message unit
   logical,save :: origin_j = .TRUE. ! is origin the Julian calendar
   real, save :: accuracy_factor = .001 ! Test equality to millisecs





   interface assignment(=)
    module procedure interval_to_real4
    module procedure interval_to_real8
    module procedure interval_to_int
    module procedure real4_to_interval
    module procedure real8_to_interval
    module procedure int_to_interval
    module procedure time_to_datetime
    module procedure seconds_to_time_real4
    module procedure seconds_to_time_real8
    module procedure seconds_to_time_int
    module procedure time_to_seconds_real4
    module procedure time_to_seconds_real8
    module procedure time_to_seconds_int
    module procedure datetime_to_time
    module procedure datetime_to_seconds_real4
    module procedure datetime_to_seconds_real8
    module procedure datetime_to_seconds_int
    module procedure seconds_to_datetime_real4
    module procedure seconds_to_datetime_real8
    module procedure seconds_to_datetime_int
    module procedure clock_to_time
    module procedure clock_to_datetime
   end interface


   ! We do not allow users to manipulate time into seconds, but
   ! we do it internally.


   PRIVATE :: seconds_to_time_real4, &
              seconds_to_time_real8, &
              seconds_to_time_int, &
              time_to_seconds_real4, &
              time_to_seconds_real8, &
              time_to_seconds_int, &
              datetime_to_seconds_real4, &
              datetime_to_seconds_real8, &
              datetime_to_seconds_int, &
              seconds_to_datetime_real4, &
              seconds_to_datetime_real8, &
              seconds_to_datetime_int



   ! users may switch between interval and seconds.

   PUBLIC :: interval_to_real4, &
              interval_to_real8, &
              interval_to_int, &
              real8_to_interval, &
              real4_to_interval, &
              int_to_interval

   ! Users may interchange between datetime and time

   PUBLIC :: datetime_to_time, time_to_datetime , clock_to_time, &
              clock_to_datetime


   interface operator(+)
    module procedure add__time_interval
    module procedure add__time_dinterval
    module procedure add__interval_time
    module procedure add__dinterval_time
    module procedure add__interval_interval
    module procedure add__time_secondsR4
    module procedure add__time_secondsR8
    module procedure add__time_secondsI
    module procedure add__secondsR4_time
    module procedure add__secondsR8_time
    module procedure add__secondsI_time
    module procedure add__datetime_interval
    module procedure add__datetime_dinterval
    module procedure add__interval_datetime
    module procedure add__dinterval_datetime
    module procedure add__datetime_secondsR4
    module procedure add__datetime_secondsR8
    module procedure add__datetime_secondsI
    module procedure add__secondsR4_datetime
    module procedure add__secondsR8_datetime
    module procedure add__secondsI_datetime
   end interface

   public :: add__time_interval, &
              add__time_dinterval, &
              add__interval_time, &
              add__dinterval_time, &
              add__interval_interval,&
              add__time_secondsR4, &
              add__time_secondsR8, &
              add__time_secondsI, &
              add__secondsR4_time, &
              add__secondsR8_time, &
              add__secondsI_time, &
              add__datetime_interval,&
              add__datetime_dinterval,&
              add__interval_datetime,&
              add__dinterval_datetime,&
              add__datetime_secondsR4, &
              add__datetime_secondsR8, &
              add__datetime_secondsI, &
              add__secondsR4_datetime, &
              add__secondsR8_datetime, &
              add__secondsI_datetime

   interface operator(-)
    module procedure sub__time_time ! interval
    module procedure sub__time_interval ! time
    module procedure sub__time_dinterval ! time
    module procedure sub__time_secondsR4 ! time
    module procedure sub__time_secondsR8 ! time
    module procedure sub__time_secondsI ! time
    module procedure sub__interval_interval ! interval
    module procedure sub__interval_secondsR4 ! interval
    module procedure sub__interval_secondsR8 ! interval
    module procedure sub__interval_secondsI ! interval
    module procedure sub__datetime_datetime ! interval
    module procedure sub__datetime_interval ! time
    module procedure sub__datetime_dinterval ! time
    module procedure sub__datetime_secondsR4
    module procedure sub__datetime_secondsR8
    module procedure sub__datetime_secondsI
    module procedure sub__datetime_time
    module procedure sub__time_datetime
    module procedure sub__clock_time
    module procedure sub__clock_datetime
    module procedure sub__clock_clock
   end interface

   public :: sub__time_time, &
             sub__time_interval, &
             sub__time_dinterval, &
             sub__time_secondsR4, &
             sub__time_secondsR8, &
             sub__time_secondsI, &
             sub__interval_interval, &
             sub__interval_secondsR4, &
             sub__interval_secondsR8, &
             sub__interval_secondsI, &
             sub__datetime_datetime, &
             sub__datetime_interval, &
             sub__datetime_dinterval, &
             sub__datetime_secondsR4, &
             sub__datetime_secondsR8, &
             sub__datetime_secondsI, &
             sub__datetime_time, &
             sub__time_datetime, &
             sub__clock_time, &
             sub__clock_datetime, &
             sub__clock_clock

   interface operator(*)
    module procedure mult__interval_real4
    module procedure mult__interval_real8
    module procedure mult__interval_int
    module procedure mult__real4_interval
    module procedure mult__real8_interval
    module procedure mult__int_interval
    module procedure mult__dinterval_real4
    module procedure mult__dinterval_real8
    module procedure mult__dinterval_int
    module procedure mult__real4_dinterval
    module procedure mult__real8_dinterval
    module procedure mult__int_dinterval
   end interface

   public :: mult__interval_real4, &
             mult__interval_real8, &
             mult__interval_int , &
             mult__real4_interval, &
             mult__real8_interval, &
             mult__int_interval
   public :: mult__dinterval_real4, &
             mult__dinterval_real8, &
             mult__dinterval_int , &
             mult__real4_dinterval, &
             mult__real8_dinterval, &
             mult__int_dinterval

   interface operator(/)
    module procedure div__interval_real4
    module procedure div__interval_real8
    module procedure div__interval_int
    module procedure div__dinterval_real4
    module procedure div__dinterval_real8
    module procedure div__dinterval_int
   end interface

   public :: div__interval_real4, &
             div__interval_real8, &
             div__interval_int

   public :: div__dinterval_real4, &
             div__dinterval_real8, &
             div__dinterval_int

   interface operator(==)
    module procedure eql__time_time
    module procedure eql__time_datetime
    module procedure eql__datetime_time
    module procedure eql__datetime_datetime
    module procedure eql__interval_interval
    module procedure eql__dti_dti
   end interface

   public :: eql__time_time, &
             eql__time_datetime, &
             eql__datetime_time, &
             eql__datetime_datetime, &
             eql__interval_interval,&
             eql__dti_dti





   interface mod_times
    module procedure mod__interval_interval
    module procedure mod__interval_secondsR4
    module procedure mod__interval_secondsR8
    module procedure mod__interval_secondsI
    module procedure mod__time_interval
    module procedure mod__time_secondsR4
    module procedure mod__time_secondsR8
    module procedure mod__time_secondsI
    module procedure mod__datetime_interval
    module procedure mod__datetime_secondsR4
    module procedure mod__datetime_secondsR8
    module procedure mod__datetime_secondsI
   end interface


   public :: mod__interval_interval, &
             mod__interval_secondsR4, &
             mod__interval_secondsR8, &
             mod__interval_secondsI, &
             mod__time_interval, &
             mod__time_secondsR4, &
             mod__time_secondsR8, &
             mod__time_secondsI, &
             mod__datetime_interval, &
             mod__datetime_secondsR4, &
             mod__datetime_secondsR8, &
             mod__datetime_secondsI

   interface z_julian_day
    module procedure z_julian_day_time
    module procedure z_julian_day_datetime
    module procedure z_julian_day_6int
    module procedure z_julian_day_5int
    module procedure z_julian_day_6real
    module procedure z_julian_day_clock
    module procedure z_julian_day_alarm
   end interface

   interface z_julian_date
    module procedure z_julian_day_time
    module procedure z_julian_day_datetime
    module procedure z_julian_day_6int
    module procedure z_julian_day_5int
    module procedure z_julian_day_6real
    module procedure z_julian_day_clock
    module procedure z_julian_day_alarm
   end interface

   interface julian_compute
    module procedure julian_compute_6int
    module procedure julian_compute_5int
    module procedure julian_compute_6real
   end interface

   interface z_julian_second
    module procedure z_julian_second_time
    module procedure z_julian_second_datetime
    module procedure z_julian_second_clock
    module procedure z_julian_second_alarm
    module procedure z_julian_second_6int
    module procedure z_julian_second_5int
    module procedure z_julian_second_6real
   end interface

   interface z_daysinm
    module procedure z_daysinm_mmyy
    module procedure z_daysinm_time
    module procedure z_daysinm_datetime
    module procedure z_daysinm_clock
    module procedure z_daysinm_alarm
   end interface

   interface as_time
    module procedure as_time_time
    module procedure as_time_datetime
    module procedure as_time_6int
    module procedure as_time_5int
    module procedure as_time_6real
    module procedure as_time_int
    module procedure as_time_real4
    module procedure as_time_real8
    module procedure z_time_clock
    module procedure z_alarm_origin
   end interface

   interface as_timeInterval
    module procedure as_timeInterval_int
    module procedure as_timeInterval_real4
    module procedure as_timeInterval_real8
    module procedure z_interval_clock
    module procedure z_alarm_interval
   end interface

   interface as_datetime
    module procedure as_datetime_time
    module procedure as_datetime_datetime
    module procedure as_datetime_6int
    module procedure as_datetime_5int
    module procedure as_datetime_6real
    module procedure as_datetime_int4
    module procedure as_datetime_int8
    module procedure as_datetime_real4
    module procedure as_datetime_real8
    module procedure z_datetime_clock
    module procedure z_datetime_alarm
   end interface

   interface as_seconds
    module procedure as_seconds_time
    module procedure as_seconds_datetime
    module procedure as_seconds_interval
   end interface

   interface as_days
    module procedure as_days_time
    module procedure as_days_datetime
    module procedure as_days_interval
   end interface


   interface z_time_to_date
    module procedure z_time_to_date_time
    module procedure z_time_to_date_clock
   end interface

   interface time_as_datetime
    module procedure time_as_datetime_time
    module procedure time_as_datetime_6int
    module procedure time_as_datetime_5int
    module procedure time_as_datetime_6real
   end interface


   interface z_date_AD
    module procedure z_dayAD_time
    module procedure z_dayAD_datetime
    module procedure z_dayAD_3int
    module procedure z_dayAD_6int
    module procedure z_dayAD_clock
    module procedure z_dayAD_alarm
   end interface

   interface z_day_AD
    module procedure z_dayAD_time
    module procedure z_dayAD_datetime
    module procedure z_dayAD_3int
    module procedure z_dayAD_6int
    module procedure z_dayAD_clock
    module procedure z_dayAD_alarm
   end interface

   interface z_second_AD
    module procedure z_secondAD_time
    module procedure z_secondAD_datetime
    module procedure z_secondAD_3int
    module procedure z_secondAD_6int
    module procedure z_secondAD_clock
    module procedure z_secondAD_alarm
   end interface

   interface cz_month
    module procedure cz_month_time
    module procedure cz_month_datetime
    module procedure cz_month_int
   end interface

   interface cz_timeline
    module procedure cz_timeline_time
    module procedure cz_timeline_clock
    module procedure cz_timeline_datetime
   end interface

   interface z_init_clock
    module procedure z_init_clock_time_interval
    module procedure z_init_clock_time_real4
    module procedure z_init_clock_time_real8
    module procedure z_init_clock_time_int
    module procedure z_init_clock_datetime_interval
    module procedure z_init_clock_datetime_real4
    module procedure z_init_clock_datetime_real8
    module procedure z_init_clock_datetime_int
   end interface

   interface z_init_alarm
    module procedure z_init_alarm_time_interval
    module procedure z_init_alarm_time_real4
    module procedure z_init_alarm_time_real8
    module procedure z_init_alarm_time_int
    module procedure z_init_alarm_datetime_interval
    module procedure z_init_alarm_datetime_real4
    module procedure z_init_alarm_datetime_real8
    module procedure z_init_alarm_datetime_int
   end interface

   interface cz_time
    module procedure cz_time_time
    module procedure cz_time_datetime
    module procedure cz_time_clock
    module procedure cz_time_alarm
   end interface

   interface cz_date
    module procedure cz_date_time
    module procedure cz_date_datetime
    module procedure cz_date_clock
    module procedure cz_date_alarm
   end interface

   interface z_year
    module procedure z_year_time
    module procedure z_year_clock
    module procedure z_year_alarm
   end interface

   interface z_month
    module procedure z_month_time
    module procedure z_month_clock
    module procedure z_month_alarm
   end interface

   interface z_day
    module procedure z_day_time
    module procedure z_day_clock
    module procedure z_day_alarm
   end interface

   interface z_hour
    module procedure z_hour_time
    module procedure z_hour_clock
    module procedure z_hour_alarm
   end interface

   interface z_minute
    module procedure z_minute_time
    module procedure z_minute_clock
    module procedure z_minute_alarm
   end interface

   interface z_second
    module procedure z_second_time
    module procedure z_second_clock
    module procedure z_second_alarm
   end interface

   interface z_sec_of_day
    module procedure z_sec_of_day_time
    module procedure z_sec_of_day_clock
    module procedure z_sec_of_day_alarm
    module procedure z_sec_of_day_datetime
   end interface

   interface z_day_of_year
    module procedure z_day_of_year_time
    module procedure z_day_of_year_clock
    module procedure z_day_of_year_alarm
    module procedure z_day_of_year_datetime
   end interface

   interface z_set_time
    module procedure z_set_clock_time_time
    module procedure z_set_clock_time_datetime
    module procedure z_set_alarm_origin_time
    module procedure z_set_alarm_origin_datetime
   end interface

   interface z_set_interval
    module procedure z_set_clock_interval_interval
    module procedure z_set_clock_interval_real4
    module procedure z_set_clock_interval_real8
    module procedure z_set_clock_interval_int
    module procedure z_set_alarm_interval_interval
    module procedure z_set_alarm_interval_real4
    module procedure z_set_alarm_interval_real8
    module procedure z_set_alarm_interval_int
   end interface

   interface z_time
    module procedure z_time_clock
    module procedure z_alarm_origin
   end interface

   interface z_datetime
    module procedure z_datetime_clock
    module procedure z_datetime_alarm
   end interface

   interface z_interval
    module procedure z_interval_clock
    module procedure z_alarm_interval
   end interface


   interface Z_set_alarm
    module procedure z_set_alarm__time_interval
    module procedure z_set_alarm__time_real4
    module procedure z_set_alarm__time_real8
    module procedure z_set_alarm__time_int
    module procedure z_set_alarm__datetime_interval
    module procedure z_set_alarm__datetime_real4
    module procedure z_set_alarm__datetime_real8
    module procedure z_set_alarm__datetime_int
   end interface

   contains




                  ! There are two basic time functions: convert
                  ! 1) convert day,month,year,hour,minute,second to a time
                  ! 2) extract day,month,year,hour,minute,second from a time


                  ! The first is done with JULIAN_COMPUTE, which takes arguments
                  ! as integers or reals, with the option that seconds can also
                  ! be integer or real (independent of day,month,year, hour and minute
                  !

                  ! The second is done with TIME_TO_DATETIME_SUB, which takes a time
                  ! argument and returns a datetime




         function JULIAN_COMPUTE_6INT(year,month,day,hour, min, sec ) RESULT(rv)
         integer, intent(in) :: year, month, day, hour, min
         integer, intent(in) :: sec
         real (kind=8) :: rv

!DIR$ ID "%Z%	%Y% $Id: zeus_clocks.cpp.f90,v 1.1 2023/12/05 20:23:02 yvikhlia Exp $ P Schopf"
!*------------------------------------
!*- calculate the julian day

  ! rv is the return value

 integer,PARAMETER :: igreg=(15+31*(10+12*1582))
 integer iy,jy,jm,ja
 real (kind=SELECTED_REAL_KIND(15)) :: day_frac

 iy = year

 if ( year < 0 ) iy = year+1
 if ( month > 2 ) then
    jy = iy
    jm = month+1
 else
    jy = iy-1
    jm = month+13
 endif

 rv = INT(365.25*jy) + INT(30.6001*jm)+day+1720995
 if ( day + 31*( month + 12*year) >= igreg ) then
   ja = INT( 0.01*jy )
   rv = rv + 2 - ja + INT(0.25*ja)
 endif
 day_frac = hour * 3600.0 + min * 60.0 + sec
 day_frac = (day_frac - 43200.0) / 86400.0
 rv = rv + day_frac

         end function julian_compute_6int

         function JULIAN_COMPUTE_5INT(year,month,day,hour, min, sec ) RESULT(rv)
         integer, intent(in) :: year, month, day, hour, min
         real, intent(in) :: sec
         real (kind=8) :: rv

!DIR$ ID "%Z%	%Y% $Id: zeus_clocks.cpp.f90,v 1.1 2023/12/05 20:23:02 yvikhlia Exp $ P Schopf"
!*------------------------------------
!*- calculate the julian day

  ! rv is the return value

 integer,PARAMETER :: igreg=(15+31*(10+12*1582))
 integer iy,jy,jm,ja
 real (kind=SELECTED_REAL_KIND(15)) :: day_frac

 iy = year

 if ( year < 0 ) iy = year+1
 if ( month > 2 ) then
    jy = iy
    jm = month+1
 else
    jy = iy-1
    jm = month+13
 endif

 rv = INT(365.25*jy) + INT(30.6001*jm)+day+1720995
 if ( day + 31*( month + 12*year) >= igreg ) then
   ja = INT( 0.01*jy )
   rv = rv + 2 - ja + INT(0.25*ja)
 endif
 day_frac = hour * 3600.0 + min * 60.0 + sec
 day_frac = (day_frac - 43200.0) / 86400.0
 rv = rv + day_frac

         end function julian_compute_5int

         function JULIAN_COMPUTE_6REAL(year,month,day,hour, min, sec ) RESULT(rv)
         real, intent(in) :: year, month, day, hour, min
         real, intent(in) :: sec
         real (kind=8) :: rv

!DIR$ ID "%Z%	%Y% $Id: zeus_clocks.cpp.f90,v 1.1 2023/12/05 20:23:02 yvikhlia Exp $ P Schopf"
!*------------------------------------
!*- calculate the julian day

  ! rv is the return value

 integer,PARAMETER :: igreg=(15+31*(10+12*1582))
 integer iy,jy,jm,ja
 real (kind=SELECTED_REAL_KIND(15)) :: day_frac

 iy = year

 if ( year < 0 ) iy = year+1
 if ( month > 2 ) then
    jy = iy
    jm = month+1
 else
    jy = iy-1
    jm = month+13
 endif

 rv = INT(365.25*jy) + INT(30.6001*jm)+day+1720995
 if ( day + 31*( month + 12*year) >= igreg ) then
   ja = INT( 0.01*jy )
   rv = rv + 2 - ja + INT(0.25*ja)
 endif
 day_frac = hour * 3600.0 + min * 60.0 + sec
 day_frac = (day_frac - 43200.0) / 86400.0
 rv = rv + day_frac

         end function julian_compute_6REAL







        subroutine SECONDS_TO_DATETIME( rv,seconds )
         TYPE (T_Datetime),intent(out) :: rv
         real (kind=8),intent(in) :: seconds

         integer :: day
         integer :: month
         integer :: year

         real (kind=8), PARAMETER :: RGREG = 2299161.0 ! Beginning of Gregorian calendar
         integer(kind=8) :: jalpha,ja,jb,jc,jd,je
         real (kind=8) :: julian_day, t
         real (kind=8) :: intt
         real (kind=8) :: julian_second

           julian_second = seconds
           if ( .not. ORIGIN_J ) julian_second = julian_second + JULIAN_OFFSET*86400.

           julian_day = (julian_second + 43200.0) / 86400.0

            ! If the user has chosen to use the modern epoch, then
            ! time is not stored as julian days, convert its value to
            ! julian


           if ( julian_day .ge. RGREG ) then
                jalpha = INT(((julian_day - 1867216) - 0.25 ) / 36524.25)
                ja = julian_day + 1 + jalpha - INT( 0.25 * jalpha )
           else
                ja = julian_day
           endif

          jb = ja + 1524

          t = 6680.0 + ((REAL(jb)-2439870.)-122.1)/365.25
          jc = FLOOR(t)
          jd = 365 * jc + INT(0.25 * jc)
          je = INT((jb - jd) / 30.6001)

          day = jb - jd - INT(30.6001 * je)
          month = je -1
          if (month > 12) month = month - 12
          year = jc - 4715
          if (month > 2) year = year - 1
          if (year <= 0) year = year - 1

          rv%day = day
          rv%month = month
          rv%year = year

          if ( (year .eq. -4713) .and. (month .eq. 1) .and. (day .eq. 1) ) then
            t = julian_second
          else
            t = julian_second + 43200.0
          endif
          intt = MOD(t, REAL(86400,KIND=8) )
          rv%hour = intt / 3600.
          rv%minute = MOD(intt/60., REAL(60.,KIND=8))
          rv%second = MOD(intt, REAL(60.,KIND=8))

        END subroutine SECONDS_TO_DATETIME
   !*-------------------------------------------------------
   !*- following are a list of private functions called
   !*- within the zeus module
   !*-------------------------------------------------------

   subroutine CHECK_ALARM(alarm,status)
   !*--------------------------------------------
   !*--- verify the alarm number
   type(T_Alarm),target,intent(in) :: alarm
   integer,intent(out),optional :: status
     if (present(status)) status = 0
     if ( .not. associated(alarm%clock) ) then
       if (verbose) then
        write(6,*) "Clock: Alarm has a bad clock pointer "
       endif
       if (present(status)) status = zcBAD_CLOCKID
       if (abort_on_error) stop
     else
       if ( ( alarm%i .gt. size(alarm%clock%alarms)) .or. &
            ( alarm%i .le. 0) ) then
         if (verbose) then
           write(6,*) "Clock: Invalid alarm reference  "
         endif
         if (present(status)) status = zcBAD_ALARM
       endif
     endif


   end subroutine check_alarm

   !*---------------------------------------------------------------
   !*-- the following public functions are called from the models
   !*---------------------------------------------------------------

   function CZ_TIMELINE_CLOCK(clock,status) RESULT(rv)
   !*--------------------------------------------------
   !*-- return the char value of the current clock time
   Type (T_Clock),intent(in) :: clock
   integer,intent(out),optional :: status

    Type (T_DateTime) :: d
    character(len=22) rv

    if ( present(status) ) status = 0
    d = clock%time
    write (FMT='(I2.2,":",I2.2,":",I2.2,"   ",A3," ",I2",",I4.4)',unit=rv) &
      d%hour, d%minute, INT(d%second), cz_month(d%month), d%day, d%year

   end function cz_timeline_clock

   subroutine Z_SET_OPTIONS (verbose_msg, fatal, message_unit, &
                             origin_julian, accuracy)
   !*------------------------------------
   !*- set status options
   logical,intent(in),optional :: verbose_msg, fatal, origin_julian
   integer,intent(in),optional :: message_unit
   real, intent(in), optional :: accuracy

    if (present(verbose_msg)) verbose = verbose_msg
    if (present(fatal)) abort_on_error = fatal
    if (present(message_unit)) emu = message_unit
    if (present(origin_julian)) origin_j = origin_julian
    if (present(accuracy)) accuracy_factor = accuracy

   end subroutine z_set_options

   subroutine Z_GET_OPTIONS (verbose_msg, fatal, message_unit, origin_julian)
   !*------------------------------------
   !*- get status options
   logical,intent(out),optional :: verbose_msg, fatal, origin_julian
   integer,intent(out),optional :: message_unit

    if (present(verbose_msg)) verbose_msg = verbose
    if (present(fatal)) fatal = abort_on_error
    if (present(message_unit)) message_unit = emu
    if (present(origin_julian)) origin_julian = origin_j

   end subroutine z_get_options

   subroutine Z_INIT_CLOCK_TIME_interval(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_time),optional,intent(in) :: time
   Type(T_timeInterval),optional,intent(in) :: interval
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    nullify(clock%alarms)
    if (present(time) ) clock%time = time
    if (present(interval) ) clock%interval = interval

   end subroutine Z_INIT_CLOCK_TIME_interval

   subroutine Z_INIT_CLOCK_TIME_real4(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_time),intent(in) :: time
   real(kind=4),intent(in) :: interval
   integer,intent(out),optional :: status


    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    clock%interval = interval

   end subroutine Z_INIT_CLOCK_TIME_real4

   subroutine Z_INIT_CLOCK_TIME_real8(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_time),intent(in) :: time
   real(kind=8),intent(in) :: interval
   integer,intent(out),optional :: status


    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    clock%interval = interval

   end subroutine Z_INIT_CLOCK_TIME_real8

   subroutine Z_INIT_CLOCK_TIME_int(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_time),intent(in) :: time
   integer,intent(in) :: interval
   integer,intent(out),optional :: status



    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    clock%interval = interval

   end subroutine Z_INIT_CLOCK_TIME_int

   subroutine Z_INIT_CLOCK_DATETIME_interval(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_Datetime),intent(in) :: time
   Type (T_TimeInterval), intent(in), optional :: interval
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    if ( present(interval)) clock%interval = interval

   end subroutine Z_INIT_CLOCK_DATETIME_interval

   subroutine Z_INIT_CLOCK_DATETIME_real4(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_Datetime),intent(in) :: time
   real(kind=4), intent(in) :: interval
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    clock%interval = interval

   end subroutine Z_INIT_CLOCK_DATETIME_real4

   subroutine Z_INIT_CLOCK_DATETIME_real8(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_Datetime),intent(in) :: time
   real(kind=8), intent(in) :: interval
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    clock%interval = interval

   end subroutine Z_INIT_CLOCK_DATETIME_real8

   subroutine Z_INIT_CLOCK_DATETIME_int(clock, time, interval, status)
   !*-----------------------------
   !*---- initialize the clock time and interval
   Type(T_Clock),intent(inout) :: clock
   Type(T_Datetime),intent(in) :: time
   integer, intent(in) :: interval
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    nullify(clock%alarms)
    clock%time = time
    clock%interval = interval

   end subroutine Z_INIT_CLOCK_DATETIME_int


   subroutine GROW_ARRAY( Aptr, By )
   !*---------------------------------------------
   !*-- increase the size of aptr
   Type ( T_Alarm_data ) , pointer :: Aptr(:)
   integer, intent(in) :: By

   Type ( T_Alarm_data ) , allocatable, target :: Anew(:)
   integer :: i, aptr_size

     aptr_size = size(aptr)
     allocate( anew( aptr_size + by) )
     anew(1:aptr_size) = aptr(1:aptr_size)

     deallocate( aptr )
     allocate ( aptr( aptr_size + by ) )
     aptr(1:aptr_size + by) = anew(1:aptr_size + by)
     deallocate(anew)

   end subroutine GROW_ARRAY



   subroutine Z_INIT_ALARM_time_interval( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_time),intent(in),optional :: origin
   Type (T_TimeInterval),intent(in),optional :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.
       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if
       if (present(interval) ) then
         clock%alarms(i)%interval = interval
       else
         clock%alarms(i)%interval = Z_INTERVAL(clock)
       end if

       if (present(origin) ) then
         clock%alarms(i)%origin = origin
       else
         clock%alarms(i)%origin = Z_TIME(clock)
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine z_init_alarm_time_interval

   subroutine Z_INIT_ALARM_time_real8( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_time),intent(in) :: origin
   real(kind=8),intent(in) :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.

       clock%alarms(i)%interval = interval
       clock%alarms(i)%origin = origin

       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine Z_INIT_ALARM_time_real8

   subroutine Z_INIT_ALARM_time_real4( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_time),intent(in) :: origin
   real(kind=4),intent(in) :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.

       clock%alarms(i)%interval = interval
       clock%alarms(i)%origin = origin

       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine Z_INIT_ALARM_time_real4

   subroutine Z_INIT_ALARM_time_int( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_time),intent(in) :: origin
   integer,intent(in) :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.

       clock%alarms(i)%interval = interval
       clock%alarms(i)%origin = origin

       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine Z_INIT_ALARM_time_int

   subroutine Z_INIT_ALARM_datetime_interval( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_datetime),intent(in) :: origin
   Type (T_TimeInterval),intent(in),optional :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.
       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if
       if (present(interval) ) then
         clock%alarms(i)%interval = interval
       else
         clock%alarms(i)%interval = Z_INTERVAL(clock)
       end if

       clock%alarms(i)%origin = origin

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine z_init_alarm_datetime_interval

   subroutine Z_INIT_ALARM_datetime_real8( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_DateTime),intent(in) :: origin
   real(kind=8),intent(in) :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.

       clock%alarms(i)%interval = interval
       clock%alarms(i)%origin = origin

       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine Z_INIT_ALARM_datetime_real8

   subroutine Z_INIT_ALARM_datetime_real4( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_DateTime),intent(in) :: origin
   real(kind=4),intent(in) :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.

       clock%alarms(i)%interval = interval
       clock%alarms(i)%origin = origin

       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine Z_INIT_ALARM_datetime_real4

   subroutine Z_INIT_ALARM_datetime_int( clock, alarm, origin, interval, ring, &
                            sticky, name, status)
   !*-------------------------------------------------
   !*--- initialize alarm parms and associate with a clock
   Type (T_Clock),intent(inout), target :: clock
   Type (T_Alarm),intent(inout), target :: alarm
   Type (T_Datetime),intent(in) :: origin
   integer,intent(in) :: interval
   logical,intent(in),optional :: ring
   logical,intent(in),optional :: sticky
   character(LEN=*), intent(in), optional :: name
   integer,intent(out),optional :: status

   integer, PARAMETER :: GROW_SIZE = 1
   integer i
   logical found
   integer :: LEN_NAME

    if ( present(status) ) status = 0

    alarm%clock => clock
    alarm%i = 0

    if ( .NOT. ASSOCIATED( clock%alarms ) ) then
         ALLOCATE( clock%alarms(GROW_SIZE) )
         do i=1, SIZE( clock%alarms )
             clock%alarms(i)%taken = .false.
         enddo
    endif

    do while ( alarm%i == 0 )

       do i=1, SIZE( clock%alarms )
          if ( .NOT. clock%alarms(i)%taken ) then
            alarm%i = i
            exit
          endif
       enddo

       if ( alarm%i == 0 ) then
           call GROW_ARRAY( clock%alarms , GROW_SIZE )
           alarm%i = size(clock%alarms)
       endif

    enddo

   !*-- set name, interval, origin, ring, sticky
       i = alarm%i
       clock%alarms(i)%taken = .true.

       clock%alarms(i)%interval = interval
       clock%alarms(i)%origin = origin

       if (present(name) ) then
         clock%alarms(i)%name = " "
         LEN_NAME = min(MAX_NAME_LEN, len_trim(name))
         clock%alarms(i)%name(1:LEN_NAME) = name(1:LEN_NAME)
       else
         clock%alarms(i)%name = "UN-NAMED ALARM"
       end if

       if (present(ring) ) then
         clock%alarms(i)%ring = ring
       else
         clock%alarms(i)%ring = .false.
       end if

       if (present(sticky) ) then
         clock%alarms(i)%sticky = sticky
       else
         clock%alarms(i)%sticky = .true.
       end if

   end subroutine Z_INIT_ALARM_datetime_int

   subroutine Z_TICK(clock, status)
   !*---------------------------------------------------
   !*-- tick the clock and check all clock alarms
   type (T_Clock),intent(inout) :: clock
   integer,intent(out),optional :: status

   integer :: i,j
   type (T_timeInterval) :: dt, st

    if ( present(status) ) status = 0

     clock%time = clock%time + clock%interval
     if ( ASSOCIATED(clock%alarms) ) then
     do i = 1, SIZE(clock%alarms)
       if (clock%alarms(i)%taken) then
         dt = clock%time - clock%alarms(i)%origin
         if ( (dt%value >= 0) .and. (clock%alarms(i)%interval%value > 0.) )then
           st = mod_times(dt, clock%alarms(i)%interval)
           if (st%value < clock%interval%value) then
             clock%alarms(i)%ring = .true.
           else
             if (.not. clock%alarms(i)%sticky) then
               clock%alarms(i)%ring = .false.
             end if
           end if
         endif
         if ( (NINT(dt%value) .ge. 0) .and. &
              (clock%alarms(i)%interval%value .eq. 0) )then
           clock%alarms(i)%ring = .true.
         endif
       endif
     enddo
     endif

   end subroutine z_tick


   type (T_time) function Z_ALARM_ORIGIN(alarm, status)
   !*----------------------------------
   !*-- return the alarm origin
   type (T_Alarm),intent(in) :: alarm
   integer,intent(out),optional :: status

     if (present(status)) then
      call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
     else
      call check_alarm(alarm)
     endif

    z_alarm_origin = alarm%clock%alarms(alarm%i)%origin

   end function z_alarm_origin

   type (T_DateTime) function Z_DATETIME_ALARM(alarm, status)
   !*----------------------------------
   !*-- return the alarm origin
   type (T_Alarm),intent(in) :: alarm
   integer,intent(out),optional :: status

     if (present(status)) then
      Z_DATETIME_ALARM = Z_ALARM_ORIGIN(alarm, status)
     else
      Z_DATETIME_ALARM = Z_ALARM_ORIGIN(alarm)
     endif

   end function Z_DATETIME_ALARM


   type (T_timeInterval) function Z_ALARM_INTERVAL(alarm, status)
   !*----------------------------
   !*-- return the alarm interval
   type (T_Alarm),intent(in) :: alarm
   integer,intent(out),optional :: status

     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

    z_alarm_interval = alarm%clock%alarms(alarm%i)%interval

   end function z_alarm_interval


   logical function Z_RINGING(alarm, status)
   !*-------------------------------------
   !*-- return the ring status of an alarm
   type (T_Alarm),intent(in) :: alarm
   integer,intent(out),optional :: status

     if (present(status)) then
       call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

    z_ringing = alarm%clock%alarms(alarm%i)%ring

   end function z_ringing


   integer function Z_NALARMS(clock, status)
   !*---------------------------------------------
   !*-- return the number of alarms for a clock id
   type (T_Clock),intent(in) :: clock
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
     z_nalarms = size(clock%alarms)

   end function z_nalarms

   subroutine Z_SET_ALARM_ORIGIN_TIME(alarm, origin, status)
   !*------------------------------------
   !*-- set the alarm origin
   type (T_Alarm),intent(inout) :: alarm
   type (T_time),intent(in) :: origin
   integer,intent(out),optional :: status

     if (present(status)) then
       call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

    alarm%clock%alarms(alarm%i)%origin = origin

   end subroutine z_set_alarm_origin_time

   subroutine Z_SET_ALARM_ORIGIN_DATETIME(alarm, origin, status)
   !*------------------------------------
   !*-- set the alarm origin
   type (T_Alarm),intent(inout) :: alarm
   type (T_Datetime),intent(in) :: origin
   integer,intent(out),optional :: status

     if (present(status)) then
       call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

    alarm%clock%alarms(alarm%i)%origin = origin

   end subroutine z_set_alarm_origin_datetime


   subroutine Z_SET_ALARM_INTERVAL_interval(alarm, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_timeInterval),intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%interval = interval

   end subroutine z_set_alarm_interval_interval

   subroutine Z_SET_ALARM_INTERVAL_REAL4(alarm, seconds, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   real(kind=4),intent(in) :: seconds
   integer,intent(out),optional :: status

   character*132 :: message


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

     if ( seconds <= 0. ) then
       if ( present(status) ) then
           status = -1
       else
        write (0,*) "WARNING: Z_SET_ALARM_INTERVAL"
        write (0,*) "  seconds <= 0. "
        write (0,FMT='("  seconds = ",E14.7,"  Alarm: ", A60)') &
               seconds,alarm%clock%alarms(alarm%i)%name

       endif
     endif

     alarm%clock%alarms(alarm%i)%interval = seconds

   end subroutine z_set_alarm_interval_REAL4

   subroutine Z_SET_ALARM_INTERVAL_REAL8(alarm, seconds, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   real(kind=8),intent(in) :: seconds
   integer,intent(out),optional :: status

   character*132 :: message


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

     if ( seconds <= 0. ) then
       if ( present(status) ) then
           status = -1
       else
        write (0,*) "WARNING: Z_SET_ALARM_INTERVAL"
        write (0,*) "  seconds <= 0. "
        write(0,FMT='("  seconds = ",E14.7,"  Alarm: ", A60)') &
               seconds,alarm%clock%alarms(alarm%i)%name

       endif
     endif

     alarm%clock%alarms(alarm%i)%interval = seconds

   end subroutine z_set_alarm_interval_REAL8

   subroutine Z_SET_ALARM_INTERVAL_INT(alarm, seconds, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   integer,intent(in) :: seconds
   integer,intent(out),optional :: status
   character*132 :: message


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

     if ( seconds <= 0 ) then
       if ( present(status) ) then
           status = -1
       else
        write (0,*) "WARNING: Z_SET_ALARM_INTERVAL"
        write (0,*) "  seconds <= 0. "
         write(0, FMT='("  seconds = ",E14.7,"  Alarm: ", A60)') &
               seconds,alarm%clock%alarms(alarm%i)%name


       endif
     endif

     alarm%clock%alarms(alarm%i)%interval = seconds

   end subroutine z_set_alarm_interval_int

   subroutine Z_SET_ALARM__time_interval(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_time),intent(in),optional :: origin
   type (T_timeInterval), intent(in), optional :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     if (present(interval)) alarm%clock%alarms(alarm%i)%interval = interval
     if (present(origin)) alarm%clock%alarms(alarm%i)%origin = origin

   end subroutine Z_SET_ALARM__time_interval

   subroutine Z_SET_ALARM__time_real4(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_time),intent(in) :: origin
   real (kind=4), intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%interval = interval
     alarm%clock%alarms(alarm%i)%origin = origin

   end subroutine Z_SET_ALARM__time_real4

   subroutine Z_SET_ALARM__time_real8(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_time),intent(in) :: origin
   real (kind=8), intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%interval = interval
     alarm%clock%alarms(alarm%i)%origin = origin

   end subroutine Z_SET_ALARM__time_real8

   subroutine Z_SET_ALARM__time_int(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_time),intent(in) :: origin
   integer, intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%interval = interval
     alarm%clock%alarms(alarm%i)%origin = origin

   end subroutine Z_SET_ALARM__time_int

   subroutine Z_SET_ALARM__datetime_interval(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_Datetime),intent(in) :: origin
   type (T_TimeInterval), intent(in),optional :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%origin = origin
     if ( present(interval)) alarm%clock%alarms(alarm%i)%interval = interval

   end subroutine Z_SET_ALARM__datetime_interval

   subroutine Z_SET_ALARM__datetime_real4(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_Datetime),intent(in) :: origin
   real(kind=4), intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%origin = origin
     alarm%clock%alarms(alarm%i)%interval = interval

   end subroutine Z_SET_ALARM__datetime_real4

   subroutine Z_SET_ALARM__datetime_real8(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_Datetime),intent(in) :: origin
   real(kind=8), intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%origin = origin
     alarm%clock%alarms(alarm%i)%interval = interval

   end subroutine Z_SET_ALARM__datetime_real8

   subroutine Z_SET_ALARM__datetime_int(alarm, origin, interval, status)
   !*-------------------------
   !*-- set the alarm interval
   type (T_Alarm),intent(inout) :: alarm
   type (T_Datetime),intent(in) :: origin
   integer, intent(in) :: interval
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%origin = origin
     alarm%clock%alarms(alarm%i)%interval = interval

   end subroutine Z_SET_ALARM__datetime_int

   subroutine Z_SHUT_ALARM(alarm, status)
   !*-----------------------
   !*-- shut the clock alarm
   type (T_Alarm),intent(inout) :: alarm
   integer,intent(out),optional :: status


     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

     alarm%clock%alarms(alarm%i)%ring = .false.

   end subroutine z_shut_alarm

   subroutine Z_SET_ALARM_RINGING(alarm, status)
   !*-----------------------------------
   !*-- set the ringing status of an alarm to true
   type (T_Alarm),intent(inout) :: alarm
   integer,intent(out),optional :: status

     if (present(status)) then
       call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif
     alarm%clock%alarms(alarm%i)%ring = .true.

   end subroutine z_set_alarm_ringing

   subroutine Z_SET_CLOCK_TIME_time(clock,time, status)
   !*-----------------------------
   !*-- set the time for a clock
   type (T_Clock),intent(inout) :: clock
   type (T_time),intent(in):: time
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
     clock%time = time

   end subroutine Z_SET_CLOCK_TIME_time

   subroutine Z_SET_CLOCK_TIME_DATETIME(clock,time, status)
   !*-----------------------------
   !*-- set the time for a clock
   type (T_Clock),intent(inout) :: clock
   type (T_Datetime),intent(in):: time
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
     clock%time = time

   end subroutine Z_SET_CLOCK_TIME_datetime

   subroutine Z_SET_CLOCK_INTERVAL_INTERVAL(clock,interval, status)
   !*---------------------------
   !*-- set the clock interval
   type (T_Clock),intent(inout):: clock
   type (T_timeInterval),intent(in) :: interval
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    clock%interval = interval

   end subroutine Z_SET_CLOCK_INTERVAL_INTERVAL

   subroutine Z_SET_CLOCK_INTERVAL_REAL4(clock,seconds,status)
   type (T_Clock),intent(inout):: clock
   real(kind=4),intent(in) :: seconds
   integer,intent(out),optional :: status

    if ( present(status) ) then
     if ( seconds > 0 ) then
       status = 0
     else
       status = -1
     endif
    else




     write (0,*) "WARNING: Clock interval should be > 0."


    endif

    clock%interval%value = seconds

   end subroutine Z_SET_CLOCK_INTERVAL_REAL4

   subroutine Z_SET_CLOCK_INTERVAL_REAL8(clock,seconds,status)
   type (T_Clock),intent(inout):: clock
   real(kind=8),intent(in) :: seconds
   integer,intent(out),optional :: status

    if ( present(status) ) then
     if ( seconds > 0 ) then
       status = 0
     else
       status = -1
     endif
    else



     write (0,*) "WARNING: Clock interval should be > 0."

    endif

    clock%interval%value = seconds

   end subroutine Z_SET_CLOCK_INTERVAL_REAL8

   subroutine Z_SET_CLOCK_INTERVAL_INT(clock,seconds,status)
   type (T_Clock),intent(inout):: clock
   integer,intent(in) :: seconds
   integer,intent(out),optional :: status

    if ( present(status) ) then
     if ( seconds > 0 ) then
       status = 0
     else
       status = -1
     endif
    else



     write (0,*) "WARNING: Clock interval should be > 0."

    endif

    clock%interval%value = seconds

   end subroutine Z_SET_CLOCK_INTERVAL_INT




   type (T_timeInterval) function Z_INTERVAL_CLOCK(clock, status)
   !*-------------------------------------------
   !*-- return the interval
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_interval_clock = clock%interval

   end function z_interval_clock


   type (T_time) function Z_TIME_CLOCK(clock, status)
   !*--------------------------------------------------
   !*-- return the ttime value
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_time_clock = clock%time

   end function z_time_clock

   type (T_DateTime) function Z_DATETIME_CLOCK(clock, status)
   !*--------------------------------------------------
   !*-- return the ttime value
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_DateTime_clock = clock%time

   end function z_DateTime_clock


   real (kind=8) function Z_JULIAN_DATE_CLOCK(clock, status)
   !*---------------------------------------
   !*-- return the real value of the julian date
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_julian_date_clock = z_julian_date_time(clock%time)

   end function z_julian_date_clock

   integer function Z_MONTH_CLOCK(clock,status)
   !*--------------------------------------------
   !*-- return the integer value of current month
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_month_clock = z_month_time(clock%time)

   end function z_month_clock

   integer function Z_YEAR_CLOCK(clock, status)
   !*-------------------------------------------
   !*-- return the integer value of current year
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_year_clock = z_year_time(clock%time)

   end function z_year_clock

   integer function Z_DAY_CLOCK(clock, status)
   !*---------------------------------------------------------------
   !*-- return the integer value of current date of the current year
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_day_clock = z_day_time(clock%time)

   end function z_day_clock

   integer function Z_HOUR_CLOCK(clock, status)
   !*-------------------------------------------
   !*-- return the integer value of current hour
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_hour_clock = z_hour_time(clock%time)

   end function z_hour_clock

   integer function Z_MINUTE_CLOCK(clock, status)
   !*---------------------------------------------
   !*-- return the integer value of current minute
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_minute_clock = z_minute_time(clock%time)

   end function z_minute_clock

   integer function Z_SECOND_CLOCK(clock, status)
   !*---------------------------------------------
   !*-- return the integer value of current second
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status


   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_second_clock = z_second_time(clock%time)

   end function z_second_clock




   real (kind=8) function Z_JULIAN_DATE_alarm(alarm, status)
   !*---------------------------------------
   !*-- return the real value of the julian date
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_julian_date_alarm = z_julian_date_time(as_time(alarm))

   end function z_julian_date_alarm

   integer function Z_MONTH_alarm(alarm,status)
   !*--------------------------------------------
   !*-- return the integer value of current month
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_month_alarm = z_month_time(as_Time(alarm))

   end function z_month_alarm

   integer function Z_YEAR_alarm(alarm, status)
   !*-------------------------------------------
   !*-- return the integer value of current year
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_year_alarm = z_year_time(as_Time(alarm))

   end function z_year_alarm

   integer function Z_DAY_alarm(alarm, status)
   !*---------------------------------------------------------------
   !*-- return the integer value of current date of the current year
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_day_alarm = z_day_time(as_Time(alarm))

   end function z_day_alarm

   integer function Z_HOUR_alarm(alarm, status)
   !*-------------------------------------------
   !*-- return the integer value of current hour
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_hour_alarm = z_hour_time(as_Time(alarm))

   end function z_hour_alarm

   integer function Z_MINUTE_alarm(alarm, status)
   !*---------------------------------------------
   !*-- return the integer value of current minute
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_minute_alarm = z_minute_time(as_Time(alarm))

   end function z_minute_alarm

   integer function Z_SECOND_alarm(alarm, status)
   !*---------------------------------------------
   !*-- return the integer value of current second
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status


   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_second_alarm = z_second_time(as_Time(alarm))

   end function z_second_alarm

   real(kind=8) function Z_SEC_OF_DAY_CLOCK(clock, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

   integer year, month, day, hour, minute, second

    if ( present(status) ) status = 0
    z_sec_of_day_clock = z_sec_of_day_time(clock%time)

   end function z_sec_of_day_clock

   real(kind=8) function Z_SEC_OF_DAY_ALARM(alarm, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_sec_of_day_alarm = z_sec_of_day_time(AS_TIME(alarm))

   end function Z_SEC_OF_DAY_ALARM

   real(kind=8) function Z_SEC_OF_DAY_DATETIME(time, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_Datetime),intent(in):: time
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_sec_of_day_datetime = time%second + 60.*time%minute + 3600.*time%hour

   end function z_sec_of_day_datetime

   integer function Z_DAY_OF_YEAR_CLOCK(clock, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_Clock),intent(in):: clock
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_day_of_year_clock = z_day_of_year_time(clock%time)

   end function z_day_of_year_clock

   integer function Z_DAY_OF_YEAR_ALARM(alarm, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_alarm),intent(in):: alarm
   integer,intent(out),optional :: status

    if ( present(status) ) status = 0
    z_day_of_year_alarm = z_day_of_year_time(AS_TIME(alarm))

   end function z_day_of_year_alarm

   integer function Z_DAY_OF_YEAR_DATETIME(time, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_Datetime),intent(in):: time
   integer,intent(out),optional :: status

   type (T_Datetime) :: d
   type (T_timeInterval) :: dt

    d=time
    d%day = 31
    d%month = 12
    d%year = d%year-1

    dt = AS_TIME(time) - AS_TIME(d)

    if ( present(status) ) status = 0
    z_day_of_year_datetime = NINT(dt%value/86400.)

   end function z_day_of_year_datetime

   integer function Z_DAY_OF_YEAR_TIME(time, status)
   !*---------------------------------------------
   !*-- return the time in seconds in start of day
   type (T_Time),intent(in):: time
   integer,intent(out),optional :: status

   type (T_Datetime) :: d
   type (T_timeInterval) :: dt

    d=time
    d%day = 31
    d%month = 12
    d%year = d%year-1

    dt = Time - AS_TIME(d)

    if ( present(status) ) status = 0
    z_day_of_year_time = NINT(dt%value/86400.)

   end function z_day_of_year_time


   subroutine Z_FREE_ALARM (alarm, status)
   !*-------------------------------------------------
   !*- initialize the alarm data
   Type (T_Alarm),intent(inout),target :: alarm
   integer,intent(out),optional :: status

   !*-- verify the parms
     if (present(status)) then
       call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

     alarm%clock%alarms(alarm%i)%interval%value = 0.
     alarm%clock%alarms(alarm%i)%origin = as_time(0.)
     alarm%clock%alarms(alarm%i)%ring = .false.
     alarm%clock%alarms(alarm%i)%taken = .false.

   end subroutine z_free_alarm


   subroutine Z_FREE_CLOCK (clock, status)
   !*---------------------------------------------
   !*-- initialize the clock parms
   type (T_Clock),intent(inout):: clock
   integer,intent(out),optional :: status

   type (T_Alarm), pointer :: an, ac

     if ( present(status) ) status = 0
     if ( associated(clock%alarms) ) deallocate(clock%alarms)
     clock%interval%value = 0.
     clock%time = as_time(0.)

   end subroutine z_free_clock

   subroutine Z_GET_STATE(alarm,statvec, status)
   !*---------------------------------
   !*-- return the status of an alarms
   type (T_Alarm),intent(in):: alarm
   real (kind=8), intent(out) :: statvec(3)
   integer,intent(out),optional :: status

     if (present(status)) then
       call check_alarm(alarm, status)
       if (status .ne. zcNO_ERROR) return
     else
       call check_alarm(alarm)
     endif

     statvec(1) = alarm%clock%alarms(alarm%i)%origin%value
     statvec(2) = alarm%clock%alarms(alarm%i)%interval%value
     if ( alarm%clock%alarms(alarm%i)%ring ) then
      statvec(3) = 1.0
     else
      statvec(3) = 0.0
     endif

   end subroutine z_get_state


   subroutine Z_RESET_STATE (alarm, statvec, status)
   !*------------------------------------
   !*-- reset the alarm from parms passed
   type (T_Alarm),intent(inout) :: alarm
   real (kind=8), intent(out) :: statvec(3)
   integer,intent(out),optional :: status

   type (T_time) :: origin
   type (T_timeInterval) :: interval

    if (present(status)) then
      call check_alarm(alarm, status)
      if (status .ne. zcNO_ERROR) return
    else
      call check_alarm(alarm)
    endif

    origin = as_time(statvec(1))
    interval%value = statvec(2)

    alarm%clock%alarms(alarm%i)%origin = origin
    alarm%clock%alarms(alarm%i)%interval = interval
    if (statvec(3) .eq. 1.0) then
     alarm%clock%alarms(alarm%i)%ring = .true.
    else
     alarm%clock%alarms(alarm%i)%ring = .false.
    endif

   end subroutine z_reset_state


   subroutine Z_PRINT_CLOCK(clock, suppress_alarms, status)
   !*-------------------------------------
   !*-- print the clock alarms for clock
   type (T_Clock),intent(in) :: clock
   logical,intent(in ),optional :: suppress_alarms
   integer,intent(out),optional :: status

   type ( T_DateTime ) :: dt
   integer interval,i
   logical ringing
   character(len=22) ctimlin
   character(len=79) OUT_LINE
   logical :: PRINT_ALARMS

    dt = Clock%time
    if ( present(status) ) status = 0
    write (6,*)
    write(6, FMT='("Current state of the clock: ", A22)') &
            cz_timeline(clock)
    write(6, FMT='("  Clock interval = ",F14.1," seconds")') &
       clock%interval%value



    if ( associated(clock%alarms) ) then

      if (.not. present(suppress_alarms)) then
        PRINT_ALARMS = .true.
      else if (suppress_alarms) then
          PRINT_ALARMS = .false.
      else
          PRINT_ALARMS = .true.
      end if

      if (PRINT_ALARMS) then




       write (6,*) "Interval    Ringing   Origin                    Name"


        do i = 1,size(clock%alarms)
          interval = NINT(clock%alarms(i)%interval%value)
          ringing = clock%alarms(i)%ring
          ctimlin = cz_timeline(clock%alarms(i)%origin)





          write(6, FMT='(I8,4X,L7,3X,A22, 5x, A30)') &
            interval, ringing, ctimlin, clock%alarms(i)%name

        enddo
      end if
    endif



     write (6,*)


   end subroutine z_print_clock

   !*---------------------------------------------------
   !*- the following functions are used to manage time
   !*---------------------------------------------------

   logical function LEAP(y)
   !*---------------------------------------------------
   !*--- return a boolean indicating if y is a leap year
   integer,intent(in) :: y

    leap = .true.
    if (y .gt. 1582) then
     if (MOD(y, 4) .ne. 0) leap = .false.
     if (MOD(y, 100) .eq. 0) leap = .false.
     if (MOD(y, 400) .eq. 0) leap = .true.
     if (MOD(y,4000) .eq. 0) leap = .false.
    else
     if (MOD(y,4) .ne. 0) leap = .false.
    endif

   end function leap



   !----------------- ASSIGNMENTS -----------------

    subroutine TIME_TO_DATETIME( d,time )
     TYPE (T_Datetime),intent(out) :: d
     type (T_time), intent(in) :: time
     real (kind=8) :: seconds
     seconds = time%value
     call SECONDS_TO_DATETIME( d, seconds)
    end subroutine TIME_TO_DATETIME

    subroutine CLOCK_TO_TIME( t,clock )
     TYPE (T_time),intent(out) :: t
     type (T_clock), intent(in) :: clock
     t = As_Time(Clock)
    end subroutine CLOCK_TO_TIME

    subroutine CLOCK_TO_DATETIME( d,clock )
     TYPE (T_Datetime),intent(out) :: d
     type (T_clock), intent(in) :: clock
     d = As_Time(Clock)
    end subroutine CLOCK_TO_DATETIME

    subroutine DATETIME_TO_TIME( t, d)
     type (T_Time), intent(out) :: t
     type (T_DateTime), intent(in) :: d
     real (kind=8) :: days
     days = JULIAN_COMPUTE(d%year,d%month,d%day,d%hour, d%minute, d%second )
     if ( .NOT. ORIGIN_J ) days = days - JULIAN_OFFSET
     t = days*86400.
    end subroutine DATETIME_TO_TIME

    subroutine time_to_seconds_real4(sec,t)
    real(kind=4), intent(out) :: sec
    type (T_time), intent(in) :: t
     sec = t%value
    end subroutine

    subroutine time_to_seconds_real8(sec,t)
    real(kind=8), intent(out) :: sec
    type (T_time), intent(in) :: t
     sec = t%value
    end subroutine time_to_seconds_real8

    subroutine time_to_seconds_int(sec,t)
    integer, intent(out) :: sec
    type (T_time), intent(in) :: t
     sec = t%value
    end subroutine time_to_seconds_int

    subroutine seconds_to_time_real4(t,sec)
    real(kind=4), intent(in) :: sec
    type (T_time), intent(out) :: t
     t%value = sec
    end subroutine

    subroutine seconds_to_time_real8(t,sec)
    real(kind=8), intent(in) :: sec
    type (T_time), intent(out) :: t
     t%value = sec
    end subroutine

    subroutine seconds_to_time_int(t,sec)
    integer, intent(in) :: sec
    type (T_time), intent(out) :: t
     t%value = sec
    end subroutine

    subroutine interval_to_real4(sec,dt)
    real(kind=4), intent(out) :: sec
    type (T_TimeInterval), intent(in) :: dt
     sec = dt%value
    end subroutine interval_to_real4

    subroutine interval_to_real8(sec,dt)
    real(kind=8), intent(out) :: sec
    type (T_TimeInterval), intent(in) :: dt
     sec = dt%value
    end subroutine interval_to_real8

    subroutine interval_to_int(sec,dt)
    integer, intent(out) :: sec
    type (T_TimeInterval), intent(in) :: dt
     sec = dt%value
    end subroutine interval_to_int

    subroutine real8_to_interval(dt,sec)
    real(kind=8), intent(in) :: sec
    type (T_TimeInterval), intent(out) :: dt
      dt%value = sec
    end subroutine real8_to_interval

    subroutine real4_to_interval(dt,sec)
    real(kind=4), intent(in) :: sec
    type (T_TimeInterval), intent(out) :: dt
      dt%value = sec
    end subroutine real4_to_interval

    subroutine int_to_interval(dt,sec)
    integer, intent(in) :: sec
    type (T_TimeInterval), intent(out) :: dt
      dt%value = sec
    end subroutine int_to_interval


    subroutine datetime_to_seconds_real4(sec,d)
    type (T_DateTime), intent(in) :: d
    real(kind=4), intent(out) :: sec
     sec = As_Time(d)
    end subroutine

    subroutine datetime_to_seconds_real8(sec,d)
    type (T_DateTime), intent(in) :: d
    real(kind=8), intent(out) :: sec
     sec = As_Time(d)
    end subroutine

    subroutine datetime_to_seconds_int(sec,d)
    type (T_DateTime), intent(in) :: d
    integer, intent(out) :: sec
     sec = As_Time(d)
    end subroutine

    subroutine seconds_to_datetime_real4(d,sec)
    type (T_DateTime), intent(out) :: d
    real(kind=4), intent(in) :: sec
     d = As_Time(sec)
    end subroutine

    subroutine seconds_to_datetime_real8(d,sec)
    type (T_DateTime), intent(out) :: d
    real(kind=8), intent(in) :: sec
     d = As_Time(sec)
    end subroutine

    subroutine seconds_to_datetime_int(d,sec)
    type (T_DateTime), intent(out) :: d
    integer, intent(in) :: sec
     d = As_Time(sec)
    end subroutine


   function ADD__time_interval(a,b) RESULT(rv) ! Time = Time + interval
   type (T_time), intent(in) :: a
   type (T_timeInterval), intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value + b%value

   end function add__time_interval

   function ADD__interval_time(a,b) RESULT(rv) ! Time = interval + Time
   type (T_timeInterval), intent(in) :: a
   type (T_time), intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value + b%value

   end function ADD__interval_time

   function ADD__interval_interval(a,b) RESULT(rv) ! Interval = Interval + Interval
   type (T_timeInterval), intent(in) :: a,b
   type (T_timeInterval) ::rv

    rv%value = a%value + b%value

   end function ADD__interval_interval

   function ADD__time_secondsR4(a,b) RESULT(rv) ! Time = Time + real seconds
   type (T_time), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_time) ::rv

    rv%value = a%value + b

   end function ADD__time_secondsR4

   function ADD__time_secondsR8(a,b) RESULT(rv) ! Time = Time + real seconds
   type (T_time), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_time) ::rv

    rv%value = a%value + b

   end function ADD__time_secondsR8

   function ADD__time_secondsI(a,b) RESULT(rv) ! Time = Time + integer seconds
   type (T_time), intent(in) :: a
   integer, intent(in) :: b
   type (T_time) ::rv

    rv%value = a%value + b

   end function ADD__time_secondsI

   function ADD__secondsR4_time(a,b) RESULT(rv) ! Time = Time + real seconds
   real(kind=4), intent(in) :: a
   type (T_time), intent(in) :: b
   type (T_time) ::rv

    rv%value = a + b%value

   end function ADD__secondsR4_time

   function ADD__secondsR8_time(a,b) RESULT(rv) ! Time = Time + real seconds
   real(kind=8), intent(in) :: a
   type (T_time), intent(in) :: b
   type (T_time) ::rv

    rv%value = a + b%value

   end function ADD__secondsR8_time

   function ADD__secondsI_time(a,b) RESULT(rv) ! Time = Time + integer seconds
   integer, intent(in) :: a
   type (T_time), intent(in) :: b
   type (T_time) ::rv

    rv%value = a + b%value

   end function ADD__secondsI_time

   function ADD__datetime_interval(t,dt) RESULT(rv) ! Time = Time + interval
   type (T_Datetime), intent(in) :: t
   type (T_timeInterval), intent(in) :: dt
   type (T_time) :: rv

    rv = As_Time(t) + dt

   end function add__datetime_interval

   function ADD__interval_datetime(dt,t) RESULT(rv) ! Time = Time + interval
   type (T_timeInterval), intent(in) :: dt
   type (T_Datetime), intent(in) :: t
   type (T_time) :: rv

    rv = As_Time(t) + dt

   end function ADD__interval_datetime

     function ADD_DATETIME_YEAR( t, b ) result(rv)
     type (T_Datetime), intent(in) :: t
     integer, intent(in) :: b
     Type (T_Datetime) :: rv
     rv = t
     rv%year = rv%year + b
     end function ADD_DATETIME_YEAR

     function ADD_DATETIME_MONTH( t, b ) result(rv)
     type (T_Datetime), intent(in) :: t
     integer, intent(in) :: b
     Type (T_Datetime) :: rv

     rv = t
     rv%month = rv%month + b
     do while ( rv%month > 12 )
       rv%month = rv%month - 12
       rv%year = rv%year + 1
     enddo
     do while ( rv%month <= 0 )
       rv%month = rv%month + 12
       rv%year = rv%year - 1
     enddo

     end function ADD_DATETIME_MONTH

     function ADD_DATETIME_DAY( t, b ) result(rv)
     type (T_Datetime), intent(in) :: t
     integer, intent(in) :: b
     Type (T_Datetime) :: rv
     rv = t
     rv%day = rv%day + b

     do while ( rv%day > Z_DAYSINM(rv%month, rv%year) )
       rv%day = rv%day - Z_DAYSINM(rv%month, rv%year)
       rv= ADD_DATETIME_MONTH( rv, 1 )
     enddo

     do while ( rv%day < 0 )
       ! Rely on Z_DAYSINM(0,1990) = Z_DAYSINM(12,1989)
       rv%day = rv%day + Z_DAYSINM(rv%month-1, rv%year)
       rv= ADD_DATETIME_MONTH( rv, -1 )
     enddo

     end function ADD_DATETIME_DAY

     function ADD_DATETIME_HOUR( t, b ) result(rv)
     type (T_Datetime), intent(in) :: t
     integer, intent(in) :: b
     Type (T_Datetime) :: rv

     rv = AS_Time(t) + 3600.*b

     end function ADD_DATETIME_HOUR

     function ADD_DATETIME_MINUTE( t, b ) result(rv)
     type (T_Datetime), intent(in) :: t
     integer, intent(in) :: b
     Type (T_Datetime) :: rv
     rv = As_Time(t) + 60.*b
     end function ADD_DATETIME_MINUTE

     function ADD_DATETIME_SECONDS( t, b ) result(rv)
     type (T_Datetime), intent(in) :: t
     integer, intent(in) :: b
     Type (T_Datetime) :: rv
       rv = As_Time(t) + b
     end function ADD_DATETIME_SECONDS

   function ADD__time_dinterval(t,dt) result(rv)
   type (T_Time), intent(in) :: t
   type (T_DateTimeInterval),intent(in) :: dt
   type (T_time) :: rv
   rv = As_time( Add__datetime_dinterval( As_datetime(t), dt ) )
   end function ADD__time_dinterval

   function ADD__dinterval_time(dt,t) result(rv)
   type (T_Time), intent(in) :: t
   type (T_DateTimeInterval),intent(in) :: dt
   type (T_time) :: rv
   rv = ADD__time_dinterval(t,dt)
   end function ADD__dinterval_time

   function ADD__datetime_dinterval(t,dt) RESULT(rv)
   type (T_Datetime), intent(in) :: t
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_Datetime) :: rv

   type (T_datetime) :: t2
   type (T_Time) :: tim
   integer :: iy,im
   real(kind=8) :: fracy,fracy1,fracy2
   real(kind=8) :: fracm,fracm1,fracm2
   real(kind=8) :: s2

   !
   ! Add years, then months, then days,hours,minutes,seconds
   !
   ! So adding 1 year and 1.5 months to 0ZJan 1, 1987
   ! will go to 1988, then to February, then notice that
   ! Feb 88 is a leap year month, and go to 12Z on the 14th
   ! Adding 1 year and 1.5 months to 0ZJan 1, 1988
   ! will go to 0ZJan 14, 1989
   !


    iy = FLOOR(dt%years)
    fracy = dt%years - iy
    if ( fracy < 1.E-05 ) fracy = 0.
    if ( fracy > .99999 ) then
       fracy = 0.
       iy = iy+1
    endif


    ! If we are adding an integer number of years, then
    ! Just bump the year counter.

    if ( fracy == 0. ) then

       t2 = t
       t2%year = t2%year + iy

    else


       fracy1 = Z_DAY_OF_YEAR_DATETIME(t) + &
                ( Z_SEC_OF_DAY_DATETIME(t)/86400.D0) ! As days
       if ( leap(t%year) ) then
         fracy1 = fracy1 / 366.
       else
         fracy1 = fracy1 / 365.
       endif

       fracy2 = fracy1 + fracy
       do while ( fracy2 > 1. )
         fracy2 = fracy2 - 1.
         iy = iy + 1
       enddo

      t2 = t
       t2%year = t2%year + iy ! Add the integer part of the year
       t2%month = 1
       t2%day = 1
       t2%hour = 0
       t2%minute = 0
       t2%second = 0.
       tim = AS_time(t2)
       if ( leap(t2%year) ) then
         tim%value = tim%value + fracy2 * 366.D0 * 86400.D0
       else
         tim%value = tim%value + fracy2 * 365.D0 * 86400.D0
       endif
       t2 = As_Datetime(tim)
     endif


    ! Now Add months


    im = FLOOR(dt%months)
    fracm = dt%months - im
    if ( fracm < 1.E-05 ) fracm = 0.
    if ( fracm > .99999 ) then
       fracm = 0.
       im = im+1
    endif

    if( fracm == 0. ) then
       rv = ADD_DATETIME_MONTH( t2,im)
    else

       ! Now determine the present fraction of the month

       fracm1 = ( z_sec_of_day_datetime(t2) + t2%day*86400. ) &
                 / ( 86400.*Z_DAYSINM(t2))

       fracm2 = fracm1 + fracm


       do while ( fracm2 > 1 )
         fracm2 = fracm2 - 1.
         im = im + 1
       enddo

       rv = ADD_DATETIME_MONTH(t2,im)

       s2 = 86400.*Z_DAYSINM(rv) * fracm2
       rv%day = 1
       rv%hour = 0
       rv%minute = 0
       rv%second = 0.
       rv = As_time(rv) + As_TimeInterval(s2)
    endif


    ! Now add any seconds, minutes, hours and days.

    s2 = dt%seconds + 60.*( dt%minutes + 60.*( dt%hours + 24.*dt%days))

    rv = rv + As_TimeInterval(s2)

   end function ADD__datetime_dinterval

   function ADD__dinterval_datetime(dt,t) RESULT(rv)
   type (T_Datetime), intent(in) :: t
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_Datetime) :: rv

    rv = ADD__datetime_dinterval(t,dt)

   end function ADD__dinterval_datetime



   function ADD__datetime_secondsR4(a,b) RESULT(rv) ! Time = Time + interval
   type (T_Datetime), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(a) + b

   end function ADD__datetime_secondsR4

   function ADD__datetime_secondsR8(a,b) RESULT(rv) ! Time = Time + interval
   type (T_Datetime), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(a) + b

   end function ADD__datetime_secondsR8

   function ADD__datetime_secondsI(a,b) RESULT(rv) ! Time = Time + interval
   type (T_Datetime), intent(in) :: a
   integer, intent(in) :: b
   type (T_Datetime) :: rv
   type (T_time) :: t

    rv = As_Time(a) + b

   end function ADD__datetime_secondsI

   function ADD__secondsR4_datetime(a,b) RESULT(rv) ! Time = Time + interval
   real(kind=4), intent(in) :: a
   type (T_Datetime), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(b) + a

   end function ADD__secondsR4_datetime

   function ADD__secondsR8_datetime(a,b) RESULT(rv) ! Time = Time + interval
   real(kind=8), intent(in) :: a
   type (T_Datetime), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(b) + a

   end function ADD__secondsR8_datetime

   function ADD__secondsI_datetime(a,b) RESULT(rv) ! Time = Time + interval
   integer, intent(in) :: a
   type (T_Datetime), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(b) + a

   end function ADD__secondsI_datetime


   function SUB__time_time(a,b) RESULT(rv) ! Interval = time - time
   type (T_time), intent(in) :: a
   type (T_time), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value - b%value

   end function SUB__time_time

   function SUB__time_interval(a,b) RESULT(rv) ! Time = Time - Interval
   type (T_time), intent(in) :: a
   type (T_timeInterval), intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value - b%value

   end function SUB__time_interval

   function SUB__time_dinterval(t,dt) RESULT(rv) ! Time = Time - Interval
   type (T_time), intent(in) :: t
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_time) :: rv
   type (T_DateTimeInterval) :: mdt

    mdt =(-1)*dt
    rv = As_time( Add__datetime_dinterval( As_datetime(t), mdt ) )

   end function SUB__time_dinterval

   function SUB__datetime_dinterval(t,dt) RESULT(rv) ! Time = Time - Interval
   type (T_Datetime), intent(in) :: t
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_Datetime) :: rv
   type (T_DateTimeInterval) :: mdt

    mdt =(-1)*dt
    rv = Add__datetime_dinterval( As_datetime(t), mdt )

   end function SUB__datetime_dinterval

   function SUB__time_secondsR4(a,b) RESULT(rv) ! Time = Time - Interval
   type (T_time), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value - b

   end function SUB__time_secondsR4

   function SUB__time_secondsR8(a,b) RESULT(rv) ! Time = Time - Interval
   type (T_time), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value - b

   end function SUB__time_secondsR8

   function SUB__time_secondsI(a,b) RESULT(rv) ! Time = Time - Interval
   type (T_time), intent(in) :: a
   integer, intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value - b

   end function SUB__time_secondsI

   function sub__interval_interval(a,b) RESULT(rv) ! Interval = Interval - Interval
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   type (T_timeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value - b%value

   end function sub__interval_interval

   function sub__interval_secondsR4(a,b) RESULT(rv) ! Interval = Interval - seconds
   type (T_timeInterval), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value - b

   end function sub__interval_secondsR4

   function sub__interval_secondsR8(a,b) RESULT(rv) ! Interval = Interval - seconds
   type (T_timeInterval), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value - b

   end function sub__interval_secondsR8

   function sub__interval_secondsI(a,b) RESULT(rv) ! Interval = Interval - seconds
   type (T_timeInterval), intent(in) :: a
   integer, intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value - b

   end function sub__interval_secondsI


   function sub__datetime_datetime(a,b) RESULT(rv) ! Interval = datetime - datetime
   type (T_dateTime), intent(in) :: a
   type (T_dateTime), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = As_Time(a) - As_Time(b)

   end function sub__datetime_datetime

   function sub__datetime_interval(a,b) RESULT(rv) ! time = datetime - datetime
   type (T_dateTime), intent(in) :: a
   type (T_TimeInterval), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(a) - b

   end function sub__datetime_interval

   function sub__datetime_secondsR4(a,b) RESULT(rv) ! time = datetime - seconds
   type (T_dateTime), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(a) - b

   end function sub__datetime_secondsR4

   function sub__datetime_secondsR8(a,b) RESULT(rv) ! time = datetime - seconds
   type (T_dateTime), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(a) - b

   end function sub__datetime_secondsR8

   function sub__datetime_secondsI(a,b) RESULT(rv) ! time = datetime - seconds
   type (T_dateTime), intent(in) :: a
   integer, intent(in) :: b
   type (T_time) :: rv

    rv = As_Time(a) - b

   end function sub__datetime_secondsI

   function sub__datetime_time(a,b) RESULT(rv) ! interval = datetime - time
   type (T_dateTime), intent(in) :: a
   type (T_Time), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = As_Time(a) - b

   end function sub__datetime_time

   function sub__time_datetime(a,b) RESULT(rv) ! interval = time - datetime
   type (T_Time), intent(in) :: a
   type (T_dateTime), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = a - As_Time(b)

   end function sub__time_datetime

   function sub__clock_time(a,b) RESULT(rv) ! interval = time - datetime
   type (T_Clock), intent(in) :: a
   type (T_Time), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = As_Time(a) - b

   end function sub__clock_time


   function sub__clock_datetime(a,b) RESULT(rv) ! interval = time - datetime
   type (T_Clock), intent(in) :: a
   type (T_dateTime), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = As_Time(a) - As_Time(b)

   end function sub__clock_datetime

   function sub__clock_clock(a,b) RESULT(rv) ! interval = time - datetime
   type (T_Clock), intent(in) :: a
   type (T_Clock), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = As_Time(a) - As_Time(b)

   end function sub__clock_clock

   function SUB_TIMES4(a,b) RESULT(rv) ! Time = Time - real
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_time), intent(in) :: a
   real, intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value - b

   end function sub_times4

   function SUB_TIMES5(a,b) RESULT(rv) ! Time = Time - integer
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_time), intent(in) :: a
   integer, intent(in) :: b
   type (T_time) :: rv

    rv%value = a%value - b

   end function sub_times5

   function MULT__INTERVAL_REAL4(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value * b

   end function MULT__INTERVAL_REAL4

   function MULT__INTERVAL_REAL8(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value * b

   end function MULT__INTERVAL_REAL8

   function MULT__INTERVAL_INT(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   integer, intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value * b

   end function MULT__INTERVAL_INT

   function MULT__REAL4_INTERVAL(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   real(kind=4), intent(in) :: a
   type (T_timeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a * b%value

   end function MULT__REAL4_INTERVAL

   function MULT__REAL8_INTERVAL(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   real(kind=8), intent(in) :: a
   type (T_timeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a * b%value

   end function MULT__REAL8_INTERVAL

   function MULT__INT_INTERVAL(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   integer, intent(in) :: a
   type (T_timeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a * b%value

   end function MULT__INT_INTERVAL

   function MULT__DINTERVAL_REAL4(dt,a) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   real(kind=4), intent(in) :: a
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_DatetimeInterval) :: rv

    rv%years = a * dt%years
    rv%months = a * dt%months
    rv%days = a * dt%days
    rv%hours = a * dt%hours
    rv%minutes = a * dt%minutes
    rv%seconds = a * dt%seconds

   end function MULT__DINTERVAL_REAL4

   function MULT__DINTERVAL_REAL8(dt,a) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   real(kind=8), intent(in) :: a
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_DatetimeInterval) :: rv

    rv%years = a * dt%years
    rv%months = a * dt%months
    rv%days = a * dt%days
    rv%hours = a * dt%hours
    rv%minutes = a * dt%minutes
    rv%seconds = a * dt%seconds

   end function MULT__DINTERVAL_REAL8

   function MULT__DINTERVAL_INT(dt,a) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   integer, intent(in) :: a
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_DatetimeInterval) :: rv

    rv%years = a * dt%years
    rv%months = a * dt%months
    rv%days = a * dt%days
    rv%hours = a * dt%hours
    rv%minutes = a * dt%minutes
    rv%seconds = a * dt%seconds

   end function MULT__DINTERVAL_INT

   function MULT__REAL4_DINTERVAL(a,dt) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   real(kind=4), intent(in) :: a
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_DatetimeInterval) :: rv

    rv%years = a * dt%years
    rv%months = a * dt%months
    rv%days = a * dt%days
    rv%hours = a * dt%hours
    rv%minutes = a * dt%minutes
    rv%seconds = a * dt%seconds

   end function MULT__REAL4_DINTERVAL

   function MULT__REAL8_DINTERVAL(a,dt) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   real(kind=8), intent(in) :: a
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_DatetimeInterval) :: rv

    rv%years = a * dt%years
    rv%months = a * dt%months
    rv%days = a * dt%days
    rv%hours = a * dt%hours
    rv%minutes = a * dt%minutes
    rv%seconds = a * dt%seconds

   end function MULT__REAL8_DINTERVAL

   function MULT__INT_DINTERVAL(a,dt) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   integer, intent(in) :: a
   type (T_DatetimeInterval), intent(in) :: dt
   type (T_DatetimeInterval) :: rv

    rv%years = a * dt%years
    rv%months = a * dt%months
    rv%days = a * dt%days
    rv%hours = a * dt%hours
    rv%minutes = a * dt%minutes
    rv%seconds = a * dt%seconds

   end function MULT__INT_DINTERVAL

   function DIV__INTERVAL_REAL4(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value / b

   end function DIV__INTERVAL_REAL4

   function DIV__INTERVAL_REAL8(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value / b

   end function DIV__INTERVAL_REAL8

   function DIV__INTERVAL_INT(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- subtract two time numbers and return a time number
   type (T_timeInterval), intent(in) :: a
   integer, intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = a%value / b

   end function DIV__INTERVAL_INT

   function DIV__DINTERVAL_REAL4(dt,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- Divide each component of date time interval by b
   type (T_DatetimeInterval), intent(in) :: dt
   real(kind=4), intent(in) :: b
   type (T_DatetimeInterval) :: rv

    rv%years = dt%years / b
    rv%months = dt%months / b
    rv%days = dt%days / b
    rv%hours = dt%hours / b
    rv%minutes = dt%minutes / b
    rv%seconds = dt%seconds / b

   end function DIV__DINTERVAL_REAL4


   function DIV__DINTERVAL_REAL8(dt,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- Divide each component of date time interval by b
   type (T_DatetimeInterval), intent(in) :: dt
   real(kind=8), intent(in) :: b
   type (T_DatetimeInterval) :: rv

    rv%years = dt%years / b
    rv%months = dt%months / b
    rv%days = dt%days / b
    rv%hours = dt%hours / b
    rv%minutes = dt%minutes / b
    rv%seconds = dt%seconds / b

   end function DIV__DINTERVAL_REAL8

   function DIV__DINTERVAL_INT(dt,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- Divide each component of date time interval by b
   type (T_DatetimeInterval), intent(in) :: dt
   integer, intent(in) :: b
   type (T_DatetimeInterval) :: rv

    rv%years = dt%years / b
    rv%months = dt%months / b
    rv%days = dt%days / b
    rv%hours = dt%hours / b
    rv%minutes = dt%minutes / b
    rv%seconds = dt%seconds / b

   end function DIV__DINTERVAL_INT





   function eql__time_time(a,b) RESULT(rv)
   type (T_time), intent(in) :: a,b
   logical :: rv
   type (T_TimeInterval) :: dt

    dt = a - b
    rv = ABS(dt%value) < accuracy_factor

   end function eql__time_time

   function eql__interval_interval(a,b) RESULT(rv)
   type (T_timeInterval), intent(in) :: a,b
   logical :: rv

    rv = (a%value .eq. b%value)

   end function eql__interval_interval

   function eql__time_datetime(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- check if two ttimes are equal
   type (T_time), intent(in) :: a
   type (T_DateTime), intent(in) :: b
   logical :: rv
   type (T_TimeInterval) :: dt

    dt = a - b
    rv = ABS(dt%value) < accuracy_factor

   end function eql__time_datetime

   function eql__datetime_time(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- check if two ttimes are equal
   type (T_Datetime), intent(in) :: a
   type (T_Time), intent(in) :: b
   logical :: rv
   type (T_TimeInterval) :: dt

    dt = a - b
    rv = ABS(dt%value) < accuracy_factor

   end function eql__datetime_time

   function eql__datetime_datetime(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- check if two ttimes are equal
   type (T_Datetime), intent(in) :: a
   type (T_DateTime), intent(in) :: b
   logical :: rv
   type (T_TimeInterval) :: dt

    dt = a - b
    rv = ABS(dt%value) < accuracy_factor

   end function eql__datetime_datetime

   function eql__dti_dti(a,b) RESULT(rv)
   !*---------------------------------------------------------------
   !*-- check if two datetimeIntervals are equal
   type (T_DatetimeInterval), intent(in) :: a
   type (T_DateTimeInterval), intent(in) :: b
   logical :: rv

    rv = a%years == b%years .and. a%months == b%months .and. &
         a%days == b%days .and. a%hours == b%hours .and. &
         a%minutes == b%minutes .and. a%seconds == b%seconds
   end function eql__dti_dti

   function MOD__time_interval(a,b) RESULT(rv) ! Interval = Time MOD Interval
   type (T_time), intent(in) :: a
   type (T_TimeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = MOD(a%value,b%value)

   end function MOD__time_interval

   function MOD__datetime_interval(a,b) RESULT(rv) ! Interval = Time MOD Interval
   type (T_datetime), intent(in) :: a
   type (T_TimeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv = MOD__time_interval( As_Time(a), b )

   end function MOD__datetime_interval

   function MOD__interval_interval(a,b) RESULT(rv) ! Interval = Interval MOD Interval
   type (T_timeInterval), intent(in) :: a
   type (T_TimeInterval), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = MOD(a%value,b%value)

   end function MOD__interval_interval

   function MOD__interval_secondsR4(a,b) RESULT(rv) ! Interval = Interval MOD Interval
   type (T_timeInterval), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_timeInterval) :: rv
   real(kind=8) :: dt

    rv%value = MOD(a%value,REAL(b,KIND=8))

   end function MOD__interval_secondsR4

   function MOD__interval_secondsR8(a,b) RESULT(rv) ! Interval = Interval MOD Interval
   type (T_timeInterval), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_timeInterval) :: rv
   real(kind=8) :: dt

    rv%value = MOD(a%value,REAL(b,KIND=8))

   end function MOD__interval_secondsR8

   function MOD__interval_secondsI(a,b) RESULT(rv) ! Interval = Interval MOD Interval
   type (T_timeInterval), intent(in) :: a
   integer, intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = MOD(a%value,REAL(b,KIND=8))

   end function MOD__interval_secondsI

   function MOD__time_secondsR4(a,b) RESULT(rv) ! Interval = Time MOD real seconds
   type (T_time), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = MOD(a%value,REAL(b,KIND=8))

   end function MOD__time_secondsR4

   function MOD__time_secondsR8(a,b) RESULT(rv) ! Interval = Time MOD real seconds
   type (T_time), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = MOD(a%value,REAL(b,KIND=8))

   end function MOD__time_secondsR8

   function MOD__datetime_secondsR4(a,b) RESULT(rv) ! Interval = Time MOD real seconds
   type (T_datetime), intent(in) :: a
   real(kind=4), intent(in) :: b
   type (T_timeInterval) :: rv

     rv = MOD_times( As_Time(a), b )

   end function MOD__datetime_secondsR4

   function MOD__datetime_secondsR8(a,b) RESULT(rv) ! Interval = Time MOD real seconds
   type (T_datetime), intent(in) :: a
   real(kind=8), intent(in) :: b
   type (T_timeInterval) :: rv

     rv = MOD_times( As_Time(a), b )

   end function MOD__datetime_secondsR8

   function MOD__time_secondsI(a,b) RESULT(rv) ! Interval = Time MOD integer seconds
   type (T_time), intent(in) :: a
   integer, intent(in) :: b
   type (T_timeInterval) :: rv

    rv%value = MOD(a%value,REAL(b,KIND=8))

   end function MOD__time_secondsI

   function MOD__datetime_secondsI(a,b) RESULT(rv) ! Interval = Time MOD real seconds
   type (T_datetime), intent(in) :: a
   integer, intent(in) :: b
   type (T_timeInterval) :: rv

     rv = MOD_times( As_Time(a), b )

   end function MOD__datetime_secondsI

   character(len=3) function CZ_MONTH_TIME (time )
   !*-------------------------------------------------
   !*-- return the 3 char values for the current month
   type (T_time), intent(in) :: time

   type (T_Datetime) :: datetime

     datetime = time
     cz_month_time = month_name( datetime%month )

   end function cz_month_time

   character(len=3) function CZ_MONTH_DATETIME (datetime)
   !*-------------------------------------------------
   !*-- return the 3 char values for the current month
   type (T_Datetime), intent(in) :: datetime

     cz_month_datetime = month_name( datetime%month )

   end function cz_month_datetime

   character(len=3) function CZ_MONTH_INT (month)
   !*-------------------------------------------------
   !*-- return the 3 char values for the current month
   integer, intent(in) :: month

     CZ_MONTH_INT = month_name( month )

   end function CZ_MONTH_INT




         subroutine Z_TIME_TO_DATE_TIME(time, years, months, days, &
                                       hours, minutes, seconds, status )
         !*---------------------------------------------------------------
         !*-- convert a ttime to yy-mm-dd hh:mm:ss
         type (T_time), intent(in) :: time
         integer, intent(out) :: months, years, days
         integer, intent(out) :: hours, minutes, seconds
         integer, intent(out), optional :: status

         type (T_Datetime) :: d

          if (present(status)) status = 0
          d = time
          years = d%year
          months = d%month
          days = d%day
          hours = d%hour
          minutes = d%minute
          seconds = d%second

         end subroutine z_time_to_date_time

         subroutine Z_TIME_TO_DATE_CLOCK(clock, years, months, days, &
                                       hours, minutes, seconds, status )
         !*---------------------------------------------------------------
         !*-- convert a tclock to yy-mm-dd hh:mm:ss
         type (T_Clock), intent(in) :: clock
         integer, intent(out) :: months, years, days
         integer, intent(out) :: hours, minutes, seconds
         integer, intent(out), optional :: status

         type (T_Datetime) :: d

          if (present(status)) status = 0
          d = clock%time
          years = d%year
          months = d%month
          days = d%day
          hours = d%hour
          minutes = d%minute
          seconds = d%second

         end subroutine z_time_to_date_clock

         subroutine Z_DATE_TO_TIME(time, years, months, days, &
                                       hours, minutes, seconds, status)
         !*----------------------------------------------------------------
         !*-- convert yy-mm-dd hh:mm:ss to a ttime
         type (T_time), intent(out) :: time
         integer, intent(in) :: months, years, days
         integer, intent(in) :: hours, minutes, seconds
         integer, intent(out), optional :: status

         if ( present(status) ) then
          time=AS_TIME_6INT(years,months,days,hours,minutes,seconds,status)
         else
          time=AS_TIME_6INT(years,months,days,hours,minutes,seconds)
         endif

         end subroutine Z_DATE_TO_TIME

         type (T_Time) function AS_TIME_TIME( time, status)
         !*---------------------------------------
         !*-- return the ttime value
         type (T_Time),intent(in) :: time
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          as_time_time = time

         end function as_time_time





         function AS_TIME_DATETIME( time, status) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         type (T_Datetime),intent(in) :: time
         integer,intent(out),optional :: status
         type (T_Time) :: rv

          if ( present(status) ) status = 0
          rv%value = z_julian_day_datetime ( time ) * 86400.0
          if ( .not. ORIGIN_J ) rv%value = rv%value - JULIAN_OFFSET*86400.

         end function as_time_datetime



         function AS_TIME_6INT(year,month,day,hour,minute,second,status) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         integer,intent(in) :: year, month, day, hour, minute, second
         integer,intent(out),optional :: status
         type (T_Time) :: rv

          if ( present(status) ) status = 0
          rv%value = z_julian_day( year, month, day, hour, minute, second) * &
                                         86400.0
          if ( .not. ORIGIN_J ) rv%value = rv%value - JULIAN_OFFSET*86400.

         end function as_time_6int

         function AS_TIME_5INT(year,month,day,hour,minute,second,status) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         integer,intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status
         type (T_Time) :: rv

          if ( present(status) ) status = 0
          rv%value = z_julian_day( year, month, day, hour, minute, second) * &
                                         86400.0
          if ( .not. ORIGIN_J ) rv%value = rv%value - JULIAN_OFFSET*86400.

         end function as_time_5int

         function AS_TIME_6REAL(year,month,day,hour, minute, second, status) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         real, intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status
         type (T_Time) :: rv

          if ( present(status) ) status = 0
          rv%value = z_julian_day( year, month, day, hour, minute, second) * &
                                         86400.0
          if ( .not. ORIGIN_J ) rv%value = rv%value - JULIAN_OFFSET*86400.

         end function as_time_6real

         function AS_TIME_INT( value, seconds, days, julian, daysad ) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         integer,intent(in) :: value
         logical,intent(in),optional :: seconds, days, julian, daysad
         type (T_Time) :: rv

          include "zeus_clocks.astime.code"

          rv%value = rsec

         end function as_time_int


         function AS_TIME_REAL4( value, seconds, days, julian, daysad ) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         real (kind=4),intent(in) :: value
         logical,intent(in),optional :: seconds, days, julian, daysad
         type (T_Time) :: rv

          include "zeus_clocks.astime.code"

         end function as_time_real4

         function AS_TIME_REAL8( value, seconds, days, julian, daysad ) RESULT(rv)
         !*---------------------------------------
         !*-- return the ttime value
         real (kind=8),intent(in) :: value
         logical,intent(in),optional :: seconds, days, julian, daysad
         type (T_Time) :: rv

          include "zeus_clocks.astime.code"

         end function as_time_real8

         function AS_TIMEINTERVAL_REAL4( seconds, days ) RESULT(rv)
         real(kind=4), intent(in) :: seconds
         logical, intent(in), optional :: days
         type (T_TimeInterval) :: rv
          rv = seconds
          if ( present(days)) then
           if (days) rv = seconds*86400.
          endif
         end function AS_TIMEINTERVAL_REAL4

         function AS_TIMEINTERVAL_REAL8( seconds, days ) RESULT(rv)
         real(kind=8), intent(in) :: seconds
         logical, intent(in), optional :: days
         type (T_TimeInterval) :: rv
          rv = seconds
          if ( present(days)) then
           if (days) rv = seconds*86400.
          endif
         end function AS_TIMEINTERVAL_REAL8

         function AS_TIMEINTERVAL_INT( seconds, days ) RESULT(rv)
         integer, intent(in) :: seconds
         logical, intent(in), optional :: days
         type (T_TimeInterval) :: rv
          rv = seconds
          if ( present(days)) then
           if (days) rv = seconds*86400.
          endif
         end function AS_TIMEINTERVAL_INT


    ! Return time values as seconds
    ! If the time origin is julian and the request is in seconds of
    ! the modern epoch, subtract the JULIAN_OFSET
    ! If the time origin is Modern epoch, and the request is in julian
    ! add the JULIAN_OFSET


         function AS_SECONDS_TIME( time, julian, AD, status ) RESULT(rv)
         type (T_Time),intent(in) :: time
         logical, optional, intent(in) :: julian,AD
         integer,intent(out),optional :: status
         real(kind=8) :: rv
         logical julian_

          julian_ = origin_j
          if ( present(julian) ) julian_ = julian
          if ( present(AD) ) julian_ = .not. AD

          if ( present(status) ) status = 0
          rv = time

          if ( origin_j .NEQV. julian_ ) then
            if ( julian_ ) then
              rv = rv + 86400.*JULIAN_OFFSET
            else
              rv = rv - 86400.*JULIAN_OFFSET
            endif
          endif

         end function as_seconds_time

         function AS_SECONDS_DATETIME( time, julian, AD, status ) RESULT(rv)
         type (T_DateTime),intent(in) :: time
         logical, optional, intent(in) :: julian,AD
         integer,intent(out),optional :: status
         real(kind=8) :: rv
         logical julian_

          julian_ = origin_j
          if ( present(julian) ) julian_ = julian
          if ( present(AD) ) julian_ = .not. AD


          if ( present(status) ) status = 0
          rv = time
          if ( origin_j .NEQV. julian_ ) then
            if ( julian_ ) then
              rv = rv + 86400.*JULIAN_OFFSET
            else
              rv = rv - 86400.*JULIAN_OFFSET
            endif
          endif


         end function as_seconds_datetime


         function AS_SECONDS_INTERVAL( interval ) RESULT(rv)
         type (T_TimeInterval),intent(in) :: interval
         real :: rv

          rv = interval

         end function as_seconds_interval

    ! Return time values as days
    ! If the time origin is julian and the request is in days of
    ! the modern epoch, subtract the JULIAN_OFSET
    ! If the time origin is Modern epoch, and the request is in julian
    ! add the JULIAN_OFSET


         function AS_days_TIME( time, julian, AD, status ) RESULT(rv)
         type (T_Time),intent(in) :: time
         logical, optional, intent(in) :: julian,AD
         integer,intent(out),optional :: status
         real(kind=8) :: rv
         logical julian_

          julian_ = origin_j
          if ( present(julian) ) julian_ = julian
          if ( present(AD) ) julian_ = .not. AD

          if ( present(status) ) status = 0
          rv = time%value/86400.

          if ( origin_j .NEQV. julian_ ) then
            if ( julian_ ) then
              rv = rv + JULIAN_OFFSET
            else
              rv = rv - JULIAN_OFFSET
            endif
          endif

         end function as_days_time

         function AS_days_DATETIME( time, julian, AD, status ) RESULT(rv)
         type (T_DateTime),intent(in) :: time
         logical, optional, intent(in) :: julian,AD
         integer,intent(out),optional :: status
         real(kind=8) :: rv
         logical julian_

          julian_ = origin_j
          if ( present(julian) ) julian_ = julian
          if ( present(AD) ) julian_ = .not. AD


          if ( present(status) ) status = 0
          rv = time
          rv = rv/86400.
          if ( origin_j .NEQV. julian_ ) then
            if ( julian_ ) then
              rv = rv + JULIAN_OFFSET
            else
              rv = rv - JULIAN_OFFSET
            endif
          endif


         end function as_days_datetime


         function AS_days_INTERVAL( interval ) RESULT(rv)
         type (T_TimeInterval),intent(in) :: interval
         real :: rv

          rv = interval%value/86400.

         end function as_days_interval


         function AS_DATETIME_TIME(time) RESULT(rv)
         type (T_Time),intent(in) :: time
         type (T_Datetime) :: rv
          rv = time
         end function as_datetime_time


         function AS_DATETIME_DATETIME(time) RESULT(rv)
         !*---------------------------------------
         !*-- return the tdatetime value
         type (T_Datetime),intent(in) :: time
         type (T_Datetime) :: rv
          rv = time
         end function as_datetime_datetime

         function AS_DATETIME_6INT(year,month,day,hour, &
                                   minute,second,status) RESULT(rv)
         integer,intent(in) :: year, month, day, hour, minute, second
         integer,intent(out),optional :: status
         type (T_Datetime) :: rv

          if ( present(status) ) status = 0
          rv = time_as_datetime( year, month, day, hour, minute, second )

         end function as_datetime_6int

         function AS_DATETIME_5INT(year,month,day,hour, &
                                   minute,second,status) RESULT(rv)
         integer,intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status
         type (T_Datetime) :: rv

          if ( present(status) ) status = 0
          rv = time_as_datetime( year, month, day, hour, minute, second )

         end function as_datetime_5int

         function AS_DATETIME_6REAL(year,month,day,hour, &
                                    minute,second,status) RESULT(rv)
         real, intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status
         type (T_Datetime) :: rv

          if ( present(status) ) status = 0
          rv = time_as_datetime( year, month, day, hour, minute, second )

         end function as_datetime_6real


         ! Specify a number and return a time
         ! by specifying SECONDS or DAYS, and JULIAN or DAYSAD
         ! the time can be defined explicitly

         function AS_DATETIME_INT4(value,seconds,days, julian, daysad ) RESULT(rv)
         integer (kind=4),intent(in) :: value
         logical,intent(in),optional :: seconds, days, julian, daysad
         type (T_Datetime) :: rv

          include "zeus_clocks.astime.code"

         end function as_datetime_int4

         function AS_DATETIME_INT8(value,SECONDS, DAYS, JULIAN, DAYSAD ) RESULT(rv)
         integer (kind=8),intent(in) :: value
         logical,intent(in),optional :: SECONDS, DAYS, JULIAN, DAYSAD
         type (T_Datetime) :: rv

          include "zeus_clocks.astime.code"

         end function as_datetime_int8

         function AS_DATETIME_REAL4(value,SECONDS,DAYS,JULIAN, DAYSAD ) RESULT(rv)
         !*---------------------------------------
         !*-- return the tdatetime value
         real (kind=4),intent(in) :: value
         logical,intent(in),optional :: SECONDS, DAYS, JULIAN, DAYSAD
         type (T_Datetime) :: rv

          include "zeus_clocks.astime.code"

         end function as_datetime_real4

         function AS_DATETIME_REAL8(value,seconds,days,julian,daysad ) RESULT(rv)
         !*---------------------------------------
         !*-- return the tdatetime value
         real (kind=8),intent(in) :: value
         logical,intent(in),optional :: seconds, days, julian, daysad
         type (T_Datetime) :: rv

          include "zeus_clocks.astime.code"

         end function as_datetime_real8

         character(len=12) function CZ_DATE_TIME(time, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_time),intent(in) :: time
         integer, intent(out), optional :: status

          integer year, month, day, hour, minute, second
          character(len=12) ctimlin

          if ( present(status) ) status = 0
          call z_time_to_date(time, year, month, day, hour, minute, second)
          write (FMT=22,unit=ctimlin) cz_month(month), day, year
         22 FORMAT(" ",A3," ",I2,",",I4.4)
          cz_date_time = ctimlin

         end function cz_date_time

         character(len=12) function CZ_DATE_DATETIME(time, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_Datetime),intent(in) :: time
         integer, intent(out), optional :: status

          integer year, month, day
          character(len=12) ctimlin

          if ( present(status) ) status = 0
          year = time%year
          month = time%month
          day = time%day
          write (FMT=24,unit=ctimlin) cz_month(month), day, year
         24 FORMAT(" ",A3," ",I2,",",I4.4)
          cz_date_datetime = ctimlin

         end function cz_date_datetime

         character(len=12) function CZ_DATE_alarm(alarm, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_alarm),intent(in) :: alarm
         integer, intent(out), optional :: status

          if ( present(status) ) status = 0
          cz_date_alarm = cz_date_time(as_time(alarm))

         end function cz_date_alarm

         character(len=12) function CZ_DATE_CLOCK(clock, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_Clock),intent(in) :: clock
         integer, intent(out), optional :: status

          if ( present(status) ) status = 0
          cz_date_clock = cz_date_time(clock%time)

         end function cz_date_clock

         character(len=8) function CZ_TIME_TIME(time, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_time),intent(in) :: time
         integer, intent(out), optional :: status

          integer year, month, day, hour, minute, second
          character(len=8) ctimlin

          if ( present(status) ) status = 0
          call z_time_to_date(time, year, month, day, hour, minute, second)
          write (FMT='(I2.2,":",I2.2,":",I2.2)',&
                 UNIT=ctimlin) hour, minute, second
          cz_time_time = ctimlin

         end function cz_time_time

         character(len=8) function CZ_TIME_DATETIME(time, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_Datetime),intent(in) :: time
         integer, intent(out), optional :: status

          integer hour, minute, second
          character(len=8) ctimlin

          if (present(status)) status = 0
          hour = time%hour
          minute = time%minute
          second = time%second
          write (FMT='(I2.2,":",I2.2,":",I2.2)',&
                 UNIT=ctimlin) hour, minute, second
          cz_time_datetime = ctimlin

         end function cz_time_datetime

         character(len=8) function CZ_TIME_CLOCK(clock, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_Clock),intent(in) :: clock
         integer, intent(out), optional :: status

          if (present(status)) status = 0
          cz_time_clock = cz_time_time(clock%time)

         end function cz_time_clock

         character(len=8) function CZ_TIME_alarm(alarm, status )
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_alarm),intent(in) :: alarm
         integer, intent(out), optional :: status

          if (present(status)) status = 0
          cz_time_alarm = cz_time_time(as_time(alarm))

         end function cz_time_alarm

         function CZ_TIMELINE_TIME(gtime, status ) RESULT(rv)
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_time),intent(in) :: gtime
         integer, intent(out), optional :: status

          integer year, month, day, hour, minute, second
          character(len=22) rv

          if ( present(status) ) status = 0
          call z_time_to_date(gtime, year, month, day, hour, minute, second)
          write (FMT='(I2.2,":",I2.2,":",I2.2,"   ",A3," ",I2,",",I4.4)', &
                 UNIT=rv) &
                 hour, minute, second, month_name(month), day, year

         end function cz_timeline_time


         subroutine Z_DAT2DMY(date, day, month, year)
         !*-------------------------------------
         !*-- convert a date to dd/mm/yy format
         integer, intent(in) :: date
         integer, intent(out),optional :: day, month, year

         type (T_time) :: temp_time

          temp_time%value = date * 86400
          if (present(day)) day = z_day(temp_time)
          if (present(month)) month = z_month(temp_time)
          if (present(year)) year = z_year(temp_time)

         end subroutine z_dat2dmy


         integer function Z_DATFDMY(day, month, year)
         integer,intent(in):: day, month, year

         real(kind=8) :: date
         !*-- verify the parms
           date = z_julian_day(year, month, day, 0, 0, 0)
           if ( ORIGIN_J ) then
            z_datfdmy = date
           else
            z_datfdmy = date - JULIAN_OFFSET
           endif

         end function z_datfdmy


         integer function Z_DAYSINM_MMYY (month, year)
         !*---------------------------------------------
         !*-- return the # of days in a month for a year
         integer,intent(in):: month, year

         type (T_Datetime) :: datetime
         integer :: tyear, tmonth

          datetime = as_datetime ( as_time ( year, month, 1, 0, 0, 0) )
          tyear = datetime%year
          tmonth = datetime%month
          if ( ( tyear .eq. 1582) .and. (tmonth .eq. 10) ) then
           z_daysinm_mmyy = 21
          else
           if (leap(tyear)) then
            z_daysinm_mmyy = daysl(tmonth)
           else
            z_daysinm_mmyy = days(tmonth)
           endif
          endif

         end function z_daysinm_mmyy

         integer function Z_DAYSINM_TIME (time)
         !*---------------------------------------------
         !*-- return the # of days in a month for a year
         type (T_time),intent(in) :: time

         integer year, month, day, hour, minute, second

          call z_time_to_date(time, year, month, day, hour, minute, second)
          if ( ( year .eq. 1582) .and. (month .eq. 10) ) then
           z_daysinm_time = 21
          else
           if (leap(year)) then
            z_daysinm_time = daysl(month)
           else
            z_daysinm_time = days(month)
           endif
          endif

         end function z_daysinm_time

         integer function Z_DAYSINM_clock (clock)
         !*---------------------------------------------
         !*-- return the # of days in a month for a year
         type (T_clock),intent(in) :: clock

         z_daysinm_clock = z_daysinm_time(clock%time)
         end function z_daysinm_clock

         integer function Z_DAYSINM_alarm (alarm)
         !*---------------------------------------------
         !*-- return the # of days in a month for a year
         type (T_alarm),intent(in) :: alarm

         z_daysinm_alarm = z_daysinm_time(as_time(alarm))
         end function z_daysinm_alarm

         integer function Z_DAYSINM_DATETIME ( time)
         !*---------------------------------------------
         !*-- return the # of days in a month for a year
         type (T_Datetime),intent(in) :: time

         integer year, month

          year = time%year
          month = time%month
          if ( ( year .eq. 1582) .and. (month .eq. 10) ) then
           z_daysinm_datetime = 21
          else
           if (leap(year)) then
            z_daysinm_datetime = daysl(month)
           else
            z_daysinm_datetime = days(month)
           endif
          endif

         end function z_daysinm_datetime

         real (kind = 8) function Z_JULIAN_SECOND_TIME (time,status)
         !*---------------------------------------
         !*-- return the real value of the time
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

          if ( present(status) ) then
            z_julian_second_time = z_julian_day_time ( time, status ) * 86400.0
          else
            z_julian_second_time = z_julian_day_time ( time ) * 86400.0
          endif

         end function z_julian_second_time

         real (kind = 8) function Z_JULIAN_SECOND_DATETIME &
                                                                      (time,status)
         !*---------------------------------------
         !*-- return the real value of the time
         type (T_Datetime),intent(in):: time
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_second_datetime = z_julian_day_datetime ( time ) * 86400.0

         end function z_julian_second_datetime

         real (kind = 8) function Z_JULIAN_SECOND_CLOCK &
                                                       (clock, status)
         !*---------------------------------------
         !*-- return the real value of the time
         type (T_Clock),intent(in):: clock
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_second_clock = z_julian_day_clock ( clock ) * 86400.0

         end function z_julian_second_clock

         real (kind = 8) function Z_JULIAN_SECOND_alarm &
                                                       (alarm, status)
         !*---------------------------------------
         !*-- return the real value of the time
         type (T_alarm),intent(in):: alarm
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_second_alarm = z_julian_day_alarm ( alarm ) * 86400.0

         end function z_julian_second_alarm

         function Z_JULIAN_SECOND_6INT (year, month, day, &
                               hour, minute, second, status) RESULT(rv)
         integer,intent(in) :: year, month, day, hour, minute, second
         integer,intent(out),optional :: status
         real (kind = 8) :: rv
          if ( present(status) ) status = 0
          rv = z_julian_day (year,month,day,hour,minute,second) &
                                                * 86400.0

         end function z_julian_second_6int

         function Z_JULIAN_SECOND_5INT (year, month, day, &
                               hour, minute, second, status) RESULT(rv)
         integer,intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status
         real (kind = 8) :: rv
          if ( present(status) ) status = 0
          rv = z_julian_day (year,month,day,hour,minute,second) &
                                                * 86400.0

         end function z_julian_second_5int

         function Z_JULIAN_SECOND_6REAL (year, month, day, &
                               hour, minute, second, status) RESULT(rv)
         real, intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status
         real (kind = 8) :: rv
          if ( present(status) ) status = 0
          rv = z_julian_day(year,month,day,hour,minute,second) &
                                                * 86400.0

         end function z_julian_second_6real




         integer function Z_JULIAN_DATE_TIME(time, status)
         !*---------------------------------------
         !*-- return the integer value of the date
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_date_time = time%value / zcSEC_PER_DAY + 1

         end function z_julian_date_time


         integer function Z_MONTH_TIME(time,status)
         !*--------------------------------------------
         !*-- return the integer value of current month
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         integer year, month, day, hour, minute, second

          if ( present(status) ) status = 0
          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_month_time = month

         end function z_month_time

         integer function Z_YEAR_TIME(time, status)
         !*-------------------------------------------
         !*-- return the integer value of current year
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         integer year, month, day, hour, minute, second

          if ( present(status) ) status = 0
          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_year_time = year

         end function z_year_time

         integer function Z_DAY_TIME(time, status)
         !*---------------------------------------------------------------
         !*-- return the integer value of current date of the current year
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         integer year, month, day, hour, minute, second

          if ( present(status) ) status = 0
          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_day_time = day

         end function z_day_time

         integer function Z_HOUR_TIME(time, status)
         !*-------------------------------------------
         !*-- return the integer value of current hour
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         integer year, month, day, hour, minute, second
         Type ( T_time ) :: t
         real (kind=8) :: x

          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_hour_time = hour
          if ( present(status)) status = 0

         end function z_hour_time

         integer function Z_MINUTE_TIME(time, status)
         !*---------------------------------------------
         !*-- return the integer value of current minute
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         real (kind=8) :: x
         integer year, month, day, hour, minute, second

          ! minutes per day = 24*60 = 1440

          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_minute_time = minute
           if ( present(status) ) status = 0

         end function z_minute_time

         integer function Z_SECOND_TIME(time, status)
         !*---------------------------------------------
         !*-- return the integer value of current second
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         real (kind=8) :: x
         integer year, month, day, hour, minute, second

          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_second_time = second
           if ( present(status) ) status = 0

         end function z_second_time

         integer function Z_SEC_OF_DAY_TIME(time, status)
         !*---------------------------------------------
         !*-- return the time in seconds in start of day
         type (T_time),intent(in):: time
         integer,intent(out),optional :: status

         integer year, month, day, hour, minute, second
         Type (T_Time) :: t

          if ( present(status) ) status = 0

         ! if ( origin_j ) then
         ! t%value = time%value + 0.5
         ! else
         ! t = time
         ! endif
         ! z_sec_of_day_time = (t%value - FLOOR(t%value ) )*86400 + 43200
          call z_time_to_date(time, year, month, day, hour, minute, second)
          z_sec_of_day_time = hour * 3600 + minute * 60 + second

         end function z_sec_of_day_time


         function CZ_TIMELINE_DATETIME(time, status) RESULT(rv)
         !*---------------------------------------
         !*-- return the char value of a timestamp
         type (T_Datetime),intent(in) :: time

          character(len=22) rv
          integer, intent(out), optional :: status

          if ( present(status) ) status = 0

          write (FMT='(I2.2,":",I2.2,":",I2.2,"   ",A3," ",I2,",",I4.4)', &
                 UNIT=rv) time%hour, time%minute, INT(time%second), &
                 month_name(time%month), time%day, time%year

         end function cz_timeline_datetime


         real (kind=8) function Z_DAYAD_TIME (time,status )
         !*---------------------------------------------
         !*-- return the number of days since 0z 1/1/1
         type (T_time),intent(in) :: time
         integer, intent(inout), optional :: status

           z_dayAD_time = time%value / zcSEC_PER_DAY
           if ( origin_j ) z_dayAD_time = z_dayAD_time - JULIAN_OFFSET
           if ( present(status) ) status = 0

         end function z_dayAD_time

         real (kind=8) function Z_DAYAD_DATETIME ( time, status )
         !*---------------------------------------------
         !*-- return the number of days since 0z 1/1/1
         type (T_Datetime),intent(in) :: time
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_dayAD_datetime = z_julian_day_datetime( time, status ) - JULIAN_OFFSET
          else
           z_dayAD_datetime= z_julian_day_datetime( time ) - JULIAN_OFFSET
          endif

         end function z_dayAD_datetime

         real (kind=8) function Z_DAYAD_3INT (year,month,day,status)
         !*---------------------------------------------
         !*-- return the number of days since 0z 1/1/1
         integer, intent(in) :: year, month, day
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_dayAD_3int = z_julian_day(year, month, day, 0, 0, 0, status) - JULIAN_OFFSET
          else
           z_dayAD_3int = z_julian_day(year, month, day, 0, 0, 0) - JULIAN_OFFSET
          endif

         end function z_dayAD_3int

         real (kind=8) function Z_DAYAD_6INT ( year, month, day, &
                                                  hour, minute, second, status )
         !*---------------------------------------------
         !*-- return the number of days since 0z 1/1/1
         integer, intent(in) :: year, month, day, hour, minute, second
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_dayAD_6int = z_julian_day(year,month,day,hour,minute,second,status) &
                                   - JULIAN_OFFSET
          else
           z_dayAD_6int = z_julian_day(year,month,day,hour,minute,second) - JULIAN_OFFSET
          endif

         end function z_dayAD_6int

         real (kind=8) function Z_DAYAD_CLOCK ( clock , status )
         !*---------------------------------------------
         !*-- return the number of days since 0z 1/1/1
         type (T_Clock), intent(in) :: clock
         integer, intent(inout), optional :: status

          if ( present(status) ) then
            z_dayAD_clock = z_dayAD_time ( clock%time , status )
          else
            z_dayAD_clock = z_dayAD_time ( clock%time )
          endif

         end function z_dayAD_clock

         real (kind=8) function Z_DAYAD_ALARM ( alarm , status )
         !*---------------------------------------------
         !*-- return the number of days since 0z 1/1/1
         type (T_Alarm), intent(in) :: alarm
         integer, intent(inout), optional :: status
         type (T_Time) :: t

          t = Z_Alarm_origin( Alarm )

          if ( present(status) ) then
            z_dayAD_alarm = z_dayAD_time ( t , status )
          else
            z_dayAD_alarm = z_dayAD_time ( t )
          endif

         end function z_dayAD_alarm



         real (kind=8) function Z_SECONDAD_TIME ( time , status )
         !*---------------------------------------------
         !*-- return the number of seconds since 0z 1/1/1
         type (T_time),intent(in) :: time
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_secondAD_time = ( z_julian_day_time(time, status) - JULIAN_OFFSET) * 86400.0
          else
           z_secondAD_time = ( z_julian_day_time(time) - JULIAN_OFFSET) * 86400.0
          endif

         end function z_secondAD_time

         real (kind=8) function Z_SECONDAD_DATETIME (time, status )
         !*---------------------------------------------
         !*-- return the number of seconds since 0z 1/1/1
         type (T_Datetime),intent(in) :: time
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_secondAD_datetime = (z_julian_day( time, status ) - JULIAN_OFFSET) * 86400.0
          else
           z_secondAD_datetime = ( z_julian_day( time ) - JULIAN_OFFSET) * 86400.0
          endif

         end function z_secondAD_datetime

         real (kind=8) function Z_SECONDAD_3INT &
                                            (year,month,day,status)
         !*---------------------------------------------
         !*-- return the number of seconds since 0z 1/1/1
         integer, intent(in) :: year, month, day
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_secondad_3int = (z_julian_day(year,month,day,0,0,0,status) &
                                - JULIAN_OFFSET) * 86400.0
          else
           z_secondad_3int = (z_julian_day(year,month,day,0,0,0)-JULIAN_OFFSET) * 86400.0
          endif

         end function z_secondad_3int

         real (kind=8) function Z_SECONDAD_6INT (year, month, day, &
                                                   hour, minute, second, status )
         !*---------------------------------------------
         !*-- return the number of seconds since 0z 1/1/1
         integer, intent(in) :: year, month, day, hour, minute, second
         integer, intent(inout), optional :: status

          if ( present(status) ) then
            z_secondad_6int = (z_julian_day(year,month,day,hour,minute,second,status) &
                           - JULIAN_OFFSET) * 86400.0
          else
            z_secondad_6int = (z_julian_day(year,month,day,hour,minute,second) &
                           - JULIAN_OFFSET) * 86400.0
          endif

         end function z_secondad_6int

         real (kind=8) function Z_SECONDAD_CLOCK ( clock , status )
         !*---------------------------------------------
         !*-- return the number of seconds since 0z 1/1/1
         type (T_Clock), intent(in) :: clock
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_secondAD_clock = z_secondAD_time ( clock %time, status )
          else
           z_secondAD_clock = z_secondAD_time ( clock %time )
          endif

         end function z_secondAD_clock

         real (kind=8) function Z_SECONDAD_alarm ( alarm , status )
         !*---------------------------------------------
         !*-- return the number of seconds since 0z 1/1/1
         type (T_alarm), intent(in) :: alarm
         integer, intent(inout), optional :: status

          if ( present(status) ) then
           z_secondAD_alarm = z_secondAD_time ( as_time(alarm), status )
          else
           z_secondAD_alarm = z_secondAD_time ( as_time(alarm) )
          endif

         end function z_secondAD_alarm

         real (kind=8) function Z_JULIAN_DAY_TIME( time, status )
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         type (T_time),intent(in) :: time
         integer,intent(out),optional :: status

         integer year, month, day, hour, minute, second

          if ( PRESENT(status) ) status = 0

          z_julian_day_time = time%value / 86400.
          if ( .not. ORIGIN_J ) z_julian_day_time = z_julian_day_time + JULIAN_OFFSET

         end function z_julian_day_time


         real (kind=8) function Z_JULIAN_DAY_DATETIME(time, status)
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         type (T_Datetime),intent(in) :: time
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0

          z_julian_day_datetime = julian_compute( time%year,time%month, time%day, &
                                                  time%hour,time%minute,time%second )

         end function z_julian_day_datetime

         real (kind=8) function Z_JULIAN_DAY_6INT( year,month,day, &
             hour, minute, second, status )
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         integer,intent(in) :: year, month, day, hour, minute, second
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_day_6int = julian_compute( year, month, day, hour, minute, second )

         end function z_julian_day_6int

         real (kind=8) function Z_JULIAN_DAY_5INT( year,month,day, &
             hour, minute, second, status )
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         integer,intent(in) :: year, month, day, hour, minute
         real, intent(in) :: second
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_day_5int = julian_compute( year, month, day, hour, minute, second )

         end function z_julian_day_5int

         real (kind=8) function Z_JULIAN_DAY_6REAL(year,month,day, &
             hour, minute, second, status )
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         real,intent(in) :: year, month, day, hour, minute
         real,intent(in) :: second
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0
          z_julian_day_6real = julian_compute( year, month, day, hour, minute, second )

         end function z_julian_day_6real

         real (kind=8) function Z_JULIAN_DAY_CLOCK( clock, status)
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         type (T_Clock), intent(in) :: clock
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0

          z_julian_day_clock = z_julian_day_time( clock%time )

         end function z_julian_day_clock


         real (kind=8) function Z_JULIAN_DAY_ALARM( alarm, status)
         !*---------------------------------------------
         !*-- return the number of julian days since origin
         type (T_alarm), intent(in) :: alarm
         integer,intent(out),optional :: status

          if ( present(status) ) status = 0

          z_julian_day_alarm = z_julian_day_time( as_TIME(alarm) )

         end function z_julian_day_alarm


   integer function CAL_MONTH( day_of_year, year )
   !*---------------------------------------------
   !*- return the calendar month for the day of year
    integer , intent(in) :: day_of_year
    integer , intent(in) :: year
    integer :: jalpha,ja,jb,jc,jd,je

    if ( year < 1582) then
      jb = day_of_year + 1722947
      jc = 4716.375 + day_of_year / 365.25
      jd = 365 * jc + INT( 0.25 * jc )
      je = ( jb - jd ) / 30.6001
      Cal_month = je - 1
      if ( Cal_month > 12 ) Cal_month = Cal_month - 12
    endif

   end function Cal_Month


   Type (T_Datetime) function TIME_AS_DATETIME_TIME( time )
   !*------------------------------------
   !*-- return the time in datetime
   type (T_Time),intent(in) :: time

   integer :: year, month, day, hour, minute, second

    call z_time_to_date (time, year, month, day, hour, minute, second)
    time_as_datetime_time%year = year
    time_as_datetime_time%month = month
    time_as_datetime_time%day = day
    time_as_datetime_time%hour = hour
    time_as_datetime_time%minute = minute
    time_as_datetime_time%second = second

   end function time_as_datetime_time

   function TIME_AS_DATETIME_6INT(year,month,day, &
                                  hour,minute,second) RESULT(rv)
   integer, intent(in) :: year, month, day, hour, minute, second
   Type (T_Datetime) :: rv

    rv%year = year
    rv%month = month
    rv%day = day
    rv%hour = hour
    rv%minute = minute
    rv%second = second

   end function time_as_datetime_6int

   function TIME_AS_DATETIME_5INT(year,month,day, &
                                  hour,minute,second) RESULT(rv)
   integer, intent(in) :: year, month, day, hour, minute
   real, intent(in) :: second
   Type (T_Datetime) :: rv
    rv%year = year
    rv%month = month
    rv%day = day
    rv%hour = hour
    rv%minute = minute
    rv%second = second

   end function time_as_datetime_5int


   function TIME_AS_DATETIME_6REAL(year,month,day, &
                                   hour,minute,second) RESULT(rv)
   real, intent(in) :: year, month, day, hour, minute
   real, intent(in) :: second
   Type (T_Datetime) :: rv

    rv%year = year
    rv%month = month
    rv%day = day
    rv%hour = hour
    rv%minute = minute
    rv%second = second

   end function time_as_datetime_6real


   Type (T_time) function JULDAY_AS_TIME ( julday )
   !*------------------------------------
   !*-- return the time in ttime
   real (kind=8), intent(in) :: julday

   julday_as_time%value = julday * 86400

   end function julday_as_time

   Type (T_time) function DATE_AS_TIME ( rdate )
   !*-------------------------------------
   !*- return the time in ttime
   real (kind=8), intent(in) :: rdate

   date_as_time%value = (rdate + 0.5 - 1721424) * 86400

   end function date_as_time


   Type (T_Datetime) function JULDAY_AS_DATETIME ( julday )
   !*------------------------------------
   !*-- return the time in tdatetime
   real (kind=8), intent(in) :: julday
   type (T_time) :: rtime

    rtime%value = julday * 86400
    julday_as_datetime = time_as_datetime ( rtime )

   end function julday_as_datetime


   integer function REAL_MOD (real_val1, real_val2)
   !*---------------------------------------------
   !*-- return the mod for a real number
   real (kind=8),intent(in) :: real_val1
   real , intent(in) :: real_val2

   integer (kind=8) :: int_val1, int_val2, int_val3, int_val4

   int_val1 = NINT(real_val1,kind=8)
   int_val2 = NINT(real_val2)
   int_val3 = int_val1 / int_val2
   int_val4 = int_val1 - (int_val3 * int_val2)
   real_mod = int_val4

   end function real_mod

   End Module zeus_clocks
