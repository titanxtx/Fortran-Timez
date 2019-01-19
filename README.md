# Fortran-Timez
Fortran Timing library

The code uses the C time.h and C++ chrono functions for timing. When using this library use C interoperability types if you can. Most of the functions take advantage of overloading so you can use many types with most of the functions. 

This should work in different operating systems since it relies on C++ chrono when its not in a linux environment.
Clock_settime, clock_getres, and settimeofday do not work outside of linux in this library.

Gfortran 8.2 or higher is required along with a recent C++ compiler c++11 or higher like g++ g++-8. Gfortran 8.1 and lower are not compatible. It doesn't have certain C interoperability features. Haven't tested any other compilers.

#### Usage  `use timez`

For more accurate results use clock_gettime instead of gettimeofday and time_tm with the interval functions

## Functions/Subroutines

#### nanosleep
```
Real types are floored

call nanosleep(timespec,timespec,c_int)
call nanosleep(timespec)
call nanosleep(timespec,c_int)
call nanosleep(c_long,c_long,c_int) !seconds,nanoseconds,return val
call nanosleep(c_long,c_long) !seconds,nanoseconds
call nanosleep(c_long) !nanoseconds
call nanosleep(c_int,c_int,c_int) !seconds,nanoseconds,return val
call nanosleep(c_int,c_int) !seconds,nanoseconds
call nanosleep(c_int) !nanoseconds
call nanosleep(c_float) !nanoseconds
call nanosleep(c_double) !nanoseconds
call nanosleep(c_long_double) !nanoseconds
```
#### usleep
```
Because unsigned variables are not in Fortran it is better to use microsleep.

Real types are floored
call usleep(c_int,c_int)
call usleep(c_int)
call usleep(c_long,c_int)
call usleep(c_long)
call usleep(c_float,c_int)
call usleep(c_float)
call usleep(c_double,c_int)
call usleep(c_double)
call usleep(c_long_double,c_int)
call usleep(c_long_double)
```
#### microsleep
```
microseconds,return val

call microsleep(c_int) 
call microsleep(c_int,c_int)
call microsleep(c_long)
call microsleep(c_long,c_int)
call microsleep(c_double)
call microsleep(c_double,c_int)
call microsleep(c_float)
call microsleep(c_float,c_int)
call microsleep(c_long_double)
call microsleep(c_long_double,c_int)
```
#### millisleep
```
milliseconds,return val

call millisleep(c_int) 
call millisleep(c_int,c_int)
call millisleep(c_long)
call millisleep(c_long,c_int)
call millisleep(c_double)
call millisleep(c_double,c_int)
call millisleep(c_float)
call millisleep(c_float,c_int)
call millisleep(c_long_double)
call millisleep(c_long_double,c_int)
```
#### sleep
```
seconds,return val

call sleep(c_int) 
call sleep(c_int,c_int)
call sleep(c_long) 
call sleep(c_long,c_int)
call sleep(c_double)
call sleep(c_double,c_int)
call sleep(c_float)
call sleep(c_float,c_int)
call sleep(c_long_double)
call sleep(c_long_double,c_int)
```
#### gettimeofday gettimeofday_nr
```
timeval,timezone,return val

subroutine version
call gettimeofday_nr(timeval,timezone,c_int)
call gettimeofday_nr(timeval,timezone)
call gettimeofday_nr(timeval)
call gettimeofday_nr(timeval,c_int)

function version - returns timeval type
gettimeofday(timezone,c_int)
gettimeofday(c_int)
gettimeofday()
```
#### settimeofday
```
timeval,timezone,return val

call settimeofday(timeval,timezone,c_int)
call settimeofday(timeval,timezone)
```
#### clock_getres clock_getres_nr
```
clk_id,timespec,return val
clk_id is a integer constant

subroutine version
call clock_getres_nr(c_int,timespec,c_int)
call clock_getres_nr(c_int,timespec)
call clock_getres_nr(timespec)

function version - returns timespec type
clock_getres(c_int,c_int)
clock_getres(c_int)
clock_getres()
```
#### clock_settime
```
clk_id,timespec,return val

call clock_settime(c_int,timespec,c_int)
call clock_settime(c_int,timespec)
call clock_settime(timespec)
```
#### clock_gettime clock_gettime_nr
```
clk_id,timespec,return val
clk_id is a integer constant

subroutine version
call clock_gettime_nr(c_int,timespec,c_int)
call clock_gettime_nr(c_int,timespec)
call clock_gettime_nr(timespec)

function version - returns timespec type
clock_gettime(c_int,c_int)
clock_gettime(c_int)
clock_gettime()
```
#### gmtime
```
rawtime - epoch

gmtime(timespec)
gmtime(timeval)
gmtime(c_int)
gmtime(c_long)
gmtime(c_float)
gmtime(c_double)
gmtime(c_long_double)
gmtime()   !gets automatically timec() aka time() in c
```
#### localtime
```
rawtime - epoch
returns time_tm type

localtime(timespec)
localtime(timeval)
localtime(c_int)
localtime(c_long)
localtime(c_float)
localtime(c_double)
localtime(c_long_double)
localtime()   !automatically gets timec() aka time() in c
```
#### strtime
```
rawtime -epoch
ctime c is strtime
returns a string

strtime(timespec)
strtime(timeval)
strtime(c_int)
strtime(c_long)
strtime(c_float)
strtime(c_double)
strtime(c_long_double)
strtime()  !automatically gets timec() aka time() in c
```
#### asctime
```
time_tm or epoch
returns a string

Main type to be used for is time_tm
asctime(time_tm)
asctime(timespec)
asctime(timeval)
asctime(c_int)
asctime(c_long)
asctime(c_float)
asctime(c_double)
asctime(c_long_double)
asctime()  !automatically gets timec() aka time() in c
```
#### time
```
rawtime -epoch
time() in c
returns the time in epoch

timec(c_long) !saves to the variable in the argument
timec() 
```
#### mktime
```
time_tm
returns the time in epoch

mktime(time_tm) !saves to the variable in the argument
mktime()  !Automatically gets time_tm with localtime()
```
#### strftime `strftime("%A %B %d %r %Y")`
```
format, time_tm or epoch, (string buffersize optional)
returns a string with the format entered in

strftime(string,time_tm)
strftime(string,time_tm,c_long)
strftime(string,timespec)
strftime(string,timespec,c_long)
strftime(string,timeval)
strftime(string,timeval,c_long)
strftime(string,c_int)
strftime(string,c_int,c_long)
strftime(string,c_long)
strftime(string,c_long,c_long)
strftime(string,c_float)
strftime(string,c_float,c_long)
strftime(string,c_double)
strftime(string,c_double,c_long)
strftime(string,c_long_double)
strftime(string,c_long_double,c_long)
strftime(string)  !automatically gets localtime()
```
#### interval
```
seconds
epoch time, epoch time
returns the time difference between time2-time1 in seconds c_long_double 
time1,time2
interval(time_tm,time_tm)
interval(time_tm,timespec)
interval(time_tm,timeval)
interval(time_tm,c_int)
interval(time_tm,c_long)
interval(time_tm,c_double)
interval(time_tm,c_float)
interval(time_tm,c_double)
interval(time_tm,c_long_double)

interval(time_tm,time_tm)
interval(timespec,time_tm)
interval(timeval,time_tm)
interval(c_int,time_tm)
interval(c_long,time_tm)
interval(c_double,time_tm)
interval(c_float,time_tm)
interval(c_double,time_tm)
interval(c_long_double,time_tm)

interval(timespec,time_tm)
interval(timespec,timespec)
interval(timespec,timeval)
interval(timespec,c_int)
interval(timespec,c_long)
interval(timespec,c_double)
interval(timespec,c_float)
interval(timespec,c_double)
interval(timespec,c_long_double)

interval(time_tm,timespec)
interval(timespec,timespec)
interval(timeval,timespec)
interval(c_int,timespec)
interval(c_long,timespec)
interval(c_double,timespec)
interval(c_float,timespec)
interval(c_double,timespec)
interval(c_long_double,timespec)

interval(timeval,time_tm)
interval(timeval,timespec)
interval(timeval,timeval)
interval(timeval,c_int)
interval(timeval,c_long)
interval(timeval,c_double)
interval(timeval,c_float)
interval(timeval,c_double)
interval(timeval,c_long_double)

interval(time_tm,timeval)
interval(timespec,timeval)
interval(timeval,timeval)
interval(c_int,timeval)
interval(c_long,timeval)
interval(c_double,timeval)
interval(c_float,timeval)
interval(c_double,timeval)
interval(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval(time_tm) 
interval(timespec)
interval(timeval)
interval(c_int)
interval(c_long)
interval(c_double)
interval(c_float)
interval(c_double)
interval(c_long_double)
```
#### interval_milli 
```
milliseconds
epoch time, epoch time
returns the time difference between time2-time1 in milliseconds c_long_double 
time1,time2
interval_milli(time_tm,time_tm)
interval_milli(time_tm,timespec)
interval_milli(time_tm,timeval)
interval_milli(time_tm,c_int)
interval_milli(time_tm,c_long)
interval_milli(time_tm,c_double)
interval_milli(time_tm,c_float)
interval_milli(time_tm,c_double)
interval_milli(time_tm,c_long_double)

interval_milli(time_tm,time_tm)
interval_milli(timespec,time_tm)
interval_milli(timeval,time_tm)
interval_milli(c_int,time_tm)
interval_milli(c_long,time_tm)
interval_milli(c_double,time_tm)
interval_milli(c_float,time_tm)
interval_milli(c_double,time_tm)
interval_milli(c_long_double,time_tm)

interval_milli(timespec,time_tm)
interval_milli(timespec,timespec)
interval_milli(timespec,timeval)
interval_milli(timespec,c_int)
interval_milli(timespec,c_long)
interval_milli(timespec,c_double)
interval_milli(timespec,c_float)
interval_milli(timespec,c_double)
interval_milli(timespec,c_long_double)

interval_milli(time_tm,timespec)
interval_milli(timespec,timespec)
interval_milli(timeval,timespec)
interval_milli(c_int,timespec)
interval_milli(c_long,timespec)
interval_milli(c_double,timespec)
interval_milli(c_float,timespec)
interval_milli(c_double,timespec)
interval_milli(c_long_double,timespec)

interval_milli(timeval,time_tm)
interval_milli(timeval,timespec)
interval_milli(timeval,timeval)
interval_milli(timeval,c_int)
interval_milli(timeval,c_long)
interval_milli(timeval,c_double)
interval_milli(timeval,c_float)
interval_milli(timeval,c_double)
interval_milli(timeval,c_long_double)

interval_milli(time_tm,timeval)
interval_milli(timespec,timeval)
interval_milli(timeval,timeval)
interval_milli(c_int,timeval)
interval_milli(c_long,timeval)
interval_milli(c_double,timeval)
interval_milli(c_float,timeval)
interval_milli(c_double,timeval)
interval_milli(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_milli(time_tm) 
interval_milli(timespec)
interval_milli(timeval)
interval_milli(c_int)
interval_milli(c_long)
interval_milli(c_double)
interval_milli(c_float)
interval_milli(c_double)
interval_milli(c_long_double)
```
#### interval_micro
```
microseconds
epoch time, epoch time
returns the time difference between time2-time1 in microseconds c_long_double 
time1,time2
interval_micro(time_tm,time_tm)
interval_micro(time_tm,timespec)
interval_micro(time_tm,timeval)
interval_micro(time_tm,c_int)
interval_micro(time_tm,c_long)
interval_micro(time_tm,c_double)
interval_micro(time_tm,c_float)
interval_micro(time_tm,c_double)
interval_micro(time_tm,c_long_double)

interval_micro(time_tm,time_tm)
interval_micro(timespec,time_tm)
interval_micro(timeval,time_tm)
interval_micro(c_int,time_tm)
interval_micro(c_long,time_tm)
interval_micro(c_double,time_tm)
interval_micro(c_float,time_tm)
interval_micro(c_double,time_tm)
interval_micro(c_long_double,time_tm)

interval_micro(timespec,time_tm)
interval_micro(timespec,timespec)
interval_micro(timespec,timeval)
interval_micro(timespec,c_int)
interval_micro(timespec,c_long)
interval_micro(timespec,c_double)
interval_micro(timespec,c_float)
interval_micro(timespec,c_double)
interval_micro(timespec,c_long_double)

interval_micro(time_tm,timespec)
interval_micro(timespec,timespec)
interval_micro(timeval,timespec)
interval_micro(c_int,timespec)
interval_micro(c_long,timespec)
interval_micro(c_double,timespec)
interval_micro(c_float,timespec)
interval_micro(c_double,timespec)
interval_micro(c_long_double,timespec)

interval_micro(timeval,time_tm)
interval_micro(timeval,timespec)
interval_micro(timeval,timeval)
interval_micro(timeval,c_int)
interval_micro(timeval,c_long)
interval_micro(timeval,c_double)
interval_micro(timeval,c_float)
interval_micro(timeval,c_double)
interval_micro(timeval,c_long_double)

interval_micro(time_tm,timeval)
interval_micro(timespec,timeval)
interval_micro(timeval,timeval)
interval_micro(c_int,timeval)
interval_micro(c_long,timeval)
interval_micro(c_double,timeval)
interval_micro(c_float,timeval)
interval_micro(c_double,timeval)
interval_micro(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_micro(time_tm) 
interval_micro(timespec)
interval_micro(timeval)
interval_micro(c_int)
interval_micro(c_long)
interval_micro(c_double)
interval_micro(c_float)
interval_micro(c_double)
interval_micro(c_long_double)
```
#### interval_nano
```
nanoseconds
epoch time, epoch time
returns the time difference between time2-time1 in nanoseconds c_long_double 
time1,time2
interval_nano(time_tm,time_tm)
interval_nano(time_tm,timespec)
interval_nano(time_tm,timeval)
interval_nano(time_tm,c_int)
interval_nano(time_tm,c_long)
interval_nano(time_tm,c_double)
interval_nano(time_tm,c_float)
interval_nano(time_tm,c_double)
interval_nano(time_tm,c_long_double)

interval_nano(time_tm,time_tm)
interval_nano(timespec,time_tm)
interval_nano(timeval,time_tm)
interval_nano(c_int,time_tm)
interval_nano(c_long,time_tm)
interval_nano(c_double,time_tm)
interval_nano(c_float,time_tm)
interval_nano(c_double,time_tm)
interval_nano(c_long_double,time_tm)

interval_nano(timespec,time_tm)
interval_nano(timespec,timespec)
interval_nano(timespec,timeval)
interval_nano(timespec,c_int)
interval_nano(timespec,c_long)
interval_nano(timespec,c_double)
interval_nano(timespec,c_float)
interval_nano(timespec,c_double)
interval_nano(timespec,c_long_double)

interval_nano(time_tm,timespec)
interval_nano(timespec,timespec)
interval_nano(timeval,timespec)
interval_nano(c_int,timespec)
interval_nano(c_long,timespec)
interval_nano(c_double,timespec)
interval_nano(c_float,timespec)
interval_nano(c_double,timespec)
interval_nano(c_long_double,timespec)

interval_nano(timeval,time_tm)
interval_nano(timeval,timespec)
interval_nano(timeval,timeval)
interval_nano(timeval,c_int)
interval_nano(timeval,c_long)
interval_nano(timeval,c_double)
interval_nano(timeval,c_float)
interval_nano(timeval,c_double)
interval_nano(timeval,c_long_double)

interval_nano(time_tm,timeval)
interval_nano(timespec,timeval)
interval_nano(timeval,timeval)
interval_nano(c_int,timeval)
interval_nano(c_long,timeval)
interval_nano(c_double,timeval)
interval_nano(c_float,timeval)
interval_nano(c_double,timeval)
interval_nano(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_nano(time_tm) 
interval_nano(timespec)
interval_nano(timeval)
interval_nano(c_int)
interval_nano(c_long)
interval_nano(c_double)
interval_nano(c_float)
interval_nano(c_double)
interval_nano(c_long_double)
```
#### interval_min
```
minutes
epoch time, epoch time
returns the time difference between time2-time1 in minutes c_long_double 
time1,time2
interval_min(time_tm,time_tm)
interval_min(time_tm,timespec)
interval_min(time_tm,timeval)
interval_min(time_tm,c_int)
interval_min(time_tm,c_long)
interval_min(time_tm,c_double)
interval_min(time_tm,c_float)
interval_min(time_tm,c_double)
interval_min(time_tm,c_long_double)

interval_min(time_tm,time_tm)
interval_min(timespec,time_tm)
interval_min(timeval,time_tm)
interval_min(c_int,time_tm)
interval_min(c_long,time_tm)
interval_min(c_double,time_tm)
interval_min(c_float,time_tm)
interval_min(c_double,time_tm)
interval_min(c_long_double,time_tm)

interval_min(timespec,time_tm)
interval_min(timespec,timespec)
interval_min(timespec,timeval)
interval_min(timespec,c_int)
interval_min(timespec,c_long)
interval_min(timespec,c_double)
interval_min(timespec,c_float)
interval_min(timespec,c_double)
interval_min(timespec,c_long_double)

interval_min(time_tm,timespec)
interval_min(timespec,timespec)
interval_min(timeval,timespec)
interval_min(c_int,timespec)
interval_min(c_long,timespec)
interval_min(c_double,timespec)
interval_min(c_float,timespec)
interval_min(c_double,timespec)
interval_min(c_long_double,timespec)

interval_min(timeval,time_tm)
interval_min(timeval,timespec)
interval_min(timeval,timeval)
interval_min(timeval,c_int)
interval_min(timeval,c_long)
interval_min(timeval,c_double)
interval_min(timeval,c_float)
interval_min(timeval,c_double)
interval_min(timeval,c_long_double)

interval_min(time_tm,timeval)
interval_min(timespec,timeval)
interval_min(timeval,timeval)
interval_min(c_int,timeval)
interval_min(c_long,timeval)
interval_min(c_double,timeval)
interval_min(c_float,timeval)
interval_min(c_double,timeval)
interval_min(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_min(time_tm) 
interval_min(timespec)
interval_min(timeval)
interval_min(c_int)
interval_min(c_long)
interval_min(c_double)
interval_min(c_float)
interval_min(c_double)
interval_min(c_long_double)
```
#### interval_hour
```
hours
epoch time, epoch time
returns the time difference between time2-time1 in hours c_long_double 
time1,time2
interval_hour(time_tm,time_tm)
interval_hour(time_tm,timespec)
interval_hour(time_tm,timeval)
interval_hour(time_tm,c_int)
interval_hour(time_tm,c_long)
interval_hour(time_tm,c_double)
interval_hour(time_tm,c_float)
interval_hour(time_tm,c_double)
interval_hour(time_tm,c_long_double)

interval_hour(time_tm,time_tm)
interval_hour(timespec,time_tm)
interval_hour(timeval,time_tm)
interval_hour(c_int,time_tm)
interval_hour(c_long,time_tm)
interval_hour(c_double,time_tm)
interval_hour(c_float,time_tm)
interval_hour(c_double,time_tm)
interval_hour(c_long_double,time_tm)

interval_hour(timespec,time_tm)
interval_hour(timespec,timespec)
interval_hour(timespec,timeval)
interval_hour(timespec,c_int)
interval_hour(timespec,c_long)
interval_hour(timespec,c_double)
interval_hour(timespec,c_float)
interval_hour(timespec,c_double)
interval_hour(timespec,c_long_double)

interval_hour(time_tm,timespec)
interval_hour(timespec,timespec)
interval_hour(timeval,timespec)
interval_hour(c_int,timespec)
interval_hour(c_long,timespec)
interval_hour(c_double,timespec)
interval_hour(c_float,timespec)
interval_hour(c_double,timespec)
interval_hour(c_long_double,timespec)

interval_hour(timeval,time_tm)
interval_hour(timeval,timespec)
interval_hour(timeval,timeval)
interval_hour(timeval,c_int)
interval_hour(timeval,c_long)
interval_hour(timeval,c_double)
interval_hour(timeval,c_float)
interval_hour(timeval,c_double)
interval_hour(timeval,c_long_double)

interval_hour(time_tm,timeval)
interval_hour(timespec,timeval)
interval_hour(timeval,timeval)
interval_hour(c_int,timeval)
interval_hour(c_long,timeval)
interval_hour(c_double,timeval)
interval_hour(c_float,timeval)
interval_hour(c_double,timeval)
interval_hour(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_hour(time_tm) 
interval_hour(timespec)
interval_hour(timeval)
interval_hour(c_int)
interval_hour(c_long)
interval_hour(c_double)
interval_hour(c_float)
interval_hour(c_double)
interval_hour(c_long_double)
```
#### interval_day
```
days
epoch time, epoch time
returns the time difference between time2-time1 in days c_long_double 
time1,time2
interval_day(time_tm,time_tm)
interval_day(time_tm,timespec)
interval_day(time_tm,timeval)
interval_day(time_tm,c_int)
interval_day(time_tm,c_long)
interval_day(time_tm,c_double)
interval_day(time_tm,c_float)
interval_day(time_tm,c_double)
interval_day(time_tm,c_long_double)

interval_day(time_tm,time_tm)
interval_day(timespec,time_tm)
interval_day(timeval,time_tm)
interval_day(c_int,time_tm)
interval_day(c_long,time_tm)
interval_day(c_double,time_tm)
interval_day(c_float,time_tm)
interval_day(c_double,time_tm)
interval_day(c_long_double,time_tm)

interval_day(timespec,time_tm)
interval_day(timespec,timespec)
interval_day(timespec,timeval)
interval_day(timespec,c_int)
interval_day(timespec,c_long)
interval_day(timespec,c_double)
interval_day(timespec,c_float)
interval_day(timespec,c_double)
interval_day(timespec,c_long_double)

interval_day(time_tm,timespec)
interval_day(timespec,timespec)
interval_day(timeval,timespec)
interval_day(c_int,timespec)
interval_day(c_long,timespec)
interval_day(c_double,timespec)
interval_day(c_float,timespec)
interval_day(c_double,timespec)
interval_day(c_long_double,timespec)

interval_day(timeval,time_tm)
interval_day(timeval,timespec)
interval_day(timeval,timeval)
interval_day(timeval,c_int)
interval_day(timeval,c_long)
interval_day(timeval,c_double)
interval_day(timeval,c_float)
interval_day(timeval,c_double)
interval_day(timeval,c_long_double)

interval_day(time_tm,timeval)
interval_day(timespec,timeval)
interval_day(timeval,timeval)
interval_day(c_int,timeval)
interval_day(c_long,timeval)
interval_day(c_double,timeval)
interval_day(c_float,timeval)
interval_day(c_double,timeval)
interval_day(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_day(time_tm) 
interval_day(timespec)
interval_day(timeval)
interval_day(c_int)
interval_day(c_long)
interval_day(c_double)
interval_day(c_float)
interval_day(c_double)
interval_day(c_long_double)
```
#### interval_week
```
weeks
epoch time, epoch time
returns the time difference between time2-time1 in weeks c_long_double 
time1,time2
interval_week(time_tm,time_tm)
interval_week(time_tm,timespec)
interval_week(time_tm,timeval)
interval_week(time_tm,c_int)
interval_week(time_tm,c_long)
interval_week(time_tm,c_double)
interval_week(time_tm,c_float)
interval_week(time_tm,c_double)
interval_week(time_tm,c_long_double)

interval_week(time_tm,time_tm)
interval_week(timespec,time_tm)
interval_week(timeval,time_tm)
interval_week(c_int,time_tm)
interval_week(c_long,time_tm)
interval_week(c_double,time_tm)
interval_week(c_float,time_tm)
interval_week(c_double,time_tm)
interval_week(c_long_double,time_tm)

interval_week(timespec,time_tm)
interval_week(timespec,timespec)
interval_week(timespec,timeval)
interval_week(timespec,c_int)
interval_week(timespec,c_long)
interval_week(timespec,c_double)
interval_week(timespec,c_float)
interval_week(timespec,c_double)
interval_week(timespec,c_long_double)

interval_week(time_tm,timespec)
interval_week(timespec,timespec)
interval_week(timeval,timespec)
interval_week(c_int,timespec)
interval_week(c_long,timespec)
interval_week(c_double,timespec)
interval_week(c_float,timespec)
interval_week(c_double,timespec)
interval_week(c_long_double,timespec)

interval_week(timeval,time_tm)
interval_week(timeval,timespec)
interval_week(timeval,timeval)
interval_week(timeval,c_int)
interval_week(timeval,c_long)
interval_week(timeval,c_double)
interval_week(timeval,c_float)
interval_week(timeval,c_double)
interval_week(timeval,c_long_double)

interval_week(time_tm,timeval)
interval_week(timespec,timeval)
interval_week(timeval,timeval)
interval_week(c_int,timeval)
interval_week(c_long,timeval)
interval_week(c_double,timeval)
interval_week(c_float,timeval)
interval_week(c_double,timeval)
interval_week(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_week(time_tm) 
interval_week(timespec)
interval_week(timeval)
interval_week(c_int)
interval_week(c_long)
interval_week(c_double)
interval_week(c_float)
interval_week(c_double)
interval_week(c_long_double)
```
#### interval_year
```
years
epoch time, epoch time
returns the time difference between time2-time1 in years c_long_double 
time1,time2
interval_year(time_tm,time_tm)
interval_year(time_tm,timespec)
interval_year(time_tm,timeval)
interval_year(time_tm,c_int)
interval_year(time_tm,c_long)
interval_year(time_tm,c_double)
interval_year(time_tm,c_float)
interval_year(time_tm,c_double)
interval_year(time_tm,c_long_double)

interval_year(time_tm,time_tm)
interval_year(timespec,time_tm)
interval_year(timeval,time_tm)
interval_year(c_int,time_tm)
interval_year(c_long,time_tm)
interval_year(c_double,time_tm)
interval_year(c_float,time_tm)
interval_year(c_double,time_tm)
interval_year(c_long_double,time_tm)

interval_year(timespec,time_tm)
interval_year(timespec,timespec)
interval_year(timespec,timeval)
interval_year(timespec,c_int)
interval_year(timespec,c_long)
interval_year(timespec,c_double)
interval_year(timespec,c_float)
interval_year(timespec,c_double)
interval_year(timespec,c_long_double)

interval_year(time_tm,timespec)
interval_year(timespec,timespec)
interval_year(timeval,timespec)
interval_year(c_int,timespec)
interval_year(c_long,timespec)
interval_year(c_double,timespec)
interval_year(c_float,timespec)
interval_year(c_double,timespec)
interval_year(c_long_double,timespec)

interval_year(timeval,time_tm)
interval_year(timeval,timespec)
interval_year(timeval,timeval)
interval_year(timeval,c_int)
interval_year(timeval,c_long)
interval_year(timeval,c_double)
interval_year(timeval,c_float)
interval_year(timeval,c_double)
interval_year(timeval,c_long_double)

interval_year(time_tm,timeval)
interval_year(timespec,timeval)
interval_year(timeval,timeval)
interval_year(c_int,timeval)
interval_year(c_long,timeval)
interval_year(c_double,timeval)
interval_year(c_float,timeval)
interval_year(c_double,timeval)
interval_year(c_long_double,timeval)

!Automatically compares the difference between clock_gettime and the user entered data
interval_year(time_tm) 
interval_year(timespec)
interval_year(timeval)
interval_year(c_int)
interval_year(c_long)
interval_year(c_double)
interval_year(c_float)
interval_year(c_double)
interval_year(c_long_double)
```
#### difftime
```
end time, beginning time
returns c_double of the time difference

difftime(c_long,c_long)
```
## types

#### time_tm:
```
integer(kind=c_int)::tm_sec
integer(kind=c_int)::tm_min
integer(kind=c_int)::tm_hour
integer(kind=c_int)::tm_mday
integer(kind=c_int)::tm_mon
integer(kind=c_int)::tm_year
integer(kind=c_int)::tm_wday
integer(kind=c_int)::tm_yday
integer(kind=c_int)::tm_isdst

functions local, gm
local sets time_tm to localtime()
gm sets time_tm to gmtime()

call this%local(c_int)
call this%local(c_long)
call this%local()

call this%gm(c_int)
call this%gm(c_long)
call this%gm()

Has operator overloading for =,+,-,*,/,**
Using any +,-,*,/,** for manipulating the seconds
New operators: 
.addsec. - adds seconds
.subsec. - substracts seconds
.addday. - adds days
.subday. - subtracts days
.addmin. - adds minutes
.submin. - subtracts minutes
.addhour. - adds hours
.subhour.  - subtracts hours
.addweek. - adds weeks
.subweek. - subtracts weeks
.addyear. - adds years
.subyear. - subtracts years

If assignment is to a number time_tm will convert to a epoch integer

print *,"" will automatically convert time_tm to a string to print
```     
#### timespec:
```
integer(kind=c_long)::tv_sec=0
integer(kind=c_long)::tv_nsec=0

function set
call this%set(c_long,c_long)  !tv_sec,tv_nsec
call this%set(c_long)  !tv_sec

Has operator overloading for =,+,-,*,/,**
Using any +,-,*,/,** for manipulating the seconds

If assignment is to a number timespec will convert to a epoch integer

print *,"" will automatically convert time_tm to a string to print
```
        
#### timeval:
```
integer(kind=c_long)::tv_sec=0
integer(kind=c_long)::tv_usec=0

function set
call this%set(c_long,c_long)  !tv_sec,tv_usec
call this%set(c_long)  !tv_sec

Has operator overloading for =,+,-,*,/,**
Using any +,-,*,/,** for manipulating the seconds

If assignment is to a number timeval will convert to a epoch integer

print *,"" will automatically convert time_tm to a string to print
```
#### timezone:
```
integer(kind=c_int)::tz_minuteswest=0
integer(kind=c_int)::tz_dsttime=0
```
## Constants
```
clk_id-
integer(kind=c_int),parameter::clock_realtime=0,clock_monotonic=1,clock_process_cputime_id=2,&
    clock_thread_cputime_id=3,clock_monotonic_raw=4,clock_realtime_coarse=5,clock_monotonic_coarse=6,&
    clock_boottime=7,clock_realtime_alarm=8,clock_boottime_alarm=9,clock_tai=11,timer_abstime=1

```
## Compiling
```
g++-8 -c chronos.cpp && gfortran-8 -c YOUR_PROGRAM.f08 timez.f08 && gfortran-8 YOUR_PROGRAM.o timez.o chronos.o -lstdc++ -o t.o &&./t.o
```
