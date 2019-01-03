# Fortran-Timez
Fortran Timing library

The code uses the C time.h functions for timing. Extra functionality was added on top so you don't need to mess with the C interface to use this library. When using this library use C interoperability types if you can. Most of the functions take advantage of function overloading so you can use many types with most of the functions. 

Some of these functions are built into Linux so using this module in windows might not work.

Tested only in gfortran 8.2 so far. Fortran 2003 or higher is required for this library to work.

## Functions/Subroutines

#### nanosleep
```
Real types are floored
timespec is a type
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
#### tv_interval - seconds
```
epoch time, epoch time
returns the time difference between time2-time1 in seconds c_long_double 
time1,time2
tv_interval(time_tm,time_tm)
tv_interval(time_tm,timespec)
tv_interval(time_tm,timeval)
tv_interval(time_tm,c_int)
tv_interval(time_tm,c_long)
tv_interval(time_tm,c_double)
tv_interval(time_tm,c_float)
tv_interval(time_tm,c_double)
tv_interval(time_tm,c_long_double)

tv_interval(time_tm,time_tm)
tv_interval(timespec,time_tm)
tv_interval(timeval,time_tm)
tv_interval(c_int,time_tm)
tv_interval(c_long,time_tm)
tv_interval(c_double,time_tm)
tv_interval(c_float,time_tm)
tv_interval(c_double,time_tm)
tv_interval(c_long_double,time_tm)

tv_interval(timespec,time_tm)
tv_interval(timespec,timespec)
tv_interval(timespec,timeval)
tv_interval(timespec,c_int)
tv_interval(timespec,c_long)
tv_interval(timespec,c_double)
tv_interval(timespec,c_float)
tv_interval(timespec,c_double)
tv_interval(timespec,c_long_double)

tv_interval(time_tm,timespec)
tv_interval(timespec,timespec)
tv_interval(timeval,timespec)
tv_interval(c_int,timespec)
tv_interval(c_long,timespec)
tv_interval(c_double,timespec)
tv_interval(c_float,timespec)
tv_interval(c_double,timespec)
tv_interval(c_long_double,timespec)

tv_interval(timeval,time_tm)
tv_interval(timeval,timespec)
tv_interval(timeval,timeval)
tv_interval(timeval,c_int)
tv_interval(timeval,c_long)
tv_interval(timeval,c_double)
tv_interval(timeval,c_float)
tv_interval(timeval,c_double)
tv_interval(timeval,c_long_double)

tv_interval(time_tm,timeval)
tv_interval(timespec,timeval)
tv_interval(timeval,timeval)
tv_interval(c_int,timeval)
tv_interval(c_long,timeval)
tv_interval(c_double,timeval)
tv_interval(c_float,timeval)
tv_interval(c_double,timeval)
tv_interval(c_long_double,timeval)
```
