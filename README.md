# Fortran-Timez
Fortran Timing library

The code uses the C time.h functions for timing. Extra functionality was added on top so you don't need to mess with the C interface to use this library. When using this library use C interoperability types if you can. Most of the functions take advantage of function overloading so you can use many types with most of the functions. 

Some of these functions are built into Linux so using this module in windows might not work.

Tested only in gfortran 8.2 so far.

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
### gettimeofday gettimeofday_nr
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
### settimeofday
```
timeval,timezone,return val

call settimeofday(timeval,timezone,c_int)
call settimeofday(timeval,timezone)
```
### clock_getres clock_getres_nr
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
### clock_settime
```
clk_id,timespec,return val

call clock_settime(c_int,timespec,c_int)
call clock_settime(c_int,timespec)
call clock_settime(timespec)
```
### clock_gettime clock_gettime_nr
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

