# Fortran-Timez
Fortran Timing library

The code uses the C time.h functions for timing. Extra functionality was added on top so you don't need to mess with the C interface to use this library. When using this library use C interoperability types if you can. Most of the functions take advantage of the function overloading so you can use many types with most of the functions. 

## Functions

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
This is better 
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
#### microsleep
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

