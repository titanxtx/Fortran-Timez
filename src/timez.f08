!MIT License

!Copyright (c) 2019 Joshua Oliva

!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:

!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.

!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
module timez
    use iso_c_binding,only:c_int,c_long,c_int32_t,c_short,c_ptr,c_null_ptr,&
    c_loc,c_double,c_long_double,c_f_pointer,c_size_t,c_float
    implicit none
    type time_tm
        integer(kind=c_int)::tm_sec
        integer(kind=c_int)::tm_min
        integer(kind=c_int)::tm_hour
        integer(kind=c_int)::tm_mday
        integer(kind=c_int)::tm_mon
        integer(kind=c_int)::tm_year
        integer(kind=c_int)::tm_wday
        integer(kind=c_int)::tm_yday
        integer(kind=c_int)::tm_isdst
        contains
        procedure,private::time_tm_write_f,time_tm_write_uf
        procedure,private::tm_equals_tm,tm_equals_int,tm_equals_long,tm_equals_double,&
        tm_equals_float,tm_equals_longd
        procedure,private::time_tm_local,time_tm_local_long,time_tm_local_int,time_tm_gm,&
        time_tm_gm_int,time_tm_gm_long

        procedure,private::tm_add_int,tm_sub_int,tm_multi_int,tm_div_int,tm_expo_int,&
        tm_add_double,tm_sub_double,tm_multi_double,tm_div_double,tm_expo_double,&
        tm_add_float,tm_sub_float,tm_multi_float,tm_div_float,tm_expo_float,&
        tm_add_long,tm_sub_long,tm_multi_long,tm_div_long,tm_expo_long,&
        tm_add_longd,tm_sub_longd,tm_multi_longd,tm_div_longd,tm_expo_longd
        generic::assignment(=)=>tm_equals_tm,tm_equals_int,tm_equals_long,tm_equals_double,&
        tm_equals_float,tm_equals_longd
        procedure,pass(this)::str=>tm_to_str
        generic::local=>time_tm_local,time_tm_local_long,time_tm_local_int
        generic::gm=>time_tm_gm,time_tm_gm_int,time_tm_gm_long
        generic::operator(+)=>tm_add_int,tm_add_double,tm_add_float,tm_add_long,tm_add_longd
        generic::operator(-)=>tm_sub_int,tm_sub_double,tm_sub_float,tm_sub_long,tm_sub_longd
        generic::operator(*)=>tm_multi_int,tm_multi_double,tm_multi_float,tm_multi_long,tm_multi_longd
        generic::operator(/)=>tm_div_int,tm_div_double,tm_div_float,tm_div_long,tm_div_longd
        generic::operator(**)=>tm_expo_int,tm_expo_double,tm_expo_float,tm_expo_long,tm_expo_longd
        generic::write(formatted)=>time_tm_write_f
        generic::write(unformatted)=>time_tm_write_uf
        !-----------------------------------------------
        procedure,private::time_tm_addday_int,time_tm_addday_long,time_tm_addday_float,&
        time_tm_addday_double,time_tm_addday_longd,&
        time_tm_addhour_int,time_tm_addhour_long,time_tm_addhour_float,&
        time_tm_addhour_double,time_tm_addhour_longd,&
        time_tm_addmin_int,time_tm_addmin_long,time_tm_addmin_float,&
        time_tm_addmin_double,time_tm_addmin_longd,&
        time_tm_addyear_int,time_tm_addyear_long,time_tm_addyear_float,&
        time_tm_addyear_double,time_tm_addyear_longd,&
        time_tm_addweek_int,time_tm_addweek_long,time_tm_addweek_float,&
        time_tm_addweek_double,time_tm_addweek_longd,&
        time_tm_addsec_int,time_tm_addsec_long,time_tm_addsec_float,&
        time_tm_addsec_double,time_tm_addsec_longd

        procedure,private::time_tm_subday_int,time_tm_subday_long,time_tm_subday_float,&
        time_tm_subday_double,time_tm_subday_longd,&
        time_tm_subhour_int,time_tm_subhour_long,time_tm_subhour_float,&
        time_tm_subhour_double,time_tm_subhour_longd,&
        time_tm_submin_int,time_tm_submin_long,time_tm_submin_float,&
        time_tm_submin_double,time_tm_submin_longd,&
        time_tm_subyear_int,time_tm_subyear_long,time_tm_subyear_float,&
        time_tm_subyear_double,time_tm_subyear_longd,&
        time_tm_subweek_int,time_tm_subweek_long,time_tm_subweek_float,&
        time_tm_subweek_double,time_tm_subweek_longd,&
        time_tm_subsec_int,time_tm_subsec_long,time_tm_subsec_float,&
        time_tm_subsec_double,time_tm_subsec_longd

        generic::operator(.addsec.)=>time_tm_addsec_int,time_tm_addsec_long,time_tm_addsec_float,&
        time_tm_addsec_double,time_tm_addsec_longd
        generic::operator(.subsec.)=>time_tm_subsec_int,time_tm_subsec_long,time_tm_subsec_float,&
        time_tm_subsec_double,time_tm_subsec_longd

        generic::operator(.addday.)=>time_tm_addday_int,time_tm_addday_long,time_tm_addday_float,&
        time_tm_addday_double,time_tm_addday_longd
        generic::operator(.subday.)=>time_tm_subday_double,time_tm_subday_longd,&
        time_tm_subhour_int,time_tm_subhour_long,time_tm_subhour_float

        generic::operator(.addhour.)=>time_tm_addhour_int,time_tm_addhour_long,time_tm_addhour_float,&
        time_tm_addhour_double,time_tm_addhour_longd
        generic::operator(.subhour.)=>time_tm_subhour_int,time_tm_subhour_long,time_tm_subhour_float,&
        time_tm_subhour_double,time_tm_subhour_longd

        generic::operator(.addmin.)=>time_tm_addmin_int,time_tm_addmin_long,time_tm_addmin_float,&
        time_tm_addmin_double,time_tm_addmin_longd
        generic::operator(.submin.)=>time_tm_submin_int,time_tm_submin_long,time_tm_submin_float,&
        time_tm_submin_double,time_tm_submin_longd

        generic::operator(.addyear.)=>time_tm_addyear_int,time_tm_addyear_long,time_tm_addyear_float,&
        time_tm_addyear_double,time_tm_addyear_longd
        generic::operator(.subyear.)=>time_tm_subyear_int,time_tm_subyear_long,time_tm_subyear_float,&
        time_tm_subyear_double,time_tm_subyear_longd

        generic::operator(.addweek.)=>time_tm_addweek_int,time_tm_addweek_long,time_tm_addweek_float,&
        time_tm_addweek_double,time_tm_addweek_longd
        generic::operator(.subweek.)=>time_tm_subweek_int,time_tm_subweek_long,time_tm_subweek_float,&
        time_tm_subweek_double,time_tm_subweek_longd
    end type time_tm

    type timespec
        integer(kind=c_long)::tv_sec=0
        integer(kind=c_long)::tv_nsec=0
        contains
        procedure,private::timespec_equals_cint,timespec_equals_cd,timespec_equals_clongd,timespec_equals_clong,&
        timespec_equals_cf
        procedure,private::set_timespec,set_timespec2,timespec_equals_timeval,timespec_add_timespec,&
        timespec_sub_timespec,timespec_multi_timespec,timespec_div_timespec,timespec_expo_timespec,&
        timespec_add_clongd,timespec_sub_clongd,timespec_multi_clongd,&
        timespec_div_clongd,timespec_expo_clongd,&
        timespec_add_clong,timespec_sub_clong,timespec_multi_clong,&
        timespec_div_clong,timespec_expo_clong,&
        timespec_add_cint,timespec_sub_cint,timespec_multi_cint,&
        timespec_div_cint,timespec_expo_cint,&
        timespec_add_cd,timespec_sub_cd,timespec_multi_cd,&
        timespec_div_cd,timespec_expo_cd,&
        timespec_add_cf,timespec_sub_cf,timespec_multi_cf,timespec_div_cf,timespec_expo_cf,&

        timespec_add_timeval,timespec_sub_timeval,timespec_multi_timeval,timespec_div_timeval,&
        timespec_expo_timeval

        procedure,private::timespec_write_f,timespec_write_uf
        procedure,private::timespec_second_int,timespec_second_long,timespec_second_longd,timespec_second_double,&
        timespec_second_float,&
        timespec_nanosecond_int,timespec_nanosecond_long,timespec_nanosecond_longd,timespec_nanosecond_double,&
        timespec_nanosecond_float
        generic::sec=>timespec_second_int,timespec_second_long,timespec_second_longd,timespec_second_double,&
        timespec_second_float
        generic::nano=>timespec_nanosecond_int,timespec_nanosecond_long,timespec_nanosecond_longd,timespec_nanosecond_double,&
        timespec_nanosecond_float
        generic::set=>set_timespec,set_timespec2
        generic::assignment(=)=>timespec_equals_timeval,timespec_equals_cint,timespec_equals_cd,timespec_equals_clongd,&
        timespec_equals_clong,timespec_equals_cf
        generic::operator(+)=>timespec_add_timespec,timespec_add_clongd,timespec_add_clong,&
        timespec_add_cint,timespec_add_cd,timespec_add_timeval,timespec_add_cf
        generic::operator(-)=>timespec_sub_timespec,timespec_sub_clongd,timespec_sub_clong,&
        timespec_sub_cint,timespec_sub_cd,timespec_sub_timeval,timespec_sub_cf
        generic::operator(*)=>timespec_multi_timespec,timespec_multi_clongd,timespec_multi_clong,&
        timespec_multi_cint,timespec_multi_cd,timespec_multi_timeval,timespec_multi_cf
        generic::operator(/)=>timespec_div_timespec,timespec_div_clongd,timespec_div_clong,&
        timespec_div_cint,timespec_div_cd,timespec_div_timeval,timespec_div_cf
        generic::operator(**)=>timespec_expo_timespec,timespec_expo_clongd,timespec_expo_clong,&
        timespec_expo_cint,timespec_expo_cd,timespec_expo_timeval,timespec_expo_cf
        generic::write(formatted)=>timespec_write_f
        generic::write(unformatted)=>timespec_write_uf
    end type timespec

    type timeval
        integer(kind=c_long)::tv_sec=0
        integer(kind=c_long)::tv_usec=0
        contains
        procedure,private::timeval_write_f,timeval_write_uf
        procedure,private::timeval_equals_cint,timeval_equals_cd,timeval_equals_clong,timeval_equals_clongd,&
        timeval_equals_cf
        procedure,private::set_timeval,set_timeval2,timeval_equals_timespec,timeval_add_timeval,&
        timeval_sub_timeval,timeval_multi_timeval,timeval_div_timeval,timeval_expo_timeval,&
        timeval_add_clongd,timeval_sub_clongd,timeval_multi_clongd,&
        timeval_div_clongd,timeval_expo_clongd,&
        timeval_add_clong,timeval_sub_clong,timeval_multi_clong,&
        timeval_div_clong,timeval_expo_clong,&
        timeval_add_cint,timeval_sub_cint,timeval_multi_cint,&
        timeval_div_cint,timeval_expo_cint,&
        timeval_add_cd,timeval_sub_cd,timeval_multi_cd,&
        timeval_div_cd,timeval_expo_cd,&
        timeval_add_cf,timeval_sub_cf,timeval_multi_cf,timeval_div_cf,timeval_expo_cf,&
        timeval_add_timespec,timeval_sub_timespec,timeval_multi_timespec,timeval_div_timespec,&
        timeval_expo_timespec
        procedure,private::timeval_second_int,timeval_second_long,timeval_second_longd,timeval_second_double,&
        timeval_second_float,&
        timeval_nanosecond_int,timeval_nanosecond_long,timeval_nanosecond_longd,timeval_nanosecond_double,&
        timeval_nanosecond_float
        generic::sec=>timeval_second_int,timeval_second_long,timeval_second_longd,timeval_second_double,&
        timeval_second_float
        generic::micro=>timeval_nanosecond_int,timeval_nanosecond_long,timeval_nanosecond_longd,timeval_nanosecond_double,&
        timeval_nanosecond_float
        generic::set=>set_timeval,set_timeval2
        generic::assignment(=)=>timeval_equals_timespec,timeval_equals_cint,timeval_equals_cd,timeval_equals_clong,&
        timeval_equals_clongd,timeval_equals_cf
        generic::operator(+)=>timeval_add_timeval,timeval_add_clongd,timeval_add_clong,&
        timeval_add_cint,timeval_add_cd,timeval_add_timespec,timeval_add_cf
        generic::operator(-)=>timeval_sub_timeval,timeval_sub_clongd,timeval_sub_clong,&
        timeval_sub_cint,timeval_sub_cd,timeval_sub_timespec,timeval_sub_cf
        generic::operator(*)=>timeval_multi_timeval,timeval_multi_clongd,timeval_multi_clong,&
        timeval_multi_cint,timeval_multi_cd,timeval_multi_timespec,timeval_multi_cf
        generic::operator(/)=>timeval_div_timeval,timeval_div_clongd,timeval_div_clong,&
        timeval_div_cint,timeval_div_cd,timeval_div_timespec,timeval_div_cf
        generic::operator(**)=>timeval_expo_timeval,timeval_expo_clongd,timeval_expo_clong,&
        timeval_expo_cint,timeval_expo_cd,timeval_expo_timespec,timeval_expo_cf
        generic::write(formatted)=>timeval_write_f
        generic::write(unformatted)=>timeval_write_uf
    end type timeval

    type timezone
        integer(kind=c_int)::tz_minuteswest=0
        integer(kind=c_int)::tz_dsttime=0
    end type timezone

    type timeb
        integer(kind=c_long)::time
        integer(kind=c_short)::millitm
        integer(kind=c_short)::timezone
        integer(kind=c_short)::dstflag 
    end type timeb

    character(len=*),parameter::time_tm_write_f_strftime="%A %B %d %r %Y",&
    time_tm_write_uf_strftime="%A %B %d %r %Y"

    integer(kind=c_int),parameter::clock_realtime=0,clock_monotonic=1,clock_process_cputime_id=2,&
    clock_thread_cputime_id=3,clock_monotonic_raw=4,clock_realtime_coarse=5,clock_monotonic_coarse=6,&
    clock_boottime=7,clock_realtime_alarm=8,clock_boottime_alarm=9,clock_tai=11,timer_abstime=1
    
    interface
        integer(kind=c_int) function sleep_(sec) bind(C,name="sleep")
            use iso_c_binding,only:c_int
            integer(kind=c_int),intent(in),value::sec
        end function sleep_

        integer(kind=c_int) function usleep_(usec) bind(C,name="usleep")
            use iso_c_binding,only:c_int
            integer(kind=c_int),intent(in),value::usec
        end function usleep_

        integer(kind=c_int) function nanosleep_(req,rem) bind(C,name="nanosleep")!timespec,timespec
            use iso_c_binding,only:c_int,c_long,c_ptr
            type(c_ptr),intent(in),value::req,rem
        end function nanosleep_

        integer(kind=c_int) function gettimeofday_(tv,tz) bind(C,name="gettimeofday")!timeval,timezone
            use iso_c_binding,only:c_int,c_long,c_ptr
            type(c_ptr),intent(in),value::tv,tz
        end function gettimeofday_

        integer(kind=c_int) function settimeofday_(tv,tz) bind(C,name="settimeofday")!timeval,timezone
            use iso_c_binding,only:c_int,c_long,c_ptr
            type(c_ptr),intent(in),value::tv,tz
        end function settimeofday_

        integer(kind=c_int) function clock_getres_(clk_id,res) bind(C,name="clock_getres")!constant, timespec
            use iso_c_binding,only:c_int,c_ptr
            integer(kind=c_int),intent(in),value::clk_id
            type(c_ptr),intent(in),value::res
        end function clock_getres_

        integer(kind=c_int) function clock_settime_(clk_id,res) bind(C,name="clock_settime")!constant, timespec
            use iso_c_binding,only:c_int,c_ptr
            integer(kind=c_int),intent(in),value::clk_id
            type(c_ptr),intent(in),value::res
        end function clock_settime_

        integer(kind=c_int) function clock_gettime_(clk_id,tp) bind(C,name="clock_gettime")!constant, timespec
            use iso_c_binding,only:c_int,c_long,c_ptr
            integer(kind=c_int),intent(in),value::clk_id
            type(c_ptr),intent(in),value::tp
        end function clock_gettime_

        integer(kind=c_long) function timec_(tm) bind(C,name="time")
            use iso_c_binding,only:c_ptr,c_long
            type(c_ptr),intent(in),value::tm
        end function timec_

        type(c_ptr) function c_gmtime_(tm) bind(C,name="gmtime")
            use iso_c_binding,only:c_ptr
            type(c_ptr),intent(in),value::tm
        end function c_gmtime_

        type(c_ptr) function c_localtime_(tm) bind(C,name="localtime")
            use iso_c_binding,only:c_int,c_long,c_ptr
            type(c_ptr),intent(in),value::tm
        end function c_localtime_

        integer(kind=c_size_t) function strftime_(ptr,maxsize,format,timeptr) bind(C,name="strftime")
            use iso_c_binding,only:c_ptr,c_size_t
            type(c_ptr),intent(in),value::ptr,format,timeptr
            integer(kind=c_size_t),intent(in)::maxsize
        end function strftime_

        real(kind=c_double) function difftime_(end_,begin_) bind(C,name="difftime")
            use iso_c_binding,only:c_double,c_long,c_ptr
            integer(kind=c_long),intent(in),value::end_,begin_
        end function difftime_

        type(c_ptr) function asctime_(tm) bind(C,name="asctime")
            use iso_c_binding,only:c_double,c_long,c_ptr
            type(c_ptr),intent(in),value::tm
        end function asctime_

        type(c_ptr) function ctime_(tm) bind(C,name="ctime")
            use iso_c_binding,only:c_double,c_long,c_ptr
            type(c_ptr),intent(in),value::tm
        end function ctime_

        integer(kind=c_long)function mktime_(tm) bind(C,name="mktime")
            use iso_c_binding,only:c_double,c_long,c_ptr
            type(c_ptr),intent(in),value::tm
        end function mktime_

        integer(kind=c_long)function clock_() bind(C,name="clock")
            use iso_c_binding,only:c_long
        end function clock_

        subroutine c_free(cptr) bind(C,name="free")
            use iso_c_binding
            type(c_ptr)::cptr
        end subroutine
    end interface
    
    interface assignment(=)
        procedure::int_equals_tm,long_equals_tm,double_equals_tm,float_equals_tm,longd_equals_tm,&

        long_equals_timespec,int_equals_timespec,float_equals_timespec,double_equals_timespec,&
        longd_equals_timespec,&

        long_equals_timeval,int_equals_timeval,float_equals_timeval,double_equals_timeval,&
        longd_equals_timeval!,&!,&

        !long_equals_time_tm,int_equals_time_tm,float_equals_time_tm,double_equals_time_tm!,&
       ! longd_equals_time_tm


    end interface

    interface operator(+)
        procedure::cint_add_timespec,clong_add_timespec,clongd_add_timespec,cd_add_timespec
    end interface

    interface operator(-)
        procedure::cint_sub_timespec,clong_sub_timespec,clongd_sub_timespec,cd_sub_timespec
    end interface

    interface operator(*)
    procedure::cint_multi_timespec,clong_multi_timespec,clongd_multi_timespec,cd_multi_timespec
    end interface

    interface operator(/)
    procedure::cint_div_timespec,clong_div_timespec,clongd_div_timespec,cd_div_timespec
    end interface

    interface operator(**)
    procedure::cint_expo_timespec,clong_expo_timespec,clongd_expo_timespec,cd_expo_timespec
    end interface

    interface nanosleep
        procedure::nanosleep1,nanosleep2,nanosleep3,nanosleep4,nanosleep5,nanosleep6,nanosleep7,&
        nanosleep8,nanosleep9,nanosleep10,nanosleep11,nanosleep12,nanosleep13
    end interface nanosleep
    
    interface sleep
        procedure::sleep1,sleep2,sleep3,sleep4,sleepd1,sleepd2,sleepd3,sleepd4,sleepd5,sleepd6
    end interface sleep

    interface usleep
        procedure::usleep1,usleep2,usleep_long1,usleep_long2,usleep_float1,&
        usleep_float2,usleep_double1,usleep_double2,usleep_longd1,usleep_longd2
    end interface usleep

    interface microsleep
        procedure::microsleep1,microsleep2,microsleep3,microsleepd1,microsleepd2,microsleepd3,&
        microsleep4,microsleepd4,microsleepd5,microsleepd6
    end interface

    interface millisleep
        procedure::millisleep1,millisleep2,millisleep3,millisleepd1,millisleepd2,millisleepd3,&
        millisleep4,millisleepd4,millisleepd5,millisleepd6
    end interface millisleep

    interface gettimeofday_nr
        procedure::gettimeofday1,gettimeofday2,gettimeofday3,gettimeofday4
    end interface gettimeofday_nr

    interface gettimeofday
        procedure::gettimeofday5,gettimeofday6,gettimeofday7
    end interface gettimeofday

    interface settimeofday
        procedure::settimeofday1,settimeofday2
    end interface settimeofday

    interface clock_getres_nr
        procedure::clock_getres1,clock_getres2,clock_getres3
    end interface clock_getres_nr

    interface clock_getres
        procedure::clock_getres4,clock_getres5,clock_getres6
    end interface clock_getres

    interface clock_settime
        procedure::clock_settime1,clock_settime2,clock_settime3
    end interface clock_settime

    interface clock_gettime_nr
        procedure::clock_gettime1,clock_gettime2,clock_gettime3
    end interface clock_gettime_nr

    interface clock_gettime
        procedure::clock_gettime4,clock_gettime5,clock_gettime6
    end interface clock_gettime

    interface gmtime
        procedure::gmtimec1,gmtimec2,gmtimec3,gmtimec4,gmtimec5,gmtimec6,gmtimec7,gmtimec8
    end interface gmtime

    interface localtime
        procedure::localtime1,localtime2,localtime3,localtime4,localtime5,localtime6,localtime7,&
        localtime8
    end interface localtime

    interface strtime !string  ctime() in C
        procedure::c_time1,c_time2,c_time3,c_time4,c_time5,c_time6,c_time7,c_time8
    end interface strtime
    
    interface timec !decimal  time() in C
        procedure::timec1,timec2
    end interface timec

    interface asctime
        procedure::asctime1,asctime2,asctime3,asctime4,asctime5,asctime6,&
        asctime7,asctime8,asctime9
    end interface asctime

    interface mktime
        procedure::mktime1,mktime2
    end interface mktime

    interface strftime
        procedure::strftime1,strftime_timespec,strftime_timeval,strftime_long,strftime_int,&
        strftime_float,strftime_double,strftime_longd
    end interface strftime
    
    interface interval
        procedure::tv_interval,clock_interval,tvclock_interval1,tvclock_interval2,&
        tv_interval_int,tv_interval_double,tv_interval_float,tv_interval_long,tv_interval_longd,&
        tv_interval_int2,tv_interval_double2,tv_interval_float2,tv_interval_long2,tv_interval_longd2,&
        clock_interval_int,clock_interval_double,clock_interval_float,clock_interval_long,clock_interval_longd,&
        clock_interval_int2,clock_interval_double2,clock_interval_float2,clock_interval_long2,clock_interval_longd2,&
        
        tvclock_interval_time_tm_timespec1,tvclock_interval_time_tm_timespec2,tvclock_interval_time_tm_timeval1,&
        tvclock_interval_time_tm_timeval2,tvclock_interval_time_tm_int1,tvclock_interval_time_tm_int2,&
        tvclock_interval_time_tm_long1,tvclock_interval_time_tm_long2,tvclock_interval_time_tm_float1,&
        tvclock_interval_time_tm_float2,tvclock_interval_time_tm_double1,tvclock_interval_time_tm_double2,&
        tvclock_interval_time_tm_longd1,tvclock_interval_time_tm_longd2
        end interface interval

    interface interval_milli
        procedure::tv_interval_milli,clock_interval_milli,tvclock_interval_milli1,tvclock_interval_milli2,&
        tv_interval_milli_int,tv_interval_milli_double,tv_interval_milli_float,tv_interval_milli_long,&
        tv_interval_milli_longd,&
        tv_interval_milli_int2,tv_interval_milli_double2,tv_interval_milli_float2,tv_interval_milli_long2,&
        tv_interval_milli_longd2,&
        clock_interval_milli_int,clock_interval_milli_double,clock_interval_milli_float,clock_interval_milli_long,&
        clock_interval_milli_longd,&
        clock_interval_milli_int2,clock_interval_milli_double2,clock_interval_milli_float2,clock_interval_milli_long2,&
        clock_interval_milli_longd2,&

        tvclock_interval_milli_time_tm_timespec1,tvclock_interval_milli_time_tm_timespec2,tvclock_interval_milli_time_tm_timeval1,&
        tvclock_interval_milli_time_tm_timeval2,tvclock_interval_milli_time_tm_int1,tvclock_interval_milli_time_tm_int2,&
        tvclock_interval_milli_time_tm_long1,tvclock_interval_milli_time_tm_long2,tvclock_interval_milli_time_tm_float1,&
        tvclock_interval_milli_time_tm_float2,tvclock_interval_milli_time_tm_double1,tvclock_interval_milli_time_tm_double2,&
        tvclock_interval_milli_time_tm_longd1,tvclock_interval_milli_time_tm_longd2
    end interface interval_milli

    interface interval_micro
        procedure::tv_interval_micro,clock_interval_micro,tvclock_interval_micro1,tvclock_interval_micro2,&
        tv_interval_micro_int,tv_interval_micro_double,tv_interval_micro_float,tv_interval_micro_long,&
        tv_interval_micro_longd,&
        tv_interval_micro_int2,tv_interval_micro_double2,tv_interval_micro_float2,tv_interval_micro_long2,&
        tv_interval_micro_longd2,&
        clock_interval_micro_int,clock_interval_micro_double,clock_interval_micro_float,clock_interval_micro_long,&
        clock_interval_micro_longd,&
        clock_interval_micro_int2,clock_interval_micro_double2,clock_interval_micro_float2,clock_interval_micro_long2,&
        clock_interval_micro_longd2,&

        tvclock_interval_micro_time_tm_timespec1,tvclock_interval_micro_time_tm_timespec2,tvclock_interval_micro_time_tm_timeval1,&
        tvclock_interval_micro_time_tm_timeval2,tvclock_interval_micro_time_tm_int1,tvclock_interval_micro_time_tm_int2,&
        tvclock_interval_micro_time_tm_long1,tvclock_interval_micro_time_tm_long2,tvclock_interval_micro_time_tm_float1,&
        tvclock_interval_micro_time_tm_float2,tvclock_interval_micro_time_tm_double1,tvclock_interval_micro_time_tm_double2,&
        tvclock_interval_micro_time_tm_longd1,tvclock_interval_micro_time_tm_longd2
    end interface interval_micro

    interface interval_nano
        procedure::tv_interval_nano,clock_interval_nano,tvclock_interval_nano1,tvclock_interval_nano2,&
        tv_interval_nano_int,tv_interval_nano_double,tv_interval_nano_float,tv_interval_nano_long,&
        tv_interval_nano_longd,&
        tv_interval_nano_int2,tv_interval_nano_double2,tv_interval_nano_float2,tv_interval_nano_long2,&
        tv_interval_nano_longd2,&
        clock_interval_nano_int,clock_interval_nano_double,clock_interval_nano_float,clock_interval_nano_long,&
        clock_interval_nano_longd,&
        clock_interval_nano_int2,clock_interval_nano_double2,clock_interval_nano_float2,clock_interval_nano_long2,&
        clock_interval_nano_longd2,&

        tvclock_interval_nano_time_tm_timespec1,tvclock_interval_nano_time_tm_timespec2,tvclock_interval_nano_time_tm_timeval1,&
        tvclock_interval_nano_time_tm_timeval2,tvclock_interval_nano_time_tm_int1,tvclock_interval_nano_time_tm_int2,&
        tvclock_interval_nano_time_tm_long1,tvclock_interval_nano_time_tm_long2,tvclock_interval_nano_time_tm_float1,&
        tvclock_interval_nano_time_tm_float2,tvclock_interval_nano_time_tm_double1,tvclock_interval_nano_time_tm_double2,&
        tvclock_interval_nano_time_tm_longd1,tvclock_interval_nano_time_tm_longd2
    end interface interval_nano

    interface interval_min
        procedure::tv_interval_min,clock_interval_min,tvclock_interval_min1,tvclock_interval_min2,tv_interval_min_int,&
        tv_interval_min_double,tv_interval_min_float,tv_interval_min_long,tv_interval_min_longd,&
        tv_interval_min_int2,tv_interval_min_double2,tv_interval_min_float2,tv_interval_min_long2,&
        tv_interval_min_longd2,clock_interval_min_int,clock_interval_min_double,clock_interval_min_float,&
        clock_interval_min_long,clock_interval_min_longd,clock_interval_min_int2,clock_interval_min_double2,&
        clock_interval_min_float2,clock_interval_min_long2,clock_interval_min_longd2,&

        tvclock_interval_min_time_tm_timespec1,tvclock_interval_min_time_tm_timespec2,&
        tvclock_interval_min_time_tm_timeval1,tvclock_interval_min_time_tm_timeval2,tvclock_interval_min_time_tm_int1,&
        tvclock_interval_min_time_tm_int2,tvclock_interval_min_time_tm_long1,tvclock_interval_min_time_tm_long2,&
        tvclock_interval_min_time_tm_float1,tvclock_interval_min_time_tm_float2,tvclock_interval_min_time_tm_double1,&
        tvclock_interval_min_time_tm_double2,tvclock_interval_min_time_tm_longd1,tvclock_interval_min_time_tm_longd2

    end interface interval_min
    !All in seconds below---------------------------------------
    interface interval_hour
        procedure::tv_interval_hour,clock_interval_hour,tvclock_interval_hour1,tvclock_interval_hour2,tv_interval_hour_int,&
        tv_interval_hour_double,tv_interval_hour_float,tv_interval_hour_long,tv_interval_hour_longd,&
        tv_interval_hour_int2,tv_interval_hour_double2,tv_interval_hour_float2,tv_interval_hour_long2,&
        tv_interval_hour_longd2,clock_interval_hour_int,clock_interval_hour_double,clock_interval_hour_float,&
        clock_interval_hour_long,clock_interval_hour_longd,clock_interval_hour_int2,clock_interval_hour_double2,&
        clock_interval_hour_float2,clock_interval_hour_long2,clock_interval_hour_longd2,&
        
        tvclock_interval_hour_time_tm_timespec1,tvclock_interval_hour_time_tm_timespec2,&
        tvclock_interval_hour_time_tm_timeval1,tvclock_interval_hour_time_tm_timeval2,tvclock_interval_hour_time_tm_int1,&
        tvclock_interval_hour_time_tm_int2,tvclock_interval_hour_time_tm_long1,tvclock_interval_hour_time_tm_long2,&
        tvclock_interval_hour_time_tm_float1,tvclock_interval_hour_time_tm_float2,tvclock_interval_hour_time_tm_double1,&
        tvclock_interval_hour_time_tm_double2,tvclock_interval_hour_time_tm_longd1,tvclock_interval_hour_time_tm_longd2
        end interface interval_hour

    interface interval_day
        procedure::tv_interval_day,clock_interval_day,tvclock_interval_day1,tvclock_interval_day2,tv_interval_day_int,&
        tv_interval_day_double,tv_interval_day_float,tv_interval_day_long,tv_interval_day_longd,&
        tv_interval_day_int2,tv_interval_day_double2,tv_interval_day_float2,tv_interval_day_long2,&
        tv_interval_day_longd2,clock_interval_day_int,clock_interval_day_double,clock_interval_day_float,&
        clock_interval_day_long,clock_interval_day_longd,clock_interval_day_int2,clock_interval_day_double2,&
        clock_interval_day_float2,clock_interval_day_long2,clock_interval_day_longd2,&
        
        tvclock_interval_day_time_tm_timespec1,tvclock_interval_day_time_tm_timespec2,&
        tvclock_interval_day_time_tm_timeval1,tvclock_interval_day_time_tm_timeval2,tvclock_interval_day_time_tm_int1,&
        tvclock_interval_day_time_tm_int2,tvclock_interval_day_time_tm_long1,tvclock_interval_day_time_tm_long2,&
        tvclock_interval_day_time_tm_float1,tvclock_interval_day_time_tm_float2,tvclock_interval_day_time_tm_double1,&
        tvclock_interval_day_time_tm_double2,tvclock_interval_day_time_tm_longd1,tvclock_interval_day_time_tm_longd2
    end interface interval_day

    interface interval_week
        procedure::tv_interval_week,clock_interval_week,tvclock_interval_week1,tvclock_interval_week2,tv_interval_week_int,&
        tv_interval_week_double,tv_interval_week_float,tv_interval_week_long,tv_interval_week_longd,&
        tv_interval_week_int2,tv_interval_week_double2,tv_interval_week_float2,tv_interval_week_long2,&
        tv_interval_week_longd2,clock_interval_week_int,clock_interval_week_double,clock_interval_week_float,&
        clock_interval_week_long,clock_interval_week_longd,clock_interval_week_int2,clock_interval_week_double2,&
        clock_interval_week_float2,clock_interval_week_long2,clock_interval_week_longd2,&
        
        tvclock_interval_week_time_tm_timespec1,tvclock_interval_week_time_tm_timespec2,&
        tvclock_interval_week_time_tm_timeval1,tvclock_interval_week_time_tm_timeval2,tvclock_interval_week_time_tm_int1,&
        tvclock_interval_week_time_tm_int2,tvclock_interval_week_time_tm_long1,tvclock_interval_week_time_tm_long2,&
        tvclock_interval_week_time_tm_float1,tvclock_interval_week_time_tm_float2,tvclock_interval_week_time_tm_double1,&
        tvclock_interval_week_time_tm_double2,tvclock_interval_week_time_tm_longd1,tvclock_interval_week_time_tm_longd2
    end interface interval_week

    interface interval_year !365.24218967  days in a year
        procedure::tv_interval_year,clock_interval_year,tvclock_interval_year1,tvclock_interval_year2,tv_interval_year_int,&
        tv_interval_year_double,tv_interval_year_float,tv_interval_year_long,tv_interval_year_longd,&
        tv_interval_year_int2,tv_interval_year_double2,tv_interval_year_float2,tv_interval_year_long2,&
        tv_interval_year_longd2,clock_interval_year_int,clock_interval_year_double,clock_interval_year_float,&
        clock_interval_year_long,clock_interval_year_longd,clock_interval_year_int2,clock_interval_year_double2,&
        clock_interval_year_float2,clock_interval_year_long2,clock_interval_year_longd2,&
        
        tvclock_interval_year_time_tm_timespec1,tvclock_interval_year_time_tm_timespec2,&
        tvclock_interval_year_time_tm_timeval1,tvclock_interval_year_time_tm_timeval2,tvclock_interval_year_time_tm_int1,&
        tvclock_interval_year_time_tm_int2,tvclock_interval_year_time_tm_long1,tvclock_interval_year_time_tm_long2,&
        tvclock_interval_year_time_tm_float1,tvclock_interval_year_time_tm_float2,tvclock_interval_year_time_tm_double1,&
        tvclock_interval_year_time_tm_double2,tvclock_interval_year_time_tm_longd1,tvclock_interval_year_time_tm_longd2
    end interface interval_year

    contains
        subroutine time_tm_write_f(this, unit, iotype, v_list, iostat, iomsg)
            class(time_tm), intent(in)    :: this      
            integer, intent(in)         :: unit     
            character(*), intent(in)    :: iotype    
            integer,intent(in)         :: v_list(:) 
            integer, intent(out)        :: iostat    
            character(*), intent(inout) :: iomsg     
            
            write (*,'(a)',advance='no') strftime(time_tm_write_f_strftime,this)
                
            !write(*,"(/)") 
        end subroutine time_tm_write_f
        subroutine time_tm_write_uf(this, unit, iostat, iomsg)
            class(time_tm), intent(in)    :: this     
            integer, intent(in)         :: unit      
            integer, intent(out)        :: iostat    
            character(*), intent(inout) :: iomsg     
            
            write (unit, "(a)", IOSTAT=iostat, IOMSG=iomsg,advance='no') strftime(time_tm_write_uf_strftime,this)
       
            !write(*,"(/)") 
          end subroutine time_tm_write_uf

          subroutine timespec_write_f(this, unit, iotype, v_list, iostat, iomsg)
            class(timespec), intent(in)    :: this      
            integer, intent(in)         :: unit      
            character(*), intent(in)    :: iotype    
            integer,intent(in)         :: v_list(:) 
            integer, intent(out)        :: iostat    
            character(*), intent(inout) :: iomsg     
            character(len=:),allocatable::tm0,tm1
            
            tm0=formatter(this%tv_sec)
            tm1=formatter(this%tv_nsec)
            write (*,"(a,"//tm0//",a,"//tm1//")",advance='no') "seconds: ",this%tv_sec," nanoseconds: ",this%tv_nsec
            
            !write(*,"(/)") 
        end subroutine timespec_write_f
        subroutine timespec_write_uf(this, unit, iostat, iomsg)
            class(timespec), intent(in)    :: this     
            integer, intent(in)         :: unit      
            integer, intent(out)        :: iostat    
            character(*), intent(inout) :: iomsg     
            character(len=:),allocatable::tm0,tm1
            
            tm0=formatter(this%tv_sec)
            tm1=formatter(this%tv_nsec)
                write (unit, "(a,"//tm0//",a,"//tm1//")", IOSTAT=iostat, IOMSG=iomsg,advance='no') "seconds: ",&
                this%tv_sec," nanoseconds: ",this%tv_nsec
           
           ! end select 
            !write(*,"(/)") 
          end subroutine timespec_write_uf
          function formatter(val) result(m)
            integer(kind=c_long),intent(in)::val
            integer::i
            character(len=:),allocatable::m
            real(kind=c_long_double)::tmp
            tmp=val
            if(tmp==0) then
                m="I1"
            else
                i=0
                do while(tmp>=1)
                    tmp=tmp/10
                    i=i+1
                end do
                allocate(character(len=8)::m)
                write(m,"(I5)") i
                m="I"//m
            end if
        end function formatter
          subroutine timeval_write_f(this, unit, iotype, v_list, iostat, iomsg)
            class(timeval), intent(in)    :: this     
            integer, intent(in)         :: unit      
            character(*), intent(in)    :: iotype   
            integer,intent(in)         :: v_list(:) 
            integer, intent(out)        :: iostat   
            character(*), intent(inout) :: iomsg     
            character(len=:),allocatable::tm0,tm1
            
            tm0=formatter(this%tv_sec)
            tm1=formatter(this%tv_usec)
            write (*,"(a,"//tm0//",a,"//tm1//")",advance='no') "seconds: ",this%tv_sec," microseconds: ",this%tv_usec
            !write(*,"(/)") 
        end subroutine timeval_write_f
        subroutine timeval_write_uf(this, unit, iostat, iomsg)
            class(timeval), intent(in)    :: this      
            integer, intent(in)         :: unit     
            integer, intent(out)        :: iostat   
            character(*), intent(inout) :: iomsg     
            character(len=:),allocatable::tm0,tm1
           
            tm0=formatter(this%tv_sec)
            tm1=formatter(this%tv_usec)
            write (unit, "(a,"//tm0//",a,"//tm1//")", IOSTAT=iostat, IOMSG=iomsg,advance='no') "seconds: ",&
            this%tv_sec," microseconds: ",this%tv_usec
           
            !write(*,"(/)") 
          end subroutine timeval_write_uf

        function timespec_second_int(this,other) result(t)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::t
            t%tv_sec=other
            t%tv_nsec=this%tv_nsec
        end function timespec_second_int
        function timespec_second_long(this,other) result(t)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::t
            t%tv_sec=other
            t%tv_nsec=this%tv_nsec
        end function timespec_second_long
        function timespec_second_longd(this,other) result(t)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::t
            t%tv_sec=int(other,kind=c_long)
            t%tv_nsec=this%tv_nsec
        end function timespec_second_longd
        function timespec_second_double(this,other) result(t)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::t
            t%tv_sec=int(other,kind=c_long)
            t%tv_nsec=this%tv_nsec
        end function timespec_second_double
        function timespec_second_float(this,other) result(t)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::t
            t%tv_sec=int(other,kind=c_long)
            t%tv_nsec=this%tv_nsec
        end function timespec_second_float

        function timespec_nanosecond_int(this,other) result(t)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::t
            t%tv_sec=this%tv_sec
            t%tv_nsec=other
        end function timespec_nanosecond_int
        function timespec_nanosecond_long(this,other) result(t)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::t
            t%tv_sec=this%tv_sec
            t%tv_nsec=other
        end function timespec_nanosecond_long
        function timespec_nanosecond_longd(this,other) result(t)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::t
            t%tv_sec=this%tv_sec
            t%tv_nsec=int(other,kind=c_long)
        end function timespec_nanosecond_longd
        function timespec_nanosecond_double(this,other) result(t)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::t
            t%tv_sec=this%tv_sec
            t%tv_nsec=int(other,kind=c_long)
        end function timespec_nanosecond_double
        function timespec_nanosecond_float(this,other) result(t)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::t
            t%tv_sec=this%tv_sec
            t%tv_nsec=int(other,kind=c_long)
        end function timespec_nanosecond_float
!----------------------------------------------------------------------------------------
        function timeval_second_int(this,other) result(t)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::t
            t%tv_sec=other
            t%tv_usec=this%tv_usec
        end function timeval_second_int
        function timeval_second_long(this,other) result(t)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::t
            t%tv_sec=other
            t%tv_usec=this%tv_usec
        end function timeval_second_long
        function timeval_second_longd(this,other) result(t)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::t
            t%tv_sec=int(other,kind=c_long)
            t%tv_usec=this%tv_usec
        end function timeval_second_longd
        function timeval_second_double(this,other) result(t)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::t
            t%tv_sec=int(other,kind=c_long)
            t%tv_usec=this%tv_usec
        end function timeval_second_double
        function timeval_second_float(this,other) result(t)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::t
            t%tv_sec=int(other,kind=c_long)
            t%tv_usec=this%tv_usec
        end function timeval_second_float

        function timeval_nanosecond_int(this,other) result(t)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::t
            t%tv_sec=this%tv_sec
            t%tv_usec=other
        end function timeval_nanosecond_int
        function timeval_nanosecond_long(this,other) result(t)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::t
            t%tv_sec=this%tv_sec
            t%tv_usec=other
        end function timeval_nanosecond_long
        function timeval_nanosecond_longd(this,other) result(t)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::t
            t%tv_sec=this%tv_sec
            t%tv_usec=int(other,kind=c_long)
        end function timeval_nanosecond_longd
        function timeval_nanosecond_double(this,other) result(t)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::t
            t%tv_sec=this%tv_sec
            t%tv_usec=int(other,kind=c_long)
        end function timeval_nanosecond_double
        function timeval_nanosecond_float(this,other) result(t)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::t
            t%tv_sec=this%tv_sec
            t%tv_usec=int(other,kind=c_long)
        end function timeval_nanosecond_float

        subroutine time_tm_local(this)
            class(time_tm),intent(inout)::this
            this=localtime()
        end subroutine time_tm_local
        subroutine time_tm_local_long(this,val)
            class(time_tm),intent(inout)::this
            integer(kind=c_long),intent(in)::val
            this=localtime(val)
        end subroutine time_tm_local_long
        subroutine time_tm_local_int(this,val)
            class(time_tm),intent(inout)::this
            integer(kind=c_int),intent(in)::val
            this=localtime(int(val,kind=c_long))
        end subroutine time_tm_local_int
        subroutine time_tm_gm(this)
            class(time_tm),intent(inout)::this
            this=gmtime()
        end subroutine time_tm_gm
        subroutine time_tm_gm_int(this,val)
            class(time_tm),intent(inout)::this
            integer(kind=c_int),intent(in)::val
            this=gmtime(int(val,kind=c_long))
        end subroutine time_tm_gm_int
        subroutine time_tm_gm_long(this,val)
            class(time_tm),intent(inout)::this
            integer(kind=c_long),intent(in)::val
            this=gmtime(val)
        end subroutine time_tm_gm_long

        function tm_to_str(this) result(t)
            class(time_tm),intent(in)::this
            character(len=:),allocatable::t
            t=asctime(this)
        end function tm_to_str

        subroutine tm_equals_tm(this,other)
            class(time_tm),intent(inout)::this
            type(time_tm),intent(in)::other
            this%tm_sec=other%tm_sec
            this%tm_min=other%tm_min
            this%tm_hour=other%tm_hour
            this%tm_mday=other%tm_mday
            this%tm_mon=other%tm_mon
            this%tm_year=other%tm_year
            this%tm_wday=other%tm_wday
            this%tm_yday=other%tm_yday
            this%tm_isdst=other%tm_isdst
        end subroutine tm_equals_tm

        subroutine tm_equals_int(this,other)
            class(time_tm),intent(inout)::this
            integer(kind=c_int),intent(in)::other
            this=localtime(int(other,kind=c_long))
        end subroutine tm_equals_int

        subroutine tm_equals_long(this,other)
            class(time_tm),intent(inout)::this
            integer(kind=c_long),intent(in)::other
            this=localtime(int(other,kind=c_long))
        end subroutine tm_equals_long

        subroutine tm_equals_double(this,other)
            class(time_tm),intent(inout)::this
            real(kind=c_double),intent(in)::other
            this=localtime(int(other,kind=c_long))
        end subroutine tm_equals_double

        subroutine tm_equals_float(this,other)
            class(time_tm),intent(inout)::this
            real(kind=c_float),intent(in)::other
            this=localtime(int(other,kind=c_long))
        end subroutine tm_equals_float

        subroutine tm_equals_longd(this,other)
            class(time_tm),intent(inout)::this
            real(kind=c_long_double),intent(in)::other
            this=localtime(int(other,kind=c_long))
        end subroutine tm_equals_longd
!--------------------------------------------------------------------
        function tm_add_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t
            t=localtime(mktime(this)+other)
        end function tm_add_int

        function tm_sub_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t
            t=localtime(mktime(this)-other)
        end function tm_sub_int

        function tm_multi_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t
            t=localtime(mktime(this)*other)
        end function tm_multi_int
        function tm_div_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t
            t=localtime(mktime(this)/other)
        end function tm_div_int
        function tm_expo_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t
            t=localtime(mktime(this)**other)
        end function tm_expo_int
!-------------------------------------------------------------------------
        function tm_add_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t
            t=localtime(int(real(mktime(this),kind=c_float)+other,kind=c_long))
        end function tm_add_float

        function tm_sub_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t
            t=localtime(int(real(mktime(this),kind=c_float)-other,kind=c_long))
        end function tm_sub_float

        function tm_multi_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t
            t=localtime(int(real(mktime(this),kind=c_float)*other,kind=c_long))
        end function tm_multi_float
        function tm_div_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t
            t=localtime(int(real(mktime(this),kind=c_float)/other,kind=c_long))
        end function tm_div_float
        function tm_expo_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t
            t=localtime(int(real(mktime(this),kind=c_float)**other,kind=c_long))
        end function tm_expo_float

        !-------------------------------------------------------------------------
        function tm_add_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)+other,kind=c_long))
        end function tm_add_double

        function tm_sub_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)-other,kind=c_long))
        end function tm_sub_double

        function tm_multi_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)*other,kind=c_long))
        end function tm_multi_double
        function tm_div_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)/other,kind=c_long))
        end function tm_div_double
        function tm_expo_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)**other,kind=c_long))
        end function tm_expo_double

        !-------------------------------------------------------------------------
        function tm_add_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)+other,kind=c_long))
        end function tm_add_long

        function tm_sub_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)-other,kind=c_long))
        end function tm_sub_long

        function tm_multi_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)*other,kind=c_long))
        end function tm_multi_long
        function tm_div_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)/other,kind=c_long))
        end function tm_div_long
        function tm_expo_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)**other,kind=c_long))
        end function tm_expo_long

!-------------------------------------------------------------------------
        function tm_add_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)+other,kind=c_long))
        end function tm_add_longd

        function tm_sub_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)-other,kind=c_long))
        end function tm_sub_longd

        function tm_multi_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)*other,kind=c_long))
        end function tm_multi_longd
        function tm_div_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)/other,kind=c_long))
        end function tm_div_longd
        function tm_expo_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t
            t=localtime(int(mktime(this)**other,kind=c_long))
        end function tm_expo_longd
!---------------------------------------------------------------------------------
        subroutine int_equals_tm(this,other)
            integer(kind=c_int),intent(inout)::this
            class(time_tm),intent(in)::other
            this=int(mktime(other),kind=c_int)
        end subroutine int_equals_tm

        subroutine long_equals_tm(this,other)
            integer(kind=c_long),intent(inout)::this
            class(time_tm),intent(in)::other
            this=mktime(other)
        end subroutine long_equals_tm

        subroutine double_equals_tm(this,other)
            real(kind=c_double),intent(inout)::this
            class(time_tm),intent(in)::other
            this=mktime(other)
        end subroutine double_equals_tm

        subroutine float_equals_tm(this,other)
            real(kind=c_float),intent(inout)::this
            class(time_tm),intent(in)::other
            this=real(mktime(other),kind=c_float)
        end subroutine float_equals_tm

        subroutine longd_equals_tm(this,other)
            real(kind=c_long_double),intent(inout)::this
            class(time_tm),intent(in)::other
            this=mktime(other)
        end subroutine longd_equals_tm

        real(kind=c_long_double) function tvclock_interval1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+(((t2_nsec/1000)-t1_usec)/1000000)
        end function tvclock_interval1

        real(kind=c_long_double) function tvclock_interval2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-(t1_nsec/1000))/1000000)
        end function tvclock_interval2

        real(kind=c_long_double) function tvclock_interval_milli1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+(((t2_nsec/1000)-t1_usec)/1000)
        end function tvclock_interval_milli1

        real(kind=c_long_double) function tvclock_interval_milli2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-(t1_nsec/1000))/1000)
        end function tvclock_interval_milli2

        real(kind=c_long_double) function tvclock_interval_micro1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec/1000)-t1_usec)
        end function tvclock_interval_micro1

        real(kind=c_long_double) function tvclock_interval_micro2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-(t1_nsec/1000))
        end function tvclock_interval_micro2

        real(kind=c_long_double) function tvclock_interval_nano1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-(t1_usec*1000))
        end function tvclock_interval_nano1

        real(kind=c_long_double) function tvclock_interval_nano2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec*1000)-t1_nsec)
        end function tvclock_interval_nano2

        !-------------------------------time_tm-------------------------------------
        real(kind=c_long_double) function tvclock_interval_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+(t2_nsec/1000000000)
        end function tvclock_interval_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)-(t1_nsec/1000000000)
        end function tvclock_interval_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+(t2_usec/1000000)
        end function tvclock_interval_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)-(t1_usec/1000000)
        end function tvclock_interval_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_time_tm_int2(time1,time2) result(difference)!time2-time1
        integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_time_tm_long2(time1,time2) result(difference)!time2-time1
        integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=t2_sec-t1_sec
        end function tvclock_interval_time_tm_longd2
        !-----------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_min_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(((t2_sec-t1_sec))+(t2_nsec/1000000000))/60
        end function tvclock_interval_min_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_nsec/1000000000)))/60
        end function tvclock_interval_min_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_min_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(((t2_sec-t1_sec))+(t2_usec/1000000))/60
        end function tvclock_interval_min_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_usec/1000000)))/60
        end function tvclock_interval_min_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_min_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_min_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_min_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_min_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_min_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_min_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/60
        end function tvclock_interval_min_time_tm_longd2

        !-----------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_hour_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(((t2_sec-t1_sec))+(t2_nsec/1000000000))/3600
        end function tvclock_interval_hour_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_nsec/1000000000)))/3600
        end function tvclock_interval_hour_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(((t2_sec-t1_sec))+(t2_usec/1000000))/3600
        end function tvclock_interval_hour_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_usec/1000000)))/3600
        end function tvclock_interval_hour_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_hour_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/3600
        end function tvclock_interval_hour_time_tm_longd2

        !-----------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_day_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(((t2_sec-t1_sec))+(t2_nsec/1000000000))/86400
        end function tvclock_interval_day_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_nsec/1000000000)))/86400
        end function tvclock_interval_day_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_day_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(((t2_sec-t1_sec))+(t2_usec/1000000))/86400
        end function tvclock_interval_day_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_usec/1000000)))/86400
        end function tvclock_interval_day_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_day_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_day_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_day_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_day_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_day_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_day_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/86400
        end function tvclock_interval_day_time_tm_longd2
!-----------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_week_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(((t2_sec-t1_sec))+(t2_nsec/1000000000))/604800
        end function tvclock_interval_week_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_nsec/1000000000)))/604800
        end function tvclock_interval_week_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_week_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(((t2_sec-t1_sec))+(t2_usec/1000000))/604800
        end function tvclock_interval_week_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_usec/1000000)))/604800
        end function tvclock_interval_week_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_week_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_week_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_week_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_week_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_week_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_week_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/604800
        end function tvclock_interval_week_time_tm_longd2
 !-----------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_year_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1 !1 astronomical year, 1 rotation around the sun
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(((t2_sec-t1_sec))+(t2_nsec/1000000000))/31556925.19
        end function tvclock_interval_year_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_nsec/1000000000)))/31556925.19
        end function tvclock_interval_year_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_year_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(((t2_sec-t1_sec))+(t2_usec/1000000))/31556925.19
        end function tvclock_interval_year_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=(((t2_sec-t1_sec))+(-(t1_usec/1000000)))/31556925.19
        end function tvclock_interval_year_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_year_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_year_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_year_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_year_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_year_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_year_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=(t2_sec-t1_sec)/31556925.19
        end function tvclock_interval_year_time_tm_longd2
    !--------------------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_milli_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+(t2_nsec/1000000)
        end function tvclock_interval_milli_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)+(-(t1_nsec/1000000))
        end function tvclock_interval_milli_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+(t2_usec/1000)
        end function tvclock_interval_milli_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)+(-(t1_usec/1000))
        end function tvclock_interval_milli_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_milli_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000)
        end function tvclock_interval_milli_time_tm_longd2
!----------------------------------------------------------------------------------------------------
        real(kind=c_long_double) function tvclock_interval_micro_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+(t2_nsec/1000)
        end function tvclock_interval_micro_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)+(-(t1_nsec/1000))
        end function tvclock_interval_micro_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec/1000)
        end function tvclock_interval_micro_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)+(-(t1_usec/1000))
        end function tvclock_interval_micro_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_micro_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000)
        end function tvclock_interval_micro_time_tm_longd2
        !------------------------------------------------------------------

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_timespec1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_nsec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+t2_nsec
        end function tvclock_interval_nano_time_tm_timespec1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_timespec2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)-t1_nsec
        end function tvclock_interval_nano_time_tm_timespec2

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_timeval1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec,t2_usec
            t1_sec=mktime(time1)
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_usec*1000)
        end function tvclock_interval_nano_time_tm_timeval1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_timeval2(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)-(t1_usec*1000)
        end function tvclock_interval_nano_time_tm_timeval2

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_int1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_int),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_int1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_int2(time1,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_int2

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_long1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            integer(kind=c_long),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_long1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_long2(time1,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_long2

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_float1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_float),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_float1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_float2(time1,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_float2

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_double1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_double1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_double2(time1,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_double2

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_longd1(time1,time2) result(difference)!time2-time1
            type(time_tm),intent(in)::time1
            real(kind=c_long_double),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=mktime(time1)
            t2_sec=time2
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_longd1

        real(kind=c_long_double) function tvclock_interval_nano_time_tm_longd2(time1,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::time1
            type(time_tm),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t2_sec
            t1_sec=time1
            t2_sec=mktime(time2)
            difference=((t2_sec-t1_sec)*1000000000)
        end function tvclock_interval_nano_time_tm_longd2

!---------------------------Interval-MIN--------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_min(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min

        real(kind=c_long_double) function clock_interval_min(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min

        real(kind=c_long_double) function tvclock_interval_min1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+(((t2_nsec/1000)-t1_usec)/1000000))/60
        end function tvclock_interval_min1

        real(kind=c_long_double) function tvclock_interval_min2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-(t1_nsec/1000))/1000000))/60
        end function tvclock_interval_min2

        real(kind=c_long_double) function tv_interval_min_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_int

        real(kind=c_long_double) function tv_interval_min_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_double

        real(kind=c_long_double) function tv_interval_min_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_float

        real(kind=c_long_double) function tv_interval_min_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_long

        real(kind=c_long_double) function tv_interval_min_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_longd

        real(kind=c_long_double) function tv_interval_min_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_int2

        real(kind=c_long_double) function tv_interval_min_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_double2

        real(kind=c_long_double) function tv_interval_min_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_float2

        real(kind=c_long_double) function tv_interval_min_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_long2

        real(kind=c_long_double) function tv_interval_min_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/60
        end function tv_interval_min_longd2

        real(kind=c_long_double) function clock_interval_min_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_int

        real(kind=c_long_double) function clock_interval_min_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_double

        real(kind=c_long_double) function clock_interval_min_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_float

        real(kind=c_long_double) function clock_interval_min_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_long

        real(kind=c_long_double) function clock_interval_min_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_longd

        real(kind=c_long_double) function clock_interval_min_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_int2

        real(kind=c_long_double) function clock_interval_min_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_double2

        real(kind=c_long_double) function clock_interval_min_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_float2

        real(kind=c_long_double) function clock_interval_min_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_long2

        real(kind=c_long_double) function clock_interval_min_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/60
        end function clock_interval_min_longd2

        !---------------------------Interval-hour--------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_hour(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour

        real(kind=c_long_double) function clock_interval_hour(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour

        real(kind=c_long_double) function tvclock_interval_hour1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+(((t2_nsec/1000)-t1_usec)/1000000))/3600
        end function tvclock_interval_hour1

        real(kind=c_long_double) function tvclock_interval_hour2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-(t1_nsec/1000))/1000000))/3600
        end function tvclock_interval_hour2

        real(kind=c_long_double) function tv_interval_hour_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_int

        real(kind=c_long_double) function tv_interval_hour_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_double

        real(kind=c_long_double) function tv_interval_hour_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_float

        real(kind=c_long_double) function tv_interval_hour_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_long

        real(kind=c_long_double) function tv_interval_hour_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_longd

        real(kind=c_long_double) function tv_interval_hour_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_int2

        real(kind=c_long_double) function tv_interval_hour_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_double2

        real(kind=c_long_double) function tv_interval_hour_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_float2

        real(kind=c_long_double) function tv_interval_hour_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_long2

        real(kind=c_long_double) function tv_interval_hour_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/3600
        end function tv_interval_hour_longd2

        real(kind=c_long_double) function clock_interval_hour_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_int

        real(kind=c_long_double) function clock_interval_hour_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_double

        real(kind=c_long_double) function clock_interval_hour_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_float

        real(kind=c_long_double) function clock_interval_hour_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_long

        real(kind=c_long_double) function clock_interval_hour_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_longd

        real(kind=c_long_double) function clock_interval_hour_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_int2

        real(kind=c_long_double) function clock_interval_hour_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_double2

        real(kind=c_long_double) function clock_interval_hour_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_float2

        real(kind=c_long_double) function clock_interval_hour_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_long2

        real(kind=c_long_double) function clock_interval_hour_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/3600
        end function clock_interval_hour_longd2
!---------------------------Interval-day--------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_day(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day

        real(kind=c_long_double) function clock_interval_day(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day

        real(kind=c_long_double) function tvclock_interval_day1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+(((t2_nsec/1000)-t1_usec)/1000000))/86400
        end function tvclock_interval_day1

        real(kind=c_long_double) function tvclock_interval_day2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-(t1_nsec/1000))/1000000))/86400
        end function tvclock_interval_day2

        real(kind=c_long_double) function tv_interval_day_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_int

        real(kind=c_long_double) function tv_interval_day_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_double

        real(kind=c_long_double) function tv_interval_day_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_float

        real(kind=c_long_double) function tv_interval_day_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_long

        real(kind=c_long_double) function tv_interval_day_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_longd

        real(kind=c_long_double) function tv_interval_day_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_int2

        real(kind=c_long_double) function tv_interval_day_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_double2

        real(kind=c_long_double) function tv_interval_day_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_float2

        real(kind=c_long_double) function tv_interval_day_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_long2

        real(kind=c_long_double) function tv_interval_day_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/86400
        end function tv_interval_day_longd2

        real(kind=c_long_double) function clock_interval_day_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_int

        real(kind=c_long_double) function clock_interval_day_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_double

        real(kind=c_long_double) function clock_interval_day_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_float

        real(kind=c_long_double) function clock_interval_day_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_long

        real(kind=c_long_double) function clock_interval_day_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_longd

        real(kind=c_long_double) function clock_interval_day_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_int2

        real(kind=c_long_double) function clock_interval_day_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_double2

        real(kind=c_long_double) function clock_interval_day_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_float2

        real(kind=c_long_double) function clock_interval_day_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_long2

        real(kind=c_long_double) function clock_interval_day_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/86400
        end function clock_interval_day_longd2
!---------------------------Interval-week--------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_week(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week

        real(kind=c_long_double) function clock_interval_week(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week

        real(kind=c_long_double) function tvclock_interval_week1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+(((t2_nsec/1000)-t1_usec)/1000000))/604800
        end function tvclock_interval_week1

        real(kind=c_long_double) function tvclock_interval_week2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-(t1_nsec/1000))/1000000))/604800
        end function tvclock_interval_week2

        real(kind=c_long_double) function tv_interval_week_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_int

        real(kind=c_long_double) function tv_interval_week_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_double

        real(kind=c_long_double) function tv_interval_week_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_float

        real(kind=c_long_double) function tv_interval_week_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_long

        real(kind=c_long_double) function tv_interval_week_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_longd

        real(kind=c_long_double) function tv_interval_week_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_int2

        real(kind=c_long_double) function tv_interval_week_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_double2

        real(kind=c_long_double) function tv_interval_week_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_float2

        real(kind=c_long_double) function tv_interval_week_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_long2

        real(kind=c_long_double) function tv_interval_week_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/604800
        end function tv_interval_week_longd2

        real(kind=c_long_double) function clock_interval_week_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_int

        real(kind=c_long_double) function clock_interval_week_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_double

        real(kind=c_long_double) function clock_interval_week_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_float

        real(kind=c_long_double) function clock_interval_week_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_long

        real(kind=c_long_double) function clock_interval_week_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_longd

        real(kind=c_long_double) function clock_interval_week_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_int2

        real(kind=c_long_double) function clock_interval_week_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_double2

        real(kind=c_long_double) function clock_interval_week_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_float2

        real(kind=c_long_double) function clock_interval_week_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_long2

        real(kind=c_long_double) function clock_interval_week_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/604800
        end function clock_interval_week_longd2
!---------------------------Interval-year--------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_year(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year

        real(kind=c_long_double) function clock_interval_year(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year

        real(kind=c_long_double) function tvclock_interval_year1(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+(((t2_nsec/1000)-t1_usec)/1000000))/31556925.19
        end function tvclock_interval_year1

        real(kind=c_long_double) function tvclock_interval_year2(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-(t1_nsec/1000))/1000000))/31556925.19
        end function tvclock_interval_year2

        real(kind=c_long_double) function tv_interval_year_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_int

        real(kind=c_long_double) function tv_interval_year_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_double

        real(kind=c_long_double) function tv_interval_year_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_float

        real(kind=c_long_double) function tv_interval_year_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_long

        real(kind=c_long_double) function tv_interval_year_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_longd

        real(kind=c_long_double) function tv_interval_year_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_int2

        real(kind=c_long_double) function tv_interval_year_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_double2

        real(kind=c_long_double) function tv_interval_year_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_float2

        real(kind=c_long_double) function tv_interval_year_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_long2

        real(kind=c_long_double) function tv_interval_year_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000))/31556925.19
        end function tv_interval_year_longd2

        real(kind=c_long_double) function clock_interval_year_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_int

        real(kind=c_long_double) function clock_interval_year_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_double

        real(kind=c_long_double) function clock_interval_year_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_float

        real(kind=c_long_double) function clock_interval_year_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_long

        real(kind=c_long_double) function clock_interval_year_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_longd

        real(kind=c_long_double) function clock_interval_year_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_int2

        real(kind=c_long_double) function clock_interval_year_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_double2

        real(kind=c_long_double) function clock_interval_year_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_float2

        real(kind=c_long_double) function clock_interval_year_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_long2

        real(kind=c_long_double) function clock_interval_year_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000))/31556925.19
        end function clock_interval_year_longd2
!------------------------------------------------------------------------------------------

        real(kind=c_long_double) function tv_interval(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval

        real(kind=c_long_double) function tv_interval_milli(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli

        real(kind=c_long_double) function tv_interval_micro(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro

        real(kind=c_long_double) function tv_interval_nano(time1,time2) result(difference)!time2-time1
            type(timeval),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano
!--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_int2

        real(kind=c_long_double) function tv_interval_milli_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_int2

        real(kind=c_long_double) function tv_interval_micro_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_int2

        real(kind=c_long_double) function tv_interval_nano_int2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_int2
!--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_double2

        real(kind=c_long_double) function tv_interval_milli_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_double2

        real(kind=c_long_double) function tv_interval_micro_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_double2

        real(kind=c_long_double) function tv_interval_nano_double2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_double2

!--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_float2

        real(kind=c_long_double) function tv_interval_milli_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_float2

        real(kind=c_long_double) function tv_interval_micro_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_float2

        real(kind=c_long_double) function tv_interval_nano_float2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_float2


        !--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_longd2

        real(kind=c_long_double) function tv_interval_milli_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_longd2

        real(kind=c_long_double) function tv_interval_micro_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_longd2

        real(kind=c_long_double) function tv_interval_nano_longd2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_longd2
!--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_long2

        real(kind=c_long_double) function tv_interval_milli_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_long2

        real(kind=c_long_double) function tv_interval_micro_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_long2

        real(kind=c_long_double) function tv_interval_nano_long2(time1,val) result(difference)!time2-time1
            type(timeval),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_long2
        !---------------------------------------------------------------------------------

        real(kind=c_long_double) function tv_interval_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_int

        real(kind=c_long_double) function tv_interval_milli_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_int

        real(kind=c_long_double) function tv_interval_micro_int(val,time2) result(difference)!time2-time1
        integer(kind=c_int),intent(in)::val
        type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_int

        real(kind=c_long_double) function tv_interval_nano_int(val,time2) result(difference)!time2-time1
        integer(kind=c_int),intent(in)::val
        type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_int
!----------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_long

        real(kind=c_long_double) function tv_interval_milli_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_long

        real(kind=c_long_double) function tv_interval_micro_long(val,time2) result(difference)!time2-time1
        integer(kind=c_long),intent(in)::val
        type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_long

        real(kind=c_long_double) function tv_interval_nano_long(val,time2) result(difference)!time2-time1
        integer(kind=c_long),intent(in)::val
        type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_long
!----------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_longd

        real(kind=c_long_double) function tv_interval_milli_longd(val,time2) result(difference)!time2-time1
        real(kind=c_long_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_longd

        real(kind=c_long_double) function tv_interval_micro_longd(val,time2) result(difference)!time2-time1
        real(kind=c_long_double),intent(in)::val
        type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_longd

        real(kind=c_long_double) function tv_interval_nano_longd(val,time2) result(difference)!time2-time1
        real(kind=c_long_double),intent(in)::val
        type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_longd
!----------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_float

        real(kind=c_long_double) function tv_interval_milli_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_float

        real(kind=c_long_double) function tv_interval_micro_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_float

        real(kind=c_long_double) function tv_interval_nano_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_float

!----------------------------------------------------------------------
        real(kind=c_long_double) function tv_interval_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=(t2_sec-t1_sec)+((t2_usec-t1_usec)/1000000)
        end function tv_interval_double

        real(kind=c_long_double) function tv_interval_milli_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000)+((t2_usec-t1_usec)/1000)
        end function tv_interval_milli_double

        real(kind=c_long_double) function tv_interval_micro_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000)+(t2_usec-t1_usec)
        end function tv_interval_micro_double

        real(kind=c_long_double) function tv_interval_nano_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timeval),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_usec,t2_sec,t2_usec
            type(timeval)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_usec=time1%tv_usec
            t2_sec=time2%tv_sec
            t2_usec=time2%tv_usec
            difference=((t2_sec-t1_sec)*1000000000)+((t2_usec-t1_usec)*1000)
        end function tv_interval_nano_double
        
!--------------------------------------------------------------------------------------

        real(kind=c_long_double) function clock_interval(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval

        real(kind=c_long_double) function clock_interval_milli(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli

        real(kind=c_long_double) function clock_interval_micro(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro

        real(kind=c_long_double) function clock_interval_nano(time1,time2) result(difference)!time2-time1
            type(timespec),intent(in)::time1,time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano
!--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function clock_interval_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_int2

        real(kind=c_long_double) function clock_interval_milli_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_int2

        real(kind=c_long_double) function clock_interval_micro_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_int2

        real(kind=c_long_double) function clock_interval_nano_int2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_int),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_int2

        !--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function clock_interval_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_long2

        real(kind=c_long_double) function clock_interval_milli_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_long2

        real(kind=c_long_double) function clock_interval_micro_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_long2

        real(kind=c_long_double) function clock_interval_nano_long2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            integer(kind=c_long),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_long2

        !--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function clock_interval_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_double2

        real(kind=c_long_double) function clock_interval_milli_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_double2

        real(kind=c_long_double) function clock_interval_micro_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_double2

        real(kind=c_long_double) function clock_interval_nano_double2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_double2

        !--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function clock_interval_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_longd2

        real(kind=c_long_double) function clock_interval_milli_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_longd2

        real(kind=c_long_double) function clock_interval_micro_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_longd2

        real(kind=c_long_double) function clock_interval_nano_longd2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_long_double),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_longd2

        !--------------------------------------------------------------------------------------------
        real(kind=c_long_double) function clock_interval_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_float2

        real(kind=c_long_double) function clock_interval_milli_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_float2

        real(kind=c_long_double) function clock_interval_micro_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_float2

        real(kind=c_long_double) function clock_interval_nano_float2(time1,val) result(difference)!time2-time1
            type(timespec),intent(in)::time1
            real(kind=c_float),intent(in)::val
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time2
            time2=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_float2
!--------------------------------------------------------------------------------------

        real(kind=c_long_double) function clock_interval_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_int

        real(kind=c_long_double) function clock_interval_milli_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_int

        real(kind=c_long_double) function clock_interval_micro_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_int

        real(kind=c_long_double) function clock_interval_nano_int(val,time2) result(difference)!time2-time1
            integer(kind=c_int),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_int


        !--------------------------------------------------------------------------------------

        real(kind=c_long_double) function clock_interval_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_float

        real(kind=c_long_double) function clock_interval_milli_float(val,time2) result(difference)!time2-time1
        real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_float

        real(kind=c_long_double) function clock_interval_micro_float(val,time2) result(difference)!time2-time1
        real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_float

        real(kind=c_long_double) function clock_interval_nano_float(val,time2) result(difference)!time2-time1
            real(kind=c_float),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_float

        !--------------------------------------------------------------------------------------

        real(kind=c_long_double) function clock_interval_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_double

        real(kind=c_long_double) function clock_interval_milli_double(val,time2) result(difference)!time2-time1
        real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_double

        real(kind=c_long_double) function clock_interval_micro_double(val,time2) result(difference)!time2-time1
        real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_double

        real(kind=c_long_double) function clock_interval_nano_double(val,time2) result(difference)!time2-time1
            real(kind=c_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_double

        !--------------------------------------------------------------------------------------

        real(kind=c_long_double) function clock_interval_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_longd

        real(kind=c_long_double) function clock_interval_milli_longd(val,time2) result(difference)!time2-time1
        real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_longd

        real(kind=c_long_double) function clock_interval_micro_longd(val,time2) result(difference)!time2-time1
        real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_longd

        real(kind=c_long_double) function clock_interval_nano_longd(val,time2) result(difference)!time2-time1
            real(kind=c_long_double),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_longd

 !--------------------------------------------------------------------------------------

        real(kind=c_long_double) function clock_interval_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=(t2_sec-t1_sec)+((t2_nsec-t1_nsec)/1000000000)
        end function clock_interval_long

        real(kind=c_long_double) function clock_interval_milli_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000)+((t2_nsec-t1_nsec)/1000000)
        end function clock_interval_milli_long

        real(kind=c_long_double) function clock_interval_micro_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000)+((t2_nsec-t1_nsec)/1000)
        end function clock_interval_micro_long

        real(kind=c_long_double) function clock_interval_nano_long(val,time2) result(difference)!time2-time1
            integer(kind=c_long),intent(in)::val
            type(timespec),intent(in)::time2
            real(kind=c_long_double)::t1_sec,t1_nsec,t2_sec,t2_nsec
            type(timespec)::time1
            time1=val
            t1_sec=time1%tv_sec
            t1_nsec=time1%tv_nsec
            t2_sec=time2%tv_sec
            t2_nsec=time2%tv_nsec
            difference=((t2_sec-t1_sec)*1000000000)+(t2_nsec-t1_nsec)
        end function clock_interval_nano_long
!----------------------------------------------------------------------------------------
        subroutine set_timeval(this,tv_sec,tv_usec)
            class(timeval)::this
            integer(kind=c_long),intent(in)::tv_sec,tv_usec
            this%tv_sec=tv_sec
            this%tv_usec=tv_usec
        end subroutine set_timeval

        subroutine set_timeval2(this,tv_sec)
            class(timeval)::this
            integer(kind=c_long),intent(in)::tv_sec
            this%tv_sec=tv_sec
        end subroutine set_timeval2

        subroutine set_timespec(this,tv_sec,tv_nsec)
            class(timespec)::this
            integer(kind=c_long),intent(in)::tv_sec,tv_nsec
            this%tv_sec=tv_sec
            this%tv_nsec=tv_nsec
        end subroutine set_timespec

        subroutine set_timespec2(this,tv_sec)
            class(timespec)::this
            integer(kind=c_long),intent(in)::tv_sec
            this%tv_sec=tv_sec
        end subroutine set_timespec2
!-------------------timespec assignment =--------------------------------
        subroutine timespec_equals_timeval(this,tm_val)
            class(timespec),intent(inout)::this
            type(timeval),intent(in)::tm_val
            this%tv_sec=tm_val%tv_sec
            this%tv_nsec=tm_val%tv_usec*1000
        end subroutine timespec_equals_timeval

        subroutine timespec_equals_cint(this,val)
            class(timespec),intent(inout)::this
            integer(kind=c_int),intent(in)::val
            this%tv_sec=val
            this%tv_nsec=0
        end subroutine timespec_equals_cint

        subroutine timespec_equals_cd(this,val)
            class(timespec),intent(inout)::this
            real(kind=c_double),intent(in)::val
            this%tv_sec=floor(val,kind=c_long)
            this%tv_nsec=int((val-floor(val))*1000000000,kind=c_long)
        end subroutine timespec_equals_cd

        subroutine timespec_equals_cf(this,val)
            class(timespec),intent(inout)::this
            real(kind=c_float),intent(in)::val
            this%tv_sec=floor(val,kind=c_long)
            this%tv_nsec=int((real(val,kind=c_long_double)-floor(val,kind=c_long))*1000000000,kind=c_long)
        end subroutine timespec_equals_cf

        subroutine timespec_equals_clongd(this,val)
            class(timespec),intent(inout)::this
            real(kind=c_long_double),intent(in)::val
            this%tv_sec=floor(val,kind=c_long)
            this%tv_nsec=int((val-floor(val))*1000000000,kind=c_long)
        end subroutine timespec_equals_clongd

        subroutine timespec_equals_clong(this,val)
            class(timespec),intent(inout)::this
            integer(kind=c_long),intent(in)::val
            this%tv_sec=val
            this%tv_nsec=0
        end subroutine timespec_equals_clong
!-----------------------------------------------------
        subroutine long_equals_timespec(this,val)
            integer(kind=c_long),intent(inout)::this
            class(timespec),intent(in)::val
            this=int(val%tv_sec+(val%tv_nsec/1000000000),kind=c_long)
        end subroutine long_equals_timespec

        subroutine int_equals_timespec(this,val)
            integer(kind=c_int),intent(inout)::this
            class(timespec),intent(in)::val
            this=int(val%tv_sec+(val%tv_nsec/1000000000),kind=c_int)
        end subroutine int_equals_timespec

        subroutine float_equals_timespec(this,val)
            real(kind=c_float),intent(inout)::this
            class(timespec),intent(in)::val
            this=real(val%tv_sec+(val%tv_nsec/1000000000),kind=c_float)
        end subroutine float_equals_timespec

        subroutine double_equals_timespec(this,val)
            real(kind=c_double),intent(inout)::this
            class(timespec),intent(in)::val
            this=real(val%tv_sec+(val%tv_nsec/1000000000),kind=c_double)
        end subroutine double_equals_timespec

        subroutine longd_equals_timespec(this,val)
            real(kind=c_long_double),intent(inout)::this
            class(timespec),intent(in)::val
            this=real(val%tv_sec+(val%tv_nsec/1000000000),kind=c_long_double)
        end subroutine longd_equals_timespec

        subroutine long_equals_timeval(this,val)
            integer(kind=c_long),intent(inout)::this
            class(timeval),intent(in)::val
            this=int(val%tv_sec+(val%tv_usec/1000000),kind=c_long)
        end subroutine long_equals_timeval

        subroutine int_equals_timeval(this,val)
            integer(kind=c_int),intent(inout)::this
            class(timeval),intent(in)::val
            this=int(val%tv_sec+(val%tv_usec/1000000),kind=c_int)
        end subroutine int_equals_timeval

        subroutine float_equals_timeval(this,val)
            real(kind=c_float),intent(inout)::this
            class(timeval),intent(in)::val
            this=real(val%tv_sec+(val%tv_usec/1000000),kind=c_float)
        end subroutine float_equals_timeval

        subroutine double_equals_timeval(this,val)
            real(kind=c_double),intent(inout)::this
            class(timeval),intent(in)::val
            this=real(val%tv_sec+(val%tv_usec/1000000),kind=c_double)
        end subroutine double_equals_timeval

        subroutine longd_equals_timeval(this,val)
            real(kind=c_long_double),intent(inout)::this
            class(timeval),intent(in)::val
            this=real(val%tv_sec+(val%tv_usec/1000000),kind=c_long_double)
        end subroutine longd_equals_timeval
!------=================time_tm=================
 !       subroutine long_equals_time_tm(this,val)
 !           integer(kind=c_long),intent(inout)::this
 !           class(time_tm),intent(in)::val
 !           this=int(mktime(val),kind=c_long)
 !       end subroutine long_equals_time_tm!

!!        subroutine int_equals_time_tm(this,val)
 !           integer(kind=c_int),intent(inout)::this
 !           class(time_tm),intent(in)::val
 !           this=int(mktime(val),kind=c_int)
 !       end subroutine int_equals_time_tm

!        subroutine float_equals_time_tm(this,val)
!            real(kind=c_float),intent(inout)::this
!            class(time_tm),intent(in)::val
!            this=real(mktime(val),kind=c_float)
!        end subroutine float_equals_time_tm

 !       subroutine double_equals_time_tm(this,val)
 !           real(kind=c_double),intent(inout)::this
 !           class(time_tm),intent(in)::val
 !           this=real(mktime(val),kind=c_double)
 !       end subroutine double_equals_time_tm

  !      subroutine longd_equals_time_tm(this,val)
  !          real(kind=c_long_double),intent(inout)::this
  !!!          class(time_tm),intent(in)::val
    !        this=real(mktime(val),kind=c_long_double)
    !    end subroutine longd_equals_time_tm

!---------------------------------------------------------------------------

        subroutine timeval_equals_timespec(this,tm_spec)
            class(timeval),intent(inout)::this
            type(timespec),intent(in)::tm_spec
            this%tv_sec=tm_spec%tv_sec
            this%tv_usec=tm_spec%tv_nsec/1000
        end subroutine timeval_equals_timespec

        subroutine timeval_equals_cint(this,val)
            class(timeval),intent(inout)::this
            integer(kind=c_int),intent(in)::val
            this%tv_sec=val
            this%tv_usec=0
        end subroutine timeval_equals_cint

        subroutine timeval_equals_cd(this,val)
            class(timeval),intent(inout)::this
            real(kind=c_double),intent(in)::val
            this%tv_sec=floor(val,kind=c_long)
            this%tv_usec=int((val-floor(val))*1000000,kind=c_long)
        end subroutine timeval_equals_cd

        subroutine timeval_equals_cf(this,val)
            class(timeval),intent(inout)::this
            real(kind=c_float),intent(in)::val
            this%tv_sec=floor(val,kind=c_long)
            this%tv_usec=int((real(val,kind=c_long_double)-floor(val,kind=c_long))*1000000,kind=c_long)
        end subroutine timeval_equals_cf

        subroutine timeval_equals_clong(this,val)
            class(timeval),intent(inout)::this
            integer(kind=c_long),intent(in)::val
            this%tv_sec=val
            this%tv_usec=0
        end subroutine timeval_equals_clong

        subroutine timeval_equals_clongd(this,val)
            class(timeval),intent(inout)::this
            real(kind=c_long_double),intent(in)::val
            this%tv_sec=floor(val,kind=c_long)
            this%tv_usec=int((val-floor(val))*1000000,kind=c_long)
        end subroutine timeval_equals_clongd

        subroutine sleep1(sec)
            integer(kind=c_int),intent(in)::sec
            integer(kind=c_int)::tmp
            tmp=sleep_(sec)
        end subroutine sleep1

        subroutine sleep2(sec,e)
            integer(kind=c_int),intent(in)::sec
            integer(kind=c_int),intent(out)::e
            e=sleep_(sec)
        end subroutine sleep2

        subroutine sleep3(s)
            integer(kind=c_long),intent(in)::s
            call nanosleep6(s,int(0,kind=c_long))
        end subroutine sleep3

        subroutine sleep4(s,e)
            integer(kind=c_long),intent(in)::s
            integer(kind=c_int),intent(out)::e
            call nanosleep5(s,int(0,kind=c_long),e)
        end subroutine sleep4

        subroutine sleepd1(s)
            real(kind=c_double),intent(in)::s
            integer(kind=c_long)::sec,nsec
            sec=floor(s,kind=c_long)
            nsec=floor((s-floor(s))*1000000000,kind=c_long)
            call nanosleep6(sec,nsec)
        end subroutine sleepd1
        
        subroutine sleepd2(s,e)
            real(kind=c_double),intent(in)::s
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::sec,nsec
            sec=floor(s,kind=c_long)
            nsec=floor((s-floor(s))*1000000000,kind=c_long)
            call nanosleep5(sec,nsec,e)
        end subroutine sleepd2

        subroutine sleepd3(s)
            real(kind=c_float),intent(in)::s
            integer(kind=c_long)::sec,nsec
            sec=floor(s,kind=c_long)
            nsec=floor((s-floor(s))*1000000000,kind=c_long)
            call nanosleep6(sec,nsec)
        end subroutine sleepd3

        subroutine sleepd4(s,e)
            real(kind=c_float),intent(in)::s
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::sec,nsec
            sec=floor(s,kind=c_long)
            nsec=floor((s-floor(s))*1000000000,kind=c_long)
            call nanosleep5(sec,nsec,e)
        end subroutine sleepd4

        subroutine sleepd5(s)
            real(kind=c_long_double),intent(in)::s
            integer(kind=c_long)::sec,nsec
            sec=floor(s,kind=c_long)
            nsec=floor((s-floor(s))*1000000000,kind=c_long)
            call nanosleep6(sec,nsec)
        end subroutine sleepd5

        subroutine sleepd6(s,e)
            real(kind=c_long_double),intent(in)::s
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::sec,nsec
            sec=floor(s,kind=c_long)
            nsec=floor((s-floor(s))*1000000000,kind=c_long)
            call nanosleep5(sec,nsec,e)
        end subroutine sleepd6

        subroutine usleep1(usec)
            integer(kind=c_int),intent(in)::usec
            integer(kind=c_int)::tmp
            tmp=usleep_(usec)
        end subroutine usleep1
        subroutine usleep2(usec,e)
            integer(kind=c_int),intent(in)::usec
            integer(kind=c_int),intent(out)::e
            e=usleep_(usec)
        end subroutine usleep2
        subroutine usleep_long1(usec,e)
            integer(kind=c_long),intent(in)::usec
            integer(kind=c_int),intent(out)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_long1
        subroutine usleep_long2(usec)
            integer(kind=c_long),intent(in)::usec
            integer(kind=c_int)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_long2
        subroutine usleep_float1(usec,e)
            real(kind=c_float),intent(in)::usec
            integer(kind=c_int),intent(out)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_float1
        subroutine usleep_float2(usec)
            real(kind=c_float),intent(in)::usec
            integer(kind=c_int)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_float2
        subroutine usleep_double1(usec,e)
            real(kind=c_double),intent(in)::usec
            integer(kind=c_int),intent(out)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_double1
        subroutine usleep_double2(usec)
            real(kind=c_double),intent(in)::usec
            integer(kind=c_int)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_double2
        subroutine usleep_longd1(usec,e)
            real(kind=c_long_double),intent(in)::usec
            integer(kind=c_int),intent(out)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_longd1
        subroutine usleep_longd2(usec)
            real(kind=c_long_double),intent(in)::usec
            integer(kind=c_int)::e
            e=usleep_(int(usec,kind=c_int))
        end subroutine usleep_longd2
        

        subroutine microsleep1(msec)
            integer(kind=c_long),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine microsleep1

        subroutine microsleep2(msec,e)
            integer(kind=c_long),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine microsleep2

        subroutine microsleep3(msec)
            integer(kind=c_int),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine microsleep3

        subroutine microsleep4(msec,e)
            integer(kind=c_int),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine microsleep4

        subroutine microsleepd1(msec)
            real(kind=c_double),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine microsleepd1
        
        subroutine microsleepd2(msec,e)
            real(kind=c_double),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine microsleepd2

        subroutine microsleepd3(msec)
            real(kind=c_float),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine microsleepd3

        subroutine microsleepd4(msec,e)
            real(kind=c_float),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine microsleepd4

        subroutine microsleepd5(msec)
            real(kind=c_long_double),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine microsleepd5

        subroutine microsleepd6(msec,e)
            real(kind=c_long_double),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine microsleepd6

        subroutine millisleep1(msec)
            integer(kind=c_long),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine millisleep1

        subroutine millisleep2(msec,e)
            integer(kind=c_long),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine millisleep2

        subroutine millisleep3(msec)
            integer(kind=c_int),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine millisleep3

        subroutine millisleep4(msec,e)
            integer(kind=c_int),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine millisleep4

        subroutine millisleepd1(msec)
            real(kind=c_double),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine millisleepd1
        
        subroutine millisleepd2(msec,e)
            real(kind=c_double),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine millisleepd2

        subroutine millisleepd3(msec)
            real(kind=c_float),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine millisleepd3

        subroutine millisleepd4(msec,e)
            real(kind=c_float),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine millisleepd4

        subroutine millisleepd5(msec)
            real(kind=c_long_double),intent(in)::msec
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep6(seconds,nanoseconds)
        end subroutine millisleepd5

        subroutine millisleepd6(msec,e)
            real(kind=c_long_double),intent(in)::msec
            integer(kind=c_int),intent(out)::e
            integer(kind=c_long)::seconds,nanoseconds
            real(kind=c_long_double)::msec2
            msec2=msec/1000
            seconds=floor(msec2,kind=c_long)
            nanoseconds=floor((msec2-floor(msec2))*1000000000,kind=c_long)
            call nanosleep5(seconds,nanoseconds,e)
        end subroutine millisleepd6

        subroutine nanosleep1(req,rem,e)
            type(timespec),intent(in),target::req,rem
            integer(kind=c_int),intent(out)::e
            e=nanosleep_(c_loc(req),c_loc(rem))
        end subroutine nanosleep1
        subroutine nanosleep2(req,rem)
            type(timespec),intent(in),target::req,rem
            integer(kind=c_int)::e
            e=nanosleep_(c_loc(req),c_loc(rem))
        end subroutine nanosleep2
        subroutine nanosleep3(req)
            use iso_c_binding,only:c_null_ptr
            type(timespec),intent(in),target::req
            integer(kind=c_int)::e
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep3
        subroutine nanosleep4(req,e)
            use iso_c_binding,only:c_null_ptr
            type(timespec),intent(in),target::req
            integer(kind=c_int),intent(out)::e
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep4

        subroutine nanosleep5(sec,nano,e)
            use iso_c_binding,only:c_null_ptr
            integer(kind=c_long),intent(in)::sec,nano
            integer(kind=c_int),intent(out)::e
            type(timespec),target::req
            req%tv_sec=sec
            req%tv_nsec=nano
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep5
        subroutine nanosleep6(sec,nano)
            use iso_c_binding,only:c_null_ptr
            integer(kind=c_long),intent(in)::sec,nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=sec
            req%tv_nsec=nano
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep6
        subroutine nanosleep7(nano)
            use iso_c_binding,only:c_null_ptr
            integer(kind=c_long),intent(in)::nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=0
            req%tv_nsec=nano
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep7

        subroutine nanosleep8(sec,nano,e)
            use iso_c_binding,only:c_null_ptr
            integer(kind=c_int),intent(in)::sec,nano
            integer(kind=c_int),intent(out)::e
            type(timespec),target::req
            req%tv_sec=sec
            req%tv_nsec=nano
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep8
        subroutine nanosleep9(sec,nano)
            use iso_c_binding,only:c_null_ptr
            integer(kind=c_int),intent(in)::sec,nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=sec
            req%tv_nsec=nano
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep9
        subroutine nanosleep10(nano)
            use iso_c_binding,only:c_null_ptr
            integer(kind=c_int),intent(in)::nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=0
            req%tv_nsec=nano
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep10

        subroutine nanosleep11(nano)
            use iso_c_binding,only:c_null_ptr
            real(kind=c_float),intent(in)::nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=0
            req%tv_nsec=int(nano,kind=c_long)
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep11

        subroutine nanosleep12(nano)
            use iso_c_binding,only:c_null_ptr
            real(kind=c_double),intent(in)::nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=0
            req%tv_nsec=int(nano,kind=c_long)
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep12

        subroutine nanosleep13(nano)
            use iso_c_binding,only:c_null_ptr
            real(kind=c_long_double),intent(in)::nano
            integer(kind=c_int)::e
            type(timespec),target::req
            req%tv_sec=0
            req%tv_nsec=int(nano,kind=c_long)
            e=nanosleep_(c_loc(req),c_null_ptr)
        end subroutine nanosleep13

        subroutine gettimeofday1(tv,tz,e)
            type(timeval),intent(inout),target::tv
            type(timezone),intent(inout),target::tz
            integer(kind=c_int),intent(out)::e
            e=gettimeofday_(c_loc(tv),c_loc(tz))
        end subroutine gettimeofday1

        subroutine gettimeofday2(tv,tz)
            type(timeval),intent(inout),target::tv
            type(timezone),intent(inout),target::tz
            integer(kind=c_int)::e
            e=gettimeofday_(c_loc(tv),c_loc(tz))
        end subroutine gettimeofday2

        subroutine gettimeofday3(tv)
            type(timeval),intent(inout),target::tv
            integer(kind=c_int)::e
            e=gettimeofday_(c_loc(tv),c_null_ptr)
        end subroutine gettimeofday3

        subroutine gettimeofday4(tv,e)
            type(timeval),intent(inout),target::tv
            integer(kind=c_int),intent(out)::e
            e=gettimeofday_(c_loc(tv),c_null_ptr)
        end subroutine gettimeofday4

        function gettimeofday5(tz,e) result(tv)
            type(timeval),target::tv
            type(timezone),intent(inout),target::tz
            integer(kind=c_int),intent(out)::e
            e=gettimeofday_(c_loc(tv),c_loc(tz))
        end function gettimeofday5

        function gettimeofday6(e) result(tv)
            type(timeval),target::tv
            integer(kind=c_int),intent(out)::e
            e=gettimeofday_(c_loc(tv),c_null_ptr)
        end function gettimeofday6

        function gettimeofday7() result(tv)
            type(timeval),target::tv
            integer(kind=c_int)::e
            e=gettimeofday_(c_loc(tv),c_null_ptr)
        end function gettimeofday7

        subroutine settimeofday1(tv,tz,e)
            type(timeval),intent(inout),target::tv
            type(timezone),intent(inout),target::tz
            integer(kind=c_int),intent(out)::e
            e=settimeofday_(c_loc(tv),c_loc(tz))
        end subroutine settimeofday1

        subroutine settimeofday2(tv,tz)
            type(timeval),intent(inout),target::tv
            type(timezone),intent(inout),target::tz
            integer(kind=c_int)::e
            e=settimeofday_(c_loc(tv),c_loc(tz))
        end subroutine settimeofday2

        subroutine clock_getres1(clk_id,tp,e)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),intent(inout),target::tp
            integer(kind=c_int),intent(out)::e
            e=clock_getres_(clk_id,c_loc(tp))
        end subroutine clock_getres1
        subroutine clock_getres2(clk_id,tp)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),intent(inout),target::tp
            integer(kind=c_int)::e
            e=clock_getres_(clk_id,c_loc(tp))
        end subroutine clock_getres2
        subroutine clock_getres3(tp)
            type(timespec),intent(inout),target::tp
            integer(kind=c_int)::e
            e=clock_getres_(clock_realtime,c_loc(tp))
        end subroutine clock_getres3

        function clock_getres4(clk_id,e) result(tp)
            integer(kind=c_int),intent(in)::clk_id
            integer(kind=c_int),intent(out)::e
            type(timespec),target::tp
            e=clock_getres_(clk_id,c_loc(tp))
        end function clock_getres4
        function clock_getres5(clk_id) result(tp)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),target::tp
            integer(kind=c_int)::e
            e=clock_getres_(clk_id,c_loc(tp))
        end function clock_getres5
        function clock_getres6() result(tp)
            type(timespec),target::tp
            integer(kind=c_int)::e
            e=clock_getres_(clock_realtime,c_loc(tp))
        end function clock_getres6

        subroutine clock_settime1(clk_id,tp,e)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),intent(inout),target::tp
            integer(kind=c_int),intent(out)::e
            e=clock_settime_(clk_id,c_loc(tp))
        end subroutine clock_settime1
        subroutine clock_settime2(clk_id,tp)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),intent(inout),target::tp
            integer(kind=c_int)::e
            e=clock_settime_(clk_id,c_loc(tp))
        end subroutine clock_settime2
        subroutine clock_settime3(tp)
            type(timespec),intent(inout),target::tp
            integer(kind=c_int)::e
            e=clock_settime_(clock_realtime,c_loc(tp))
        end subroutine clock_settime3

        subroutine clock_gettime1(clk_id,tp,e)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),intent(inout),target::tp
            integer(kind=c_int),intent(out)::e
            e=clock_gettime_(clk_id,c_loc(tp))
        end subroutine clock_gettime1
        subroutine clock_gettime2(clk_id,tp)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),intent(inout),target::tp
            integer(kind=c_int)::e
            e=clock_gettime_(clk_id,c_loc(tp))
        end subroutine clock_gettime2
        subroutine clock_gettime3(tp)
            type(timespec),intent(inout),target::tp
            integer(kind=c_int)::e
            e=clock_gettime_(clock_realtime,c_loc(tp))
        end subroutine clock_gettime3

        function clock_gettime4(clk_id,e) result(tp)
            integer(kind=c_int),intent(in)::clk_id
            integer(kind=c_int),intent(out)::e
            type(timespec),target::tp
            e=clock_gettime_(clk_id,c_loc(tp))
        end function clock_gettime4
        function clock_gettime5(clk_id) result(tp)
            integer(kind=c_int),intent(in)::clk_id
            type(timespec),target::tp
            integer(kind=c_int)::e
            e=clock_gettime_(clk_id,c_loc(tp))
        end function clock_gettime5
        function clock_gettime6() result(tp)
            type(timespec),target::tp
            integer(kind=c_int)::e
            e=clock_gettime_(clock_realtime,c_loc(tp))
        end function clock_gettime6

        integer(kind=c_long) function timec1(raw_t) result(t) !time() in C
            integer(kind=c_long),intent(inout),target::raw_t
            t=timec_(c_loc(raw_t))
        end function timec1

        integer(kind=c_long) function timec2() result(t) !time() in C
            t=timec_(c_null_ptr)
        end function timec2

        function gmtimec1(rawtime) result(t)
            integer(kind=c_long),intent(in),target::rawtime
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            tmp=c_gmtime_(c_loc(rawtime))
            call c_f_pointer(tmp,t)
         !   nullify(tmp2)
        end function gmtimec1
        function gmtimec2() result(t)
            integer(kind=c_long),target::rawtime
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            rawtime=timec()
            tmp=c_gmtime_(c_loc(rawtime))
            call c_f_pointer(tmp,t)
        end function gmtimec2

        function gmtimec3(val) result(t)
            type(timespec),intent(in),target::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            tmp=c_localtime_(c_loc(val%tv_sec))
            call c_f_pointer(tmp,t)
        end function gmtimec3

        function gmtimec4(val) result(t)
            type(timeval),intent(in),target::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            tmp=c_localtime_(c_loc(val%tv_sec))
            call c_f_pointer(tmp,t)
        end function gmtimec4

        function gmtimec5(val) result(t)
            integer(kind=c_int),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=val
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function gmtimec5

        function gmtimec6(val) result(t)
            real(kind=c_float),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=int(val,kind=c_long)
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function gmtimec6

        function gmtimec7(val) result(t)
            real(kind=c_double),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=int(val,kind=c_long)
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function gmtimec7

        function gmtimec8(val) result(t)
            real(kind=c_long_double),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=int(val,kind=c_long)
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function gmtimec8

        function localtime1(rawtime) result(t)
            integer(kind=c_long),intent(in),target::rawtime
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            tmp=c_localtime_(c_loc(rawtime))
            call c_f_pointer(tmp,t)
          !  nullify(tmp2)
        end function localtime1
        function localtime2() result(t)
            integer(kind=c_long),target::rawtime
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            rawtime=timec()
            tmp=c_localtime_(c_loc(rawtime))
            call c_f_pointer(tmp,t)
            !nullify(tmp2)
        end function localtime2
        function localtime3(val) result(t)
            type(timespec),intent(in),target::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            tmp=c_localtime_(c_loc(val%tv_sec))
            call c_f_pointer(tmp,t)
        end function localtime3

        function localtime4(val) result(t)
            type(timeval),intent(in),target::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            tmp=c_localtime_(c_loc(val%tv_sec))
            call c_f_pointer(tmp,t)
        end function localtime4

        function localtime5(val) result(t)
            integer(kind=c_int),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=val
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function localtime5

        function localtime6(val) result(t)
            real(kind=c_float),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=int(val,kind=c_long)
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function localtime6

        function localtime7(val) result(t)
            real(kind=c_double),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=int(val,kind=c_long)
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function localtime7

        function localtime8(val) result(t)
            real(kind=c_long_double),intent(in)::val
            type(time_tm),pointer::t
            type(c_ptr)::tmp
            integer(kind=c_long),target::x
            x=int(val,kind=c_long)
            tmp=c_localtime_(c_loc(x))
            call c_f_pointer(tmp,t)
        end function localtime8
        
        function strftime1(format_,tm,buffersize) result(e)
            use iso_c_binding,only:c_char,c_null_char
            character(len=*),intent(in)::format_
            type(time_tm),intent(in),optional,target::tm
            integer(kind=c_long),intent(in),optional,target::buffersize
            character(len=:),allocatable::e,f
            integer(kind=c_int)::cx,dx=0,gx=0
            integer(kind=c_size_t)::zx,zx2

            character(kind=c_char,len=1),allocatable,target::tmp(:),tmp2(:)
            f=format_
            if(.not.present(buffersize)) then
            do while(index(f,"%")>0)
                gx=gx+2
                cx=index(f,"%")
                select case(f(cx+1:cx+1))
                case('a') 
                    dx=dx+5
                case('A')
                    dx=dx+12
                case('b')
                    dx=dx+5
                case('B')
                    dx=dx+12
                case('c')
                    dx=dx+30
                case('C')
                    dx=dx+4
                case('d')
                    dx=dx+4
                case('D')
                    dx=dx+12
                case('e')
                    dx=dx+4
                case('F')
                    dx=dx+14
                case('g')
                    dx=dx+4
                case('G')
                    dx=dx+6
                case('h')
                    dx=dx+5
                case('H')
                    dx=dx+4
                case('I')
                    dx=dx+4
                case('j')
                    dx=dx+5
                case('m')
                    dx=dx+4
                case('M')
                    dx=dx+4
                case('n')
                    dx=dx+4
                case('p')
                    dx=dx+4
                case('r')
                    dx=dx+14
                case('R')
                    dx=dx+7
                case('S')
                    dx=dx+4
                case('t')
                    dx=dx+4
                case('T')
                    dx=dx+14
                case('u')
                    dx=dx+3
                case('U')
                    dx=dx+4
                case('V')
                    dx=dx+4
                case('w')
                    dx=dx+3
                case('W')
                    dx=dx+4
                case('x')
                    dx=dx+12
                case('X')
                    dx=dx+12
                case('y')
                    dx=dx+4
                case('Y')
                    dx=dx+7
                case('z')
                    dx=dx+8
                case('Z')
                    dx=dx+7
                case('%')
                    dx=dx+2
                case default
                    dx=dx+10
                end select
                f=f(cx+2:len(f))
            end do
            zx=len(trim(adjustl(format_)))-gx+dx+5
            else
                zx=buffersize
            end if
            allocate(tmp2(zx))
            call fstring_to_cstring(format_,tmp)
            if(present(tm)) then
                zx2= strftime_(c_loc(tmp2),zx,c_loc(tmp),c_loc(tm))
            else
                zx2= strftime_(c_loc(tmp2),zx,c_loc(tmp),c_loc(localtime()))
            end if
            call cstring_to_fstring(tmp2,e,clen_size=zx2)
        end function strftime1

        function strftime_timespec(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            type(timespec),intent(in)::val
            integer(kind=c_long),intent(in),optional,target::buffersize
            character(len=:),allocatable::e
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_timespec

        function strftime_timeval(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            type(timeval),intent(in)::val
            character(len=:),allocatable::e
            integer(kind=c_long),intent(in),optional,target::buffersize
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_timeval

        function strftime_long(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            integer(kind=c_long),intent(in)::val
            character(len=:),allocatable::e
            integer(kind=c_long),intent(in),optional,target::buffersize
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_long

        function strftime_int(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            integer(kind=c_int),intent(in)::val
            character(len=:),allocatable::e
            integer(kind=c_long),intent(in),optional,target::buffersize
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_int

        function strftime_float(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            real(kind=c_float),intent(in)::val
            character(len=:),allocatable::e
            integer(kind=c_long),intent(in),optional,target::buffersize
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_float

        function strftime_double(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            real(kind=c_double),intent(in)::val
            character(len=:),allocatable::e
            integer(kind=c_long),intent(in),optional,target::buffersize
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_double
        function strftime_longd(format_,val,buffersize) result(e)
            character(len=*),intent(in)::format_
            real(kind=c_long_double),intent(in)::val
            character(len=:),allocatable::e
            integer(kind=c_long),intent(in),optional,target::buffersize
            if(present(buffersize)) then
                e=strftime1(format_,localtime(val),buffersize)
            else
                e=strftime1(format_,localtime(val))
            end if
        end function strftime_longd

        real(kind=c_double) function difftime(end,beginning) result(dtime)
            integer(kind=c_long),intent(in)::end,beginning
            dtime=difftime_(end,beginning)
        end function difftime

        function asctime1(tm) result(e)
            use iso_c_binding,only:c_char
            type(time_tm),intent(in),target::tm
            character(len=:),allocatable::e
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime1

        function asctime2() result(e)
            use iso_c_binding,only:c_char
            character(len=:),allocatable::e
            e=cptr_to_fstring(asctime_(c_loc(localtime())),80,mode=0)
        end function asctime2

        function asctime3(val) result(e)
            use iso_c_binding,only:c_char
            integer(kind=c_int),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=val
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime3

        function asctime4(val) result(e)
            use iso_c_binding,only:c_char
            integer(kind=c_long),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=val
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime4

        function asctime5(val) result(e)
            use iso_c_binding,only:c_char
            real(kind=c_float),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=val
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime5

        function asctime6(val) result(e)
            use iso_c_binding,only:c_char
            real(kind=c_double),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=val
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime6

        function asctime7(val) result(e)
            use iso_c_binding,only:c_char
            real(kind=c_long_double),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=val
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime7

        function asctime8(val) result(e)
            use iso_c_binding,only:c_char
            type(timespec),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=int(val%tv_sec+(val%tv_nsec/1000000000),kind=c_long)
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime8

        function asctime9(val) result(e)
            use iso_c_binding,only:c_char
            type(timeval),intent(in)::val
            character(len=:),allocatable::e
            type(time_tm),target::tm
            tm=int(val%tv_sec+(val%tv_usec/1000000),kind=c_long)
            e=cptr_to_fstring(asctime_(c_loc(tm)),80,mode=0)
        end function asctime9

        function c_time1(timer) result(e) !ctime() in C
            integer(kind=c_long),intent(in),target::timer
            character(len=:),allocatable::e
            e=cptr_to_fstring(ctime_(c_loc(timer)),100,mode=0)
        end function c_time1
        function c_time2(timer) result(e) !ctime() in C
            integer(kind=c_int),intent(in)::timer
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=timer
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time2
        function c_time3(timer) result(e) !ctime() in C
            real(kind=c_float),intent(in)::timer
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=int(timer,kind=c_long)
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time3
        function c_time4(timer) result(e) !ctime() in C
            real(kind=c_double),intent(in)::timer
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=int(timer,kind=c_long)
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time4
        function c_time5(timer) result(e) !ctime() in C
            real(kind=c_long_double),intent(in)::timer
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=int(timer,kind=c_long)
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time5
        function c_time6(timer) result(e) !ctime() in C
            type(timespec),intent(in)::timer
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=timer%tv_sec+int(timer%tv_nsec/1000000000,kind=c_long)
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time6
        function c_time7(timer) result(e) !ctime() in C
            type(timeval),intent(in)::timer
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=timer%tv_sec+int(timer%tv_usec/1000000,kind=c_long)
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time7

        function c_time8() result(e) !ctime() in C
            character(len=:),allocatable::e
            integer(kind=c_long),target::tmp
            tmp=timec()
            e=cptr_to_fstring(ctime_(c_loc(tmp)),100,mode=0)
        end function c_time8

        integer(kind=c_long) function mktime1(tm) result(t)
            type(time_tm),intent(in),target::tm
            t=mktime_(c_loc(tm))
        end function mktime1

        integer(kind=c_long) function mktime2() result(t)
            t=mktime_(c_loc(localtime()))
        end function mktime2

        integer(kind=c_long) function clockc() result(t)
            t=clock_()
        end function clockc

        function cptr_to_fstring(cptr,size_,free,mode) result(w)
            use iso_c_binding,only:c_int,c_ptr,c_char,c_f_pointer
            type(c_ptr),intent(in)::cptr
            integer(kind=c_int),intent(in),optional::size_,mode
            logical,intent(in),optional::free
            character(kind=c_char,len=1),pointer::ch(:)
            character(len=:),allocatable::w
            if(present(size_)) then
                call c_f_pointer(cptr, ch,[size_])
                if(present(mode)) then
                    if(mode==0) then
                        call cstring_to_fstring(ch,w) 
                    else if(mode==1) then
                        call cstring_to_fstring(ch,w,size_)
                    else
                        call cstring_to_fstring(ch,w,size_)
                    end if
                else
                    call cstring_to_fstring(ch,w,size_) 
                end if
            else
                if(present(mode)) then
                    if(mode==0) then
                        call c_f_pointer(cptr, ch,[50])
                    else if(mode==1) then
                        call c_f_pointer(cptr, ch,[100])
                    else if(mode==2) then
                        call c_f_pointer(cptr, ch,[200])
                    else if(mode==3) then
                        call c_f_pointer(cptr, ch,[350])
                    else if(mode==4) then
                        call c_f_pointer(cptr, ch,[600])
                    else if(mode==5) then
                        call c_f_pointer(cptr, ch,[1000])
                    else if(mode==6) then
                        call c_f_pointer(cptr, ch,[2000])
                    else
                        call c_f_pointer(cptr, ch,[80])
                    end if
                else
                    call c_f_pointer(cptr, ch,[350])
                end if
                call cstring_to_fstring(ch,w)
            end if
            if(present(free) .and. free .eqv. .true.) then
                call C_free(cptr)
            end if 
        end function cptr_to_fstring

        subroutine fstring_to_cstring(xs,mk)
            use iso_c_binding,only:c_char,c_null_char
            character(len=*),intent(in)::xs
            character(len=1,kind=c_char),allocatable,intent(inout)::mk(:)
            character(len=:),allocatable::tmp
            integer::i
            tmp=trim(adjustl(xs))//c_null_char
            allocate(mk(len(tmp)));
            do i=1,len(tmp)
                mk(i)=tmp(i:i)
            end do
        end subroutine fstring_to_cstring

        subroutine cstring_to_fstring(cstr,fstr,clen,clen_long,clen_size)
            use iso_c_binding,only:c_char,c_null_char,c_int,c_size_t
            character(len=1,kind=c_char),intent(in)::cstr(*)
            character(len=:),allocatable,intent(inout)::fstr
            integer(kind=c_int),intent(in),optional::clen
            integer(kind=c_long),intent(in),optional::clen_long
            integer(kind=c_size_t),intent(in),optional::clen_size
            integer::i=1,i2
            integer(kind=c_size_t)::i_size
            integer(kind=c_long)::i_long
            if(present(clen_size)) then
                allocate(character(len=clen_size)::fstr)
                do i_size=1,clen_size
                    fstr(i_size:i_size)=cstr(i_size)
                end do
            else if(present(clen_long)) then
                allocate(character(len=clen_long)::fstr)
                do i_long=1,clen_long
                    fstr(i_long:i_long)=cstr(i_long)
                end do
            else if(present(clen)) then
                allocate(character(len=clen)::fstr)
                do i=1,clen
                    fstr(i:i)=cstr(i)
                end do
            else
                do while(cstr(i)/=c_null_char)
                    i=i+1
                end do
                i=i-1
                allocate(character(len=i)::fstr)
                do i2=1,i
                    fstr(i2:i2)=cstr(i2)
                end do
            end if
        end subroutine cstring_to_fstring

!----------------------------------------------------------------------------------------

        function timespec_add_timeval(this,other) result(z)
            class(timespec),intent(in)::this
            type(timeval),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))+(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_add_timeval

        function timespec_sub_timeval(this,other) result(z)
            class(timespec),intent(in)::this
            type(timeval),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))-(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_sub_timeval

        function timespec_multi_timeval(this,other) result(z)
            class(timespec),intent(in)::this
            type(timeval),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))*(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_multi_timeval

        function timespec_div_timeval(this,other) result(z)
            class(timespec),intent(in)::this
            type(timeval),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_timeval

        function timespec_expo_timeval(this,other) result(z)
            class(timespec),intent(in)::this
            type(timeval),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_timeval
    
    !----------------------------------------------------------------------------

        function timespec_add_timespec(this,other) result(z)
            class(timespec),intent(in)::this,other
            type(timespec)::z
            real(kind=c_long_double)::t1
            
            t1=this%tv_nsec+other%tv_nsec
            if(t1>=1000000000) then
                t1=t1/1000000000
                z%tv_sec=this%tv_sec+other%tv_sec+floor(t1,kind=c_long)
                z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
            else
                z%tv_sec=this%tv_sec+other%tv_sec
                z%tv_nsec=this%tv_nsec+other%tv_nsec
            end if
        end function timespec_add_timespec

        function timespec_add_clongd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_add_clongd

        function timespec_sub_timespec(this,other) result(z)
            class(timespec),intent(in)::this,other
            type(timespec)::z
            real(kind=c_long_double)::t1
            
            t1=this%tv_nsec-other%tv_nsec
            if(t1<0) then
                t1=this%tv_sec-other%tv_sec+(t1/1000000000)
                z%tv_sec=floor(t1,kind=c_long)
                z%tv_nsec=floor((t1-floor(t1))*1000000000,kind=c_long)
            else
                z%tv_sec=this%tv_sec-other%tv_sec
                z%tv_nsec=this%tv_nsec-other%tv_nsec
            end if
        end function timespec_sub_timespec

        function timespec_sub_clongd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_sub_clongd

        function timespec_multi_timespec(this,other) result(z)
            class(timespec),intent(in)::this,other
            type(timespec)::z
            real(kind=c_long_double)::t1
            
            t1=this%tv_nsec*other%tv_nsec
            if(t1>=1000000000) then
                t1=t1/1000000000
                z%tv_sec=(this%tv_sec*other%tv_sec)+floor(t1,kind=c_long)
                z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
            else
                z%tv_sec=this%tv_sec*other%tv_sec
                z%tv_nsec=this%tv_nsec*other%tv_nsec
            end if
        end function timespec_multi_timespec

        function timespec_multi_clongd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_multi_clongd

        function timespec_div_timespec(this,other) result(z)
            class(timespec),intent(in)::this,other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_timespec

        function timespec_div_clongd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_clongd

        function timespec_expo_timespec(this,other) result(z)
            class(timespec),intent(in)::this,other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_timespec

        function timespec_expo_clongd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_clongd

!----------------------------------------------------------------------------------------
        function timespec_add_cf(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))+real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_add_cf

        function timespec_sub_cf(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))-real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_sub_cf

        function timespec_multi_cf(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))*real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_multi_cf

        function timespec_div_cf(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_cf

        function timespec_expo_cf(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_cf

        !---------------------------------------------------------------------------

        function timespec_add_cd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_add_cd

        function timespec_sub_cd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_sub_cd

        function timespec_multi_cd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_multi_cd

        function timespec_div_cd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_cd

        function timespec_expo_cd(this,other) result(z)
            class(timespec),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_cd
        !------------------------
        function timespec_add_clong(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_add_clong

        function timespec_sub_clong(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_sub_clong


        function timespec_multi_clong(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_multi_clong


        function timespec_div_clong(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_clong


        function timespec_expo_clong(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_clong
!-------------------------------------------------------------------------
        function timespec_add_cint(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_add_cint

        function timespec_sub_cint(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_sub_cint


        function timespec_multi_cint(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_multi_cint


        function timespec_div_cint(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_div_cint


        function timespec_expo_cint(this,other) result(z)
            class(timespec),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_nsec/1000000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function timespec_expo_cint
    !------------------------------------------------------------------------------
        function cint_add_timespec(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cint_add_timespec

        function cint_sub_timespec(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cint_sub_timespec


        function cint_multi_timespec(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cint_multi_timespec

        function cint_div_timespec(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cint_div_timespec

        function cint_expo_timespec(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cint_expo_timespec

!------------------------------------------------------------------------------
        function clong_add_timespec(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clong_add_timespec

        function clong_sub_timespec(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clong_sub_timespec


        function clong_multi_timespec(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clong_multi_timespec

        function clong_div_timespec(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clong_div_timespec

        function clong_expo_timespec(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clong_expo_timespec
     
    !------------------------------------------------------------------------------
        function clongd_add_timespec(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clongd_add_timespec

        function clongd_sub_timespec(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clongd_sub_timespec

        function clongd_multi_timespec(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clongd_multi_timespec

        function clongd_div_timespec(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clongd_div_timespec

        function clongd_expo_timespec(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function clongd_expo_timespec

        !------------------------------------------------------------------------------
        function cd_add_timespec(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cd_add_timespec

        function cd_sub_timespec(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cd_sub_timespec

        function cd_multi_timespec(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cd_multi_timespec

        function cd_div_timespec(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cd_div_timespec

        function cd_expo_timespec(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timespec),intent(in)::other
            type(timespec)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_nsec/1000000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_nsec=int((t1-floor(t1))*1000000000,kind=c_long)
        end function cd_expo_timespec


        !----------------------------------------------------------------------------
!----------------------------------------------------------------------------

        function timeval_add_timeval(this,other) result(z)
            class(timeval),intent(in)::this,other
            type(timeval)::z
            real(kind=c_long_double)::t1
            
            t1=this%tv_usec+other%tv_usec
            if(t1>=1000000) then
                t1=t1/1000000
                z%tv_sec=this%tv_sec+other%tv_sec+floor(t1,kind=c_long)
                z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
            else
                z%tv_sec=this%tv_sec+other%tv_sec
                z%tv_usec=this%tv_usec+other%tv_usec
            end if
        end function timeval_add_timeval

        function timeval_sub_timeval(this,other) result(z)
            class(timeval),intent(in)::this,other
            type(timeval)::z
            real(kind=c_long_double)::t1
            
            t1=this%tv_usec-other%tv_usec
            if(t1<0) then
                t1=this%tv_sec-other%tv_sec+(t1/1000000)
                z%tv_sec=floor(t1,kind=c_long)
                z%tv_usec=floor((t1-floor(t1))*1000000,kind=c_long)
            else
                z%tv_sec=this%tv_sec-other%tv_sec
                z%tv_usec=this%tv_usec-other%tv_usec
            end if
        end function timeval_sub_timeval

        function timeval_multi_timeval(this,other) result(z)
            class(timeval),intent(in)::this,other
            type(timeval)::z
            real(kind=c_long_double)::t1
            
            t1=this%tv_usec*other%tv_usec
            if(t1>=1000000) then
                t1=t1/1000000
                z%tv_sec=(this%tv_sec*other%tv_sec)+floor(t1,kind=c_long)
                z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
            else
                z%tv_sec=this%tv_sec*other%tv_sec
                z%tv_usec=this%tv_usec*other%tv_usec
            end if
        end function timeval_multi_timeval

        function timeval_div_timeval(this,other) result(z)
            class(timeval),intent(in)::this,other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_timeval

        function timeval_expo_timeval(this,other) result(z)
            class(timeval),intent(in)::this,other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_timeval
    !-----------------------------------------------------------------------------

        function timeval_add_timespec(this,other) result(z)
            class(timeval),intent(in)::this
            type(timespec),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))+(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_add_timespec

        function timeval_sub_timespec(this,other) result(z)
            class(timeval),intent(in)::this
            type(timespec),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))-(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_sub_timespec

        function timeval_multi_timespec(this,other) result(z)
            class(timeval),intent(in)::this
            type(timespec),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))*(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_multi_timespec

        function timeval_div_timespec(this,other) result(z)
            class(timeval),intent(in)::this
            type(timespec),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_timespec

        function timeval_expo_timespec(this,other) result(z)
            class(timeval),intent(in)::this
            type(timespec),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**(other%tv_sec+(other%tv_nsec/1000000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_timespec

!------------------------------------------------------------------------------------------

        function timeval_add_clongd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_add_clongd

        function timeval_sub_clongd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_sub_clongd

        function timeval_multi_clongd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_multi_clongd

        function timeval_div_clongd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_clongd

        function timeval_expo_clongd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_clongd
!====================================================================================
       
!---------------------------------------------------------------------------------
        function timeval_add_cf(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))+real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_add_cf

        function timeval_sub_cf(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))-real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_sub_cf

        function timeval_multi_cf(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))*real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_multi_cf

        function timeval_div_cf(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_cf

        function timeval_expo_cf(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**real(other,kind=c_long_double)
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_cf
        !---------------------------------------------------------------------------

        function timeval_add_cd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_add_cd

        function timeval_sub_cd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_sub_cd

        function timeval_multi_cd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_multi_cd

        function timeval_div_cd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_cd

        function timeval_expo_cd(this,other) result(z)
            class(timeval),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_cd
        !------------------------
        function timeval_add_clong(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_add_clong

        function timeval_sub_clong(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_sub_clong


        function timeval_multi_clong(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_multi_clong


        function timeval_div_clong(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_clong


        function timeval_expo_clong(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_clong
!-------------------------------------------------------------------------
        function timeval_add_cint(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))+other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_add_cint

        function timeval_sub_cint(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))-other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_sub_cint


        function timeval_multi_cint(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))*other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_multi_cint


        function timeval_div_cint(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))/other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_div_cint


        function timeval_expo_cint(this,other) result(z)
            class(timeval),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(this%tv_sec+(this%tv_usec/1000000))**other
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function timeval_expo_cint
    !------------------------------------------------------------------------------
        function cint_add_timeval(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cint_add_timeval

        function cint_sub_timeval(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cint_sub_timeval


        function cint_multi_timeval(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cint_multi_timeval

        function cint_div_timeval(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cint_div_timeval

        function cint_expo_timeval(this,other) result(z)
            integer(kind=c_int),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cint_expo_timeval

!------------------------------------------------------------------------------
        function clong_add_timeval(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clong_add_timeval

        function clong_sub_timeval(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clong_sub_timeval


        function clong_multi_timeval(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clong_multi_timeval

        function clong_div_timeval(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clong_div_timeval

        function clong_expo_timeval(this,other) result(z)
            integer(kind=c_long),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clong_expo_timeval
     
    !------------------------------------------------------------------------------
        function clongd_add_timeval(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clongd_add_timeval

        function clongd_sub_timeval(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clongd_sub_timeval

        function clongd_multi_timeval(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clongd_multi_timeval

        function clongd_div_timeval(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clongd_div_timeval

        function clongd_expo_timeval(this,other) result(z)
            real(kind=c_long_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function clongd_expo_timeval

        !------------------------------------------------------------------------------
        function cd_add_timeval(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))+this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cd_add_timeval

        function cd_sub_timeval(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=this-(other%tv_sec+(other%tv_usec/1000000))
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cd_sub_timeval

        function cd_multi_timeval(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))*this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cd_multi_timeval

        function cd_div_timeval(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))/this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cd_div_timeval

        function cd_expo_timeval(this,other) result(z)
            real(kind=c_double),intent(in)::this
            class(timeval),intent(in)::other
            type(timeval)::z
            real(kind=c_long_double)::t1
            t1=(other%tv_sec+(other%tv_usec/1000000))**this
            z%tv_sec=floor(t1,kind=c_long)
            z%tv_usec=int((t1-floor(t1))*1000000,kind=c_long)
        end function cd_expo_timeval
        !===================================================================
!=========================Time_TM operators=========================================
        function time_tm_addsec_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+other
        end function time_tm_addsec_int
        function time_tm_addsec_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+other
        end function time_tm_addsec_long
        function time_tm_addsec_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+floor(other,kind=c_long)
        end function time_tm_addsec_float
        function time_tm_addsec_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+other
        end function time_tm_addsec_double
        function time_tm_addsec_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+other
        end function time_tm_addsec_longd

        function time_tm_subsec_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-other
        end function time_tm_subsec_int
        function time_tm_subsec_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-other
        end function time_tm_subsec_long
        function time_tm_subsec_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-floor(other,kind=c_long)
        end function time_tm_subsec_float
        function time_tm_subsec_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-other
        end function time_tm_subsec_double
        function time_tm_subsec_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-other
        end function time_tm_subsec_longd
        
        function time_tm_addday_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(86400*other)
        end function time_tm_addday_int
        function time_tm_addday_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(86400*other)
        end function time_tm_addday_long
        function time_tm_addday_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+floor(86400*other,kind=c_long)
        end function time_tm_addday_float
        function time_tm_addday_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(86400*other)
        end function time_tm_addday_double
        function time_tm_addday_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(86400*other)
        end function time_tm_addday_longd

        function time_tm_subday_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(86400*other)
        end function time_tm_subday_int
        function time_tm_subday_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(86400*other)
        end function time_tm_subday_long
        function time_tm_subday_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-floor(86400*other,kind=c_long)
        end function time_tm_subday_float
        function time_tm_subday_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(86400*other)
        end function time_tm_subday_double
        function time_tm_subday_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(86400*other)
        end function time_tm_subday_longd

        function time_tm_addhour_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(3600*other)
        end function time_tm_addhour_int
        function time_tm_addhour_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(3600*other)
        end function time_tm_addhour_long
        function time_tm_addhour_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+floor(3600*other,kind=c_long)
        end function time_tm_addhour_float
        function time_tm_addhour_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(3600*other)
        end function time_tm_addhour_double
        function time_tm_addhour_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(3600*other)
        end function time_tm_addhour_longd

        function time_tm_subhour_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(3600*other)
        end function time_tm_subhour_int
        function time_tm_subhour_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(3600*other)
        end function time_tm_subhour_long
        function time_tm_subhour_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-floor(3600*other,kind=c_long)
        end function time_tm_subhour_float
        function time_tm_subhour_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(3600*other)
        end function time_tm_subhour_double
        function time_tm_subhour_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(3600*other)
        end function time_tm_subhour_longd

        function time_tm_addmin_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(60*other)
        end function time_tm_addmin_int
        function time_tm_addmin_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(60*other)
        end function time_tm_addmin_long
        function time_tm_addmin_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+int(60*other,kind=c_long)
        end function time_tm_addmin_float
        function time_tm_addmin_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(60*other)
        end function time_tm_addmin_double
        function time_tm_addmin_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(60*other)
        end function time_tm_addmin_longd

        function time_tm_submin_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(60*other)
        end function time_tm_submin_int
        function time_tm_submin_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(60*other)
        end function time_tm_submin_long
        function time_tm_submin_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-int(60*other,kind=c_long)
        end function time_tm_submin_float
        function time_tm_submin_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(60*other)
        end function time_tm_submin_double
        function time_tm_submin_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(60*other)
        end function time_tm_submin_longd

        function time_tm_addyear_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !year 31536000 31558149.
            t=this
            t%tm_year=t%tm_year+other
            !t=int(real(mktime(this),kind=c_long_double)+(31556925.19*other),kind=c_long)
        end function time_tm_addyear_int
        function time_tm_addyear_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year+int(other,kind=c_int)
            !t=int(real(mktime(this),kind=c_long_double)+(31556925.19*other),kind=c_long)
        end function time_tm_addyear_long
        function time_tm_addyear_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year+int(other,kind=c_int)
            t=int(real(mktime(t),kind=c_long_double)+(31556925.19*(other-floor(other))),kind=c_long)
        end function time_tm_addyear_float
        function time_tm_addyear_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year+int(other,kind=c_int)
            t=int(real(mktime(t),kind=c_long_double)+(31556925.19*(other-floor(other))),kind=c_long)
        end function time_tm_addyear_double
        function time_tm_addyear_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year+int(other,kind=c_int)
            t=int(real(mktime(t),kind=c_long_double)+(31556925.19*(other-floor(other))),kind=c_long)
            !t=mktime(this)+(31556925.19*other)
        end function time_tm_addyear_longd

        function time_tm_subyear_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year-int(other,kind=c_int)
            !t=int(real(mktime(this),kind=c_long_double)-(31556925.19*other),kind=c_long)
        end function time_tm_subyear_int
        function time_tm_subyear_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year-int(other,kind=c_int)
            !t=int(real(mktime(this),kind=c_long_double)-(31556925.19*other),kind=c_long)
        end function time_tm_subyear_long
        function time_tm_subyear_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year-int(other,kind=c_int)
            t=int(real(mktime(t),kind=c_long_double)-(31556925.19*(other-floor(other))),kind=c_long)
            !t=mktime(this)-floor(31556925.19*other,kind=c_long)
        end function time_tm_subyear_float
        function time_tm_subyear_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year-int(other,kind=c_int)
            t=int(real(mktime(t),kind=c_long_double)-(31556925.19*(other-floor(other))),kind=c_long)
            !t=mktime(this)-(31556925.19*other)
        end function time_tm_subyear_double
        function time_tm_subyear_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=this
            t%tm_year=t%tm_year-int(other,kind=c_int)
            t=int(real(mktime(t),kind=c_long_double)-(31556925.19*(other-floor(other))),kind=c_long)
            !t=mktime(this)-(31556925.19*other)
        end function time_tm_subyear_longd

        function time_tm_addweek_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !week 604800
            t=mktime(this)+(604800*other)
        end function time_tm_addweek_int
        function time_tm_addweek_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(604800*other)
        end function time_tm_addweek_long
        function time_tm_addweek_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+floor(604800*other,kind=c_long)
        end function time_tm_addweek_float
        function time_tm_addweek_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(604800*other)
        end function time_tm_addweek_double
        function time_tm_addweek_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)+(604800*other)
        end function time_tm_addweek_longd

        function time_tm_subweek_int(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_int),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(604800*other)
        end function time_tm_subweek_int
        function time_tm_subweek_long(this,other) result(t)
            class(time_tm),intent(in)::this
            integer(kind=c_long),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(604800*other)
        end function time_tm_subweek_long
        function time_tm_subweek_float(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_float),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-floor(604800*other,kind=c_long)
        end function time_tm_subweek_float
        function time_tm_subweek_double(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(604800*other)
        end function time_tm_subweek_double
        function time_tm_subweek_longd(this,other) result(t)
            class(time_tm),intent(in)::this
            real(kind=c_long_double),intent(in)::other
            type(time_tm)::t !day 86400
            t=mktime(this)-(604800*other)
        end function time_tm_subweek_longd

        
end module timez
