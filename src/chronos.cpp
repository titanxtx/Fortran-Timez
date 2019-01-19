/*MIT License

Copyright (c) 2019 Joshua Oliva

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*/
#include <chrono>
#include <thread>
#include <time.h>
#if defined(__linux__) && defined(HAVE_SYS_TIME_H)
    #include <sys/time.h>
#endif
#if defined(__linux__) && defined(HAVE_UNISTD_H)
    #include <unistd.h>
#endif
extern "C"{
    int gettimeofday_check(struct timeval *x, struct timezone *y)
    {
        #if defined(__linux__) && defined(HAVE_SYS_TIME_H)
            return gettimeofday(x,y);
        #else
        auto stx=std::chrono::high_resolution_clock::now().time_since_epoch();
        x->tv_sec=long(std::chrono::duration_cast<std::chrono::seconds>(stx).count());
        x->tv_usec=long(std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::duration<long double>(stx)-std::chrono::duration_cast<std::chrono::seconds>(stx)).count());
        return 1;
        #endif
    }

    int clock_gettime_check(clockid_t x,struct timespec *y)
    {
        #if defined(__linux__) && defined(HAVE_SYS_TIME_H)
            return clock_gettime(x,y);
        #else
        auto stx=std::chrono::high_resolution_clock::now().time_since_epoch();
        y->tv_sec=long(std::chrono::duration_cast<std::chrono::seconds>(stx).count());
        y->tv_nsec=long(std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::duration<long double>(stx)-std::chrono::duration_cast<std::chrono::seconds>(stx)).count());
        return 1;
        #endif
    }
    
    int sleep_check(unsigned int usec){
        #if defined(__linux__) && defined(HAVE_UNISTD_H)
        return sleep(usec);
        #else
        std::this_thread::sleep_for(std::chrono::seconds(usec));
        return 1;
        #endif
    }

    int usleep_check(unsigned int usec){
        #if defined(__linux__) && defined(HAVE_UNISTD_H)
        return usleep(usec);
        #else
        std::this_thread::sleep_for(std::chrono::microseconds(usec));
        return 1;
        #endif
    }

    int nanosleep_check(struct timespec *req,struct timespec *rem){
        #ifdef __linux__
            return nanosleep(req,rem);
        #else
            std::this_thread::sleep_for(std::chrono::nanoseconds((req->tv_sec*1000000000)+req->tv_nsec));
            return 1;
        #endif
    }
}