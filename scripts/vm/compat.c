/* Adapt stack discovery and temporary paths to the restricted OCaml port VM. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/resource.h>
/* glibc requires procfs to describe the initial thread's stack. Discover its
actually mapped pages instead; keep native attributes for other threads. */
int pthread_getattr_np(pthread_t t,pthread_attr_t*a){
    static int(*real)(pthread_t,pthread_attr_t*);
    if(!real)real=dlsym(RTLD_NEXT,"pthread_getattr_np");
    int r=real(t,a);
    if(!r||!pthread_equal(t,pthread_self()))return r;
    long page=sysconf(_SC_PAGESIZE);
    uintptr_t here=(uintptr_t)&r&~((uintptr_t)page-1),lo=here,hi=here+page;
    unsigned char v;
    size_t cap=64*1024*1024;
    while(here-lo<cap&&mincore((void*)(lo-page),page,&v)==0)lo-=page;
    while(hi-here<cap&&mincore((void*)hi,page,&v)==0)hi+=page;
    struct rlimit limit;
    if(getrlimit(RLIMIT_STACK,&limit)==0&&limit.rlim_cur!=RLIM_INFINITY&&limit.rlim_cur>hi-lo)lo=hi-limit.rlim_cur;
    r=pthread_attr_init(a);
    if(r)return r;
    return pthread_attr_setstack(a,(void*)lo,hi-lo);
}
#include <fcntl.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <limits.h>
static const char*temp_path(const char*p,char*b){
    if(p&&strncmp(p,"/tmp",4)==0&&(p[4]=='/'||p[4]==0)){
        const char*root=getenv("PORT_VM_TMPDIR");
        if(!root)return p;
        snprintf(b,PATH_MAX,"%s%s",root,p+4);
        return b;
    }
    return p;
}
int open(const char*p,int f,...){
    static int(*real)(const char*,int,...);
    if(!real)real=dlsym(RTLD_NEXT,"open");
    va_list v;
    va_start(v,f);
    int m=(f&O_CREAT)?va_arg(v,int):0;
    va_end(v);
    char b[PATH_MAX];
    return real(temp_path(p,b),f,m);
}
int open64(const char*p,int f,...){
    static int(*real)(const char*,int,...);
    if(!real)real=dlsym(RTLD_NEXT,"open64");
    va_list v;
    va_start(v,f);
    int m=(f&O_CREAT)?va_arg(v,int):0;
    va_end(v);
    char b[PATH_MAX];
    return real(temp_path(p,b),f,m);
}
int mkdir(const char*p,mode_t m){
    static int(*real)(const char*,mode_t);
    if(!real)real=dlsym(RTLD_NEXT,"mkdir");
    char b[PATH_MAX];
    return real(temp_path(p,b),m);
}
int stat(const char*p,struct stat*s){
    static int(*real)(const char*,struct stat*);
    if(!real)real=dlsym(RTLD_NEXT,"stat");
    char b[PATH_MAX];
    return real(temp_path(p,b),s);
}
int stat64(const char*p,struct stat64*s){
    static int(*real)(const char*,struct stat64*);
    if(!real)real=dlsym(RTLD_NEXT,"stat64");
    char b[PATH_MAX];
    return real(temp_path(p,b),s);
}
int lstat64(const char*p,struct stat64*s){
    static int(*real)(const char*,struct stat64*);
    if(!real)real=dlsym(RTLD_NEXT,"lstat64");
    char b[PATH_MAX];
    return real(temp_path(p,b),s);
}
int unlink(const char*p){
    static int(*real)(const char*);
    if(!real)real=dlsym(RTLD_NEXT,"unlink");
    char b[PATH_MAX];
    return real(temp_path(p,b));
}
int access(const char*p,int m){
    static int(*real)(const char*,int);
    if(!real)real=dlsym(RTLD_NEXT,"access");
    char b[PATH_MAX];
    return real(temp_path(p,b),m);
}
int statx(int d,const char*p,int f,unsigned mask,struct statx*s){
    static int(*real)(int,const char*,int,unsigned,struct statx*);
    if(!real)real=dlsym(RTLD_NEXT,"statx");
    char b[PATH_MAX];
    return real(d,temp_path(p,b),f,mask,s);
}
int fstatat64(int d,const char*p,struct stat64*s,int f){
    static int(*real)(int,const char*,struct stat64*,int);
    if(!real)real=dlsym(RTLD_NEXT,"fstatat64");
    char b[PATH_MAX];
    return real(d,temp_path(p,b),s,f);
}
int openat(int d,const char*p,int f,...){
    static int(*real)(int,const char*,int,...);
    if(!real)real=dlsym(RTLD_NEXT,"openat");
    va_list v;
    va_start(v,f);
    int m=(f&O_CREAT)?va_arg(v,int):0;
    va_end(v);
    char b[PATH_MAX];
    return real(d,temp_path(p,b),f,m);
}
int openat64(int d,const char*p,int f,...){
    static int(*real)(int,const char*,int,...);
    if(!real)real=dlsym(RTLD_NEXT,"openat64");
    va_list v;
    va_start(v,f);
    int m=(f&O_CREAT)?va_arg(v,int):0;
    va_end(v);
    char b[PATH_MAX];
    return real(d,temp_path(p,b),f,m);
}
int __xstat64(int v,const char*p,struct stat64*s){
    static int(*real)(int,const char*,struct stat64*);
    if(!real)real=dlsym(RTLD_NEXT,"__xstat64");
    char b[PATH_MAX];
    return real(v,temp_path(p,b),s);
}
int __lxstat64(int v,const char*p,struct stat64*s){
    static int(*real)(int,const char*,struct stat64*);
    if(!real)real=dlsym(RTLD_NEXT,"__lxstat64");
    char b[PATH_MAX];
    return real(v,temp_path(p,b),s);
}
int chmod(const char*p,mode_t m){
    static int(*real)(const char*,mode_t);
    if(!real)real=dlsym(RTLD_NEXT,"chmod");
    char b[PATH_MAX];
    return real(temp_path(p,b),m);
}
int rename(const char*p,const char*q){
    static int(*real)(const char*,const char*);
    if(!real)real=dlsym(RTLD_NEXT,"rename");
    char b[PATH_MAX],c[PATH_MAX];
    return real(temp_path(p,b),temp_path(q,c));
}
#include <dirent.h>
DIR*opendir(const char*p){
    static DIR*(*real)(const char*);
    if(!real)real=dlsym(RTLD_NEXT,"opendir");
    char b[PATH_MAX];
    return real(temp_path(p,b));
}
