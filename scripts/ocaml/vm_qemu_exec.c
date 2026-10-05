#define _GNU_SOURCE
#include <dlfcn.h>
#include <spawn.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <sys/stat.h>
static const char *mapped(const char *path) {
  static __thread char replacement[PATH_MAX];
  const char *prefix = "/opt/dcb/qemu/";
  const char *directory = getenv("PORT_QEMU_DIRECTORY");
  if (path && directory && strncmp(path,prefix,strlen(prefix)) == 0) {
    int length=snprintf(replacement,sizeof replacement,"%s/%s",directory,path+strlen(prefix));
    if (length>0 && length<(int)sizeof replacement) return replacement;
  }
  return path;
}
int execve(const char *path,char *const argv[],char *const envp[]) {
  int (*real)(const char*,char *const[],char *const[])=dlsym(RTLD_NEXT,"execve");
  return real(mapped(path),argv,envp);
}
int execv(const char *path,char *const argv[]) {
  int (*real)(const char*,char *const[])=dlsym(RTLD_NEXT,"execv");
  return real(mapped(path),argv);
}
int execvp(const char *path,char *const argv[]) {
  int (*real)(const char*,char *const[])=dlsym(RTLD_NEXT,"execvp");
  return real(mapped(path),argv);
}
int posix_spawn(pid_t *pid,const char *path,const posix_spawn_file_actions_t *actions,const posix_spawnattr_t *attributes,char *const argv[],char *const envp[]) {
  int (*real)(pid_t*,const char*,const posix_spawn_file_actions_t*,const posix_spawnattr_t*,char *const[],char *const[])=dlsym(RTLD_NEXT,"posix_spawn");
  return real(pid,mapped(path),actions,attributes,argv,envp);
}
int posix_spawnp(pid_t *pid,const char *path,const posix_spawn_file_actions_t *actions,const posix_spawnattr_t *attributes,char *const argv[],char *const envp[]) {
  int (*real)(pid_t*,const char*,const posix_spawn_file_actions_t*,const posix_spawnattr_t*,char *const[],char *const[])=dlsym(RTLD_NEXT,"posix_spawnp");
  return real(pid,mapped(path),actions,attributes,argv,envp);
}
/* The E2E runner checks its pinned QEMU path before starting a process. */
int stat(const char *path,struct stat *buffer) {
  int (*real)(const char*,struct stat*)=dlsym(RTLD_NEXT,"stat");
  return real(mapped(path),buffer);
}
int lstat(const char *path,struct stat *buffer) {
  int (*real)(const char*,struct stat*)=dlsym(RTLD_NEXT,"lstat");
  return real(mapped(path),buffer);
}
int stat64(const char *path,struct stat64 *buffer) {
  int (*real)(const char*,struct stat64*)=dlsym(RTLD_NEXT,"stat64");
  return real(mapped(path),buffer);
}
int lstat64(const char *path,struct stat64 *buffer) {
  int (*real)(const char*,struct stat64*)=dlsym(RTLD_NEXT,"lstat64");
  return real(mapped(path),buffer);
}
int __xstat64(int version,const char *path,struct stat64 *buffer) {
  int (*real)(int,const char*,struct stat64*)=dlsym(RTLD_NEXT,"__xstat64");
  return real(version,mapped(path),buffer);
}
int __lxstat64(int version,const char *path,struct stat64 *buffer) {
  int (*real)(int,const char*,struct stat64*)=dlsym(RTLD_NEXT,"__lxstat64");
  return real(version,mapped(path),buffer);
}
int access(const char *path,int mode) {
  int (*real)(const char*,int)=dlsym(RTLD_NEXT,"access");
  return real(mapped(path),mode);
}
void *dlopen(const char *path,int flags) {
  void *(*real)(const char*,int)=dlsym(RTLD_NEXT,"dlopen");
  return real(mapped(path),flags);
}
