#define _GNU_SOURCE
#include <dlfcn.h>
#include <spawn.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
static const char *mapped(const char *path) {
  static __thread char replacement[PATH_MAX];
  const char *prefix = "/opt/dcb/qemu/";
  const char *directory = getenv("PORT_QEMU_DIRECTORY");
  if (directory && strncmp(path,prefix,strlen(prefix)) == 0) {
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
