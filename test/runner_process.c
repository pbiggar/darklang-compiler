/* Keep fixture children in a process group so timeout cleanup reaches descendants. */
#define _GNU_SOURCE
#include <spawn.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
CAMLprim value dark_runner_spawn(value request) {
  CAMLparam1(request);
  CAMLlocal2(result,message);
  value arguments=Field(request,1), environment=Field(request,2);
  mlsize_t argc=Wosize_val(arguments), envc=Wosize_val(environment);
  char **argv=malloc((argc+1)*sizeof(char*));
  char **envp=malloc((envc+1)*sizeof(char*));
  if (!argv || !envp) { free(argv);free(envp);caml_raise_out_of_memory(); }
  for (mlsize_t i=0;i<argc;i++) argv[i]=(char*)String_val(Field(arguments,i));
  for (mlsize_t i=0;i<envc;i++) envp[i]=(char*)String_val(Field(environment,i));
  argv[argc]=NULL;envp[envc]=NULL;
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attr;
  int error=posix_spawn_file_actions_init(&actions);
  int has_actions=(error==0),has_attr=0;
  if (!error) { error=posix_spawnattr_init(&attr);has_attr=(error==0); }
  if (!error) error=posix_spawnattr_setflags(&attr,POSIX_SPAWN_SETPGROUP);
  if (!error) error=posix_spawnattr_setpgroup(&attr,0);
  if (!error) error=posix_spawn_file_actions_adddup2(&actions,Int_val(Field(request,3)),0);
  if (!error) error=posix_spawn_file_actions_adddup2(&actions,Int_val(Field(request,4)),1);
  if (!error) error=posix_spawn_file_actions_adddup2(&actions,Int_val(Field(request,5)),2);
  pid_t pid=-1;
  if (!error) error=posix_spawnp(&pid,String_val(Field(request,0)),&actions,&attr,argv,envp);
  if (has_actions) posix_spawn_file_actions_destroy(&actions);
  if (has_attr) posix_spawnattr_destroy(&attr);
  free(argv);free(envp);
  message=caml_copy_string(error ? strerror(error) : "");
  result=caml_alloc_tuple(2);
  Store_field(result,0,Val_int(error ? -1 : pid));Store_field(result,1,message);
  CAMLreturn(result);
}

/* .NET invariant casing applies simple ICU casing and preserves dotless i. */
#include <unicode/uchar.h>
CAMLprim value dark_runner_upper_scalar(value scalar) {
  int code=Int_val(scalar);return Val_int(code==0x131 ? code : u_toupper(code));
}
