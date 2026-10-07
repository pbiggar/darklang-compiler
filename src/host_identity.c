/* host_identity.c - Read host OS/architecture without a shell subprocess. */
#define CAML_NAME_SPACE
#include <sys/utsname.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

CAMLprim value dark_compiler_host_identity(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(result, os, arch);
  struct utsname host;
  if (uname(&host) != 0) caml_failwith("Unable to detect host platform");
  os = caml_copy_string(host.sysname);
  arch = caml_copy_string(host.machine);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, os);
  Store_field(result, 1, arch);
  CAMLreturn(result);
}
