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

/* Fresh inference identities use the same OS entropy source as Unix Guid.NewGuid. */
#include <errno.h>
#if defined(__APPLE__)
#include <unistd.h>
#else
#include <sys/random.h>
#endif
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
CAMLprim value dark_random_uuid_bytes(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(bytes);
  unsigned char raw[16];
  #if defined(__APPLE__)
  if (getentropy(raw, sizeof(raw)) != 0) caml_failwith("Unable to obtain UUID entropy");
  #else
  size_t offset = 0;
  while (offset < sizeof(raw)) {
    ssize_t count = getrandom(raw + offset, sizeof(raw) - offset, 0);
    if (count < 0 && errno == EINTR) continue;
    if (count <= 0) caml_failwith("Unable to obtain UUID entropy");
    offset += (size_t)count;
  }
  #endif
  bytes = caml_alloc_initialized_string(sizeof(raw), (const char *)raw);
  CAMLreturn(bytes);
}
